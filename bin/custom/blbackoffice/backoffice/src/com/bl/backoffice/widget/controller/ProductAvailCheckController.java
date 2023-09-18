package com.bl.backoffice.widget.controller;
import com.bl.backoffice.wizards.util.ProductAvailCheckToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.*;
import javax.annotation.Resource;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;
public class ProductAvailCheckController extends DefaultWidgetController {
    private static final Logger LOG = Logger.getLogger(ProductAvailCheckController.class);
    protected static final String OUT_CONFIRM = "confirmOutput";
    protected static final String COMPLETE = "completed";
    private static final int BUFFER_SIZE = 4096;
    protected static final String IN_SOCKET = "nodeSelected";
    public static final String PRODUCT_AVAIL_CHECK = "productAvailCheck";
    private Textbox productCode;
    private Datebox rentalStartDate;
    private Datebox rentalEndDate;
    private Grid productAvailCheckToolDataGrid;
    @Wire
    private Div productAvailCheckToolDataHeader;
    @Resource(name = "blCommerceStockService")
    private BlCommerceStockService blCommerceStockService;
    @Resource(name = "baseStoreService")
    private BaseStoreService baseStoreService ;
    @Resource(name = "productDao")
    private BlProductDao productDao;
    @SocketEvent(socketId = IN_SOCKET)
    public void initLoadPage(final Object productCode) {
        this.getWidgetInstanceManager()
                .setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.product.avail.check.heading")));
        this.productAvailCheckToolDataHeader.setStyle("resize:none;display:none");
    }
    @ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void cancel() {
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }
    @ViewEvent(componentID = PRODUCT_AVAIL_CHECK, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void getAvailableSerialData()
    {
        if (StringUtils.isBlank(productCode.getText()))
        {
            this.productAvailCheckToolDataHeader.setStyle("resize:none;display:none");
            Messagebox.show("Please Enter Product Code");
        }
        else{
            final List<String> productCodeList = Arrays.asList(productCode.getText());
            final String selectedFromDate = rentalStartDate.getText();
            final String selectedToDate = rentalEndDate.getText();
            Set<String> productCodeSets = new HashSet<String>(productCodeList);
            String saparaterString = "    :";
            final BaseStoreModel baseStore = baseStoreService.getBaseStoreForUid("bl");
            final Collection<WarehouseModel> warehouseModels = baseStore.getWarehouses().stream().filter(warehouse -> warehouse.isActive()).collect(Collectors.toList());
            LocalDate localStartDate = BlDateTimeUtils.convertStringDateToLocalDate(selectedFromDate, BlCoreConstants.SQL_DATE_FORMAT);
            LocalDate localEndDate = BlDateTimeUtils.convertStringDateToLocalDate(selectedToDate, BlCoreConstants.SQL_DATE_FORMAT);
            Date startDate = Date.from(localStartDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
            Date endDate = Date.from(localEndDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
            final Collection<StockLevelModel> finalStockLevels = new ArrayList<>();
            for (WarehouseModel warehouse : warehouseModels) {
                final Collection<StockLevelModel> stockLevels = blCommerceStockService.getStockForProductCodesAndDate(productCodeSets, warehouse, startDate, endDate);
                if (CollectionUtils.isNotEmpty(stockLevels)) {
                    finalStockLevels.addAll(stockLevels);
                }
            }
            List<ProductAvailCheckToolData> productAvailCheckToolData = new ArrayList<>();
            if (CollectionUtils.isNotEmpty(finalStockLevels)) {
                final Map<String, List<StockLevelModel>> stockLevelsSerialWise = finalStockLevels.stream()
                        .collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));
                Collection<BlSerialProductModel> blSerialProducts = productDao.getBlSerialProductsForCodes(stockLevelsSerialWise.keySet());
//    System.out.println("Size of all serial product which come from stock table:" + blSerialProducts.size());
                final List<BlSerialProductModel> nonBufferProducts = blSerialProducts.stream()
                        .filter(serial -> BooleanUtils.isFalse(serial.getIsBufferedInventory()))
                        .collect(Collectors.toList());
//    System.out.println("After removed buffer product i.e non buffer product size:" + nonBufferProducts.size());
                final List<BlSerialProductModel> consignerSerial = nonBufferProducts.stream().filter(serial -> "BL".equalsIgnoreCase(serial.getOwnedBy()))
                        .collect(Collectors.toList());
//    System.out.println("After filter consigner serial i.e. owned by BL serial size:" + consignerSerial.size())
                final Map<BlProductModel,
                        List<BlSerialProductModel>> availableSerialMap = consignerSerial.stream().collect(Collectors.groupingBy(BlSerialProductModel::getBlProduct));
                availableSerialMap.values().forEach(serialProductList -> {
                    serialProductList.forEach(serialProduct -> {
                        ProductAvailCheckToolData productAvailCheckToolData1 = new ProductAvailCheckToolData();
                        productAvailCheckToolData1.setProductCode(serialProduct.getBlProduct().getCode());
                        productAvailCheckToolData1.setArticleNumber(serialProduct.getCode());
                        productAvailCheckToolData1.setBarcode(serialProduct.getBarcode());
                        productAvailCheckToolData1.setSerialStatus(serialProduct.getSerialStatus().getCode());
                        productAvailCheckToolData1.setWarehouseLocation(serialProduct.getWarehouseLocation().getName());
                        productAvailCheckToolData1.setOcLocation(serialProduct.getOcLocation());
                        productAvailCheckToolData1.setLastLocationScanParent(serialProduct.getLastLocationScanParent());
                        productAvailCheckToolData.add(productAvailCheckToolData1);
                    });
                });
                this.productAvailCheckToolDataHeader.setStyle("resize:none;display:block");
                this.getProductAvailCheckToolDataGrid().setModel(new ListModelList<>(productAvailCheckToolData));
                this.getProductAvailCheckToolDataGrid().renderAll();
            }
            else {
                Messagebox.show("No available stock found for product for given duration"+productCode.getText());
                this.productAvailCheckToolDataHeader.setStyle("resize:none;display:block");
                this.getProductAvailCheckToolDataGrid().setModel(new ListModelList<>(productAvailCheckToolData));
                this.getProductAvailCheckToolDataGrid().renderAll();
            }
        }
    }
    public Grid getProductAvailCheckToolDataGrid() {
        return productAvailCheckToolDataGrid;
    }
    public void setProductAvailCheckToolDataGrid(Grid productAvailCheckToolDataGrid) {
        this.productAvailCheckToolDataGrid = productAvailCheckToolDataGrid;
    }
}