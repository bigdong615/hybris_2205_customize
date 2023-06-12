import com.bl.core.constants.BlCoreConstants
import com.bl.core.model.BlSerialProductModel
import com.bl.core.product.dao.BlProductDao
import com.bl.core.stock.BlCommerceStockService
import com.bl.core.utils.BlDateTimeUtils
import de.hybris.platform.ordersplitting.model.StockLevelModel
import de.hybris.platform.ordersplitting.model.WarehouseModel
import de.hybris.platform.store.BaseStoreModel
import de.hybris.platform.store.services.BaseStoreService
import org.apache.commons.collections.CollectionUtils

import java.time.LocalDate
import java.time.ZoneId
import java.util.stream.Collectors

final List<String> productCodeList = Arrays.asList("canon-16-35mm-iii-lens","canon-50mm-f1.2-l");
final String selectedFromDate = "06-27-2023";
final String selectedToDate = "07-03-2023";
Set<String> productCodeSets = new HashSet<String>(productCodeList);
String saparaterString = "    :";

BlCommerceStockService blCommerceStockService = spring.getBean("blCommerceStockService");
BaseStoreService baseStoreService = spring.getBean("baseStoreService");
BlProductDao blProductDao = spring.getBean("productDao");

final BaseStoreModel baseStore = baseStoreService.getBaseStoreForUid("bl");
final Collection<WarehouseModel> warehouseModels = baseStore.getWarehouses().stream().filter(warehouse -> warehouse.isActive()).collect(Collectors.toList());

LocalDate localStartDate = BlDateTimeUtils.convertStringDateToLocalDate(selectedFromDate, BlCoreConstants.SQL_DATE_FORMAT);
LocalDate localEndDate = BlDateTimeUtils.convertStringDateToLocalDate(selectedToDate, BlCoreConstants.SQL_DATE_FORMAT);
Date startDate = Date.from(localStartDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
Date endDate = Date.from(localEndDate.atStartOfDay(ZoneId.systemDefault()).toInstant())

final Collection<StockLevelModel> finalStockLevels = new ArrayList<>();
for (WarehouseModel warehouse : warehouseModels) {
    final Collection<StockLevelModel> stockLevels = blCommerceStockService.getStockForProductCodesAndDate(productCodeSets, warehouse, startDate, endDate);
    if (CollectionUtils.isNotEmpty(stockLevels)) {
        finalStockLevels.addAll(stockLevels)
    }
}
if (CollectionUtils.isNotEmpty(finalStockLevels)) {
    final Map<String, List<StockLevelModel>> stockLevelsSerialWise = finalStockLevels.stream()
            .collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));
    Collection<BlSerialProductModel> blSerialProducts = blProductDao.getBlSerialProductsForCodes(stockLevelsSerialWise.keySet());
//    System.out.println("Size of all serial product which come from stock table:" + blSerialProducts.size());

    final List<BlSerialProductModel> consignerSerial = blSerialProducts.stream().filter(serial -> "BL".equalsIgnoreCase(serial.getOwnedBy()))
            .collect(Collectors.toList())
    System.out.println("All the Consigner serial i.e. owned by BL serial:")
    final Map<String, List<StockLevelModel>> availableSerialMap = consignerSerial.stream().collect(Collectors.groupingBy(BlSerialProductModel::getBlProduct));
    System.out.println("Product Code" + saparaterString + "Article Number" + saparaterString + "Barcode" + saparaterString + "SerialStatus" + saparaterString + "Warehouse Location" + saparaterString + "OC Location" + saparaterString + "Last Location Scan Parent" + saparaterString + "IsBufferInventory");
    availableSerialMap.values().forEach(serialProductList -> {
        final Map<String, List<StockLevelModel>> warehouseBasedAvailableSerialMap = serialProductList.stream().collect(Collectors.groupingBy(BlSerialProductModel::getWarehouseLocation));
        warehouseBasedAvailableSerialMap.values().forEach(serialProducts -> {
            serialProducts.forEach(serialProduct -> {
                System.out.println(serialProduct.getBlProduct().getCode() + saparaterString + serialProduct.getCode() + saparaterString + serialProduct.getBarcode() + saparaterString + serialProduct.getSerialStatus().getCode() + saparaterString + serialProduct.getWarehouseLocation().getName() + saparaterString + serialProduct.getOcLocation() + saparaterString + serialProduct.getLastLocationScanParent() + saparaterString + serialProduct.getIsBufferedInventory());
            })
        });
    });


    final List<BlSerialProductModel> nonConsignerSerial = blSerialProducts.stream().filter(serial -> !"BL".equalsIgnoreCase(serial.getOwnedBy())).collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(nonConsignerSerial)) {
        System.out.println("\nAll the non BL serial:")
    final Map<String, List<StockLevelModel>> nonBlAvailableSerialMap = nonConsignerSerial.stream().collect(Collectors.groupingBy(BlSerialProductModel::getBlProduct));
    System.out.println("Product Code" + saparaterString + "Article Number" + saparaterString + "Barcode" + saparaterString + "SerialStatus" + saparaterString + "Warehouse Location" + saparaterString + "OC Location" + saparaterString + "Last Location Scan Parent" + saparaterString + "IsBufferInventory" + saparaterString + "For Sale");
    nonBlAvailableSerialMap.values().forEach(serialProductList -> {
        final Map<String, List<StockLevelModel>> warehouseBasedAvailableSerialMap = serialProductList.stream().collect(Collectors.groupingBy(BlSerialProductModel::getWarehouseLocation));
        warehouseBasedAvailableSerialMap.values().forEach(serialProducts -> {
            serialProducts.forEach(serialProduct -> {
                System.out.println(serialProduct.getBlProduct().getCode() + saparaterString + serialProduct.getCode() + saparaterString + serialProduct.getBarcode() + saparaterString + serialProduct.getSerialStatus().getCode() + saparaterString + serialProduct.getWarehouseLocation().getName() + saparaterString + serialProduct.getOcLocation() + saparaterString + serialProduct.getLastLocationScanParent() + saparaterString + serialProduct.getIsBufferedInventory() + saparaterString + serialProduct.getForSale());
            })
        });
    });
    }
} else {
    System.out.println("No available stock found for given product for given duration");
}