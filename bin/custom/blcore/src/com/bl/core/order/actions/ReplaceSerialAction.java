package com.bl.core.order.actions;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ConsignmentEntryStatusEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.ReallocateSerialProcessModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.task.RetryLaterException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class ReplaceSerialAction extends AbstractSimpleDecisionAction<ReallocateSerialProcessModel> {

  private static final Logger LOG = Logger.getLogger(ReplaceSerialAction.class);
  private ModelService modelService;

  @Resource(name = "productDao")
  private BlProductDao productDao;
  @Resource(name="orderDao")// need to remove this line
  private BlOrderDao orderDao;
  @Resource(name="blCommerceStockService")
  private BlCommerceStockService blCommerceStockService;

  @Override
  public Transition executeAction(ReallocateSerialProcessModel serialProcessModel)
      throws RetryLaterException, Exception {
    updateSerialOnOrder(serialProcessModel.getAssociatedOrderStocks(),serialProcessModel.getOldSerialProduct(),serialProcessModel.getReservedStatus());

    return Transition.OK;
  }

 /* @Override
  public void executeAction(BusinessProcessModel businessProcessModel)
      throws RetryLaterException, Exception {
    ReallocateSerialProcessModel serialProcessModel = (ReallocateSerialProcessModel) businessProcessModel;
    updateSerialOnOrder(serialProcessModel.getAssociatedOrderStocks(),serialProcessModel.getOldSerialProduct(),serialProcessModel.getReservedStatus());
  }*/


  private void updateSerialOnOrder(final List<StockLevelModel> associatedOrderStocks,final BlSerialProductModel blSerialProduct, final boolean reservedStatus){

    final Map<String, List<StockLevelModel>> stockLevelsOrderWise = associatedOrderStocks.stream()
        .collect(Collectors.groupingBy(StockLevelModel::getOrder));
    for(Map.Entry<String,List<StockLevelModel>> orderCodeEntry :stockLevelsOrderWise.entrySet()){
      String orderCode = orderCodeEntry.getKey();
      AtomicReference<Boolean> isSerialUpdated = new AtomicReference<>(false);
      final Map<String, List<StockLevelModel>> stockLevelsProductWise = orderCodeEntry.getValue().stream().collect(Collectors.groupingBy(StockLevelModel::getProductCode));
      for (Map.Entry<String,List<StockLevelModel>> productCodeEntry : stockLevelsProductWise.entrySet()){
        Set<String> productCode = new HashSet<>();
        productCode.add(productCodeEntry.getKey());
        Set<String> oldSerialProductCode = productCodeEntry.getValue().stream()
            .collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode)).keySet();
        final AbstractOrderModel order = getOrderDao().getOrderByCode(orderCode);

        filterOrderEntryAndAssignSerial(order,oldSerialProductCode,isSerialUpdated,productCode);

      }
      if(isSerialUpdated.get()){
        List<StockLevelModel> stockLevelModelList = orderCodeEntry.getValue();
        stockLevelModelList.forEach(stockLevel -> {
          stockLevel.setSerialStatus(blSerialProduct.getSerialStatus());
          stockLevel.setOrder(null);
          saveStockRecord(stockLevel, reservedStatus);
        });
        break;
      }
    }

  }


  public void filterOrderEntryAndAssignSerial(AbstractOrderModel order ,Set<String> oldSerialProductCode, AtomicReference<Boolean> isSerialUpdated,Set<String> productCode){
    order.getConsignments().forEach(consignmentModel -> {
      consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
        consignmentEntryModel.getSerialProducts().forEach(blProductModel -> {
          if(blProductModel instanceof  BlSerialProductModel ){
            BlSerialProductModel oldSerialProduct = (BlSerialProductModel)blProductModel;
            if(oldSerialProduct.getCode().equals(oldSerialProductCode.toString())){
              isSerialUpdated.set( findStockAndAssignSerial(productCode,consignmentModel,consignmentEntryModel,oldSerialProduct,isSerialUpdated));
            }
          }
        });
      });
    });
  }


  private Boolean findStockAndAssignSerial(Set<String> productCode, ConsignmentModel consignmentModel,
      ConsignmentEntryModel consignmentEntryModel,BlSerialProductModel oldSerialProduct,AtomicReference<Boolean> isSerialUpdated){
    final Collection<StockLevelModel> stockLevels = getBlCommerceStockService().getStockForProductCodesAndDate(productCode, consignmentModel.getWarehouse(), consignmentModel.getOptimizedShippingStartDate(), consignmentModel.getOptimizedShippingEndDate());

    if (CollectionUtils.isNotEmpty(stockLevels)) {
      final Map<String, List<StockLevelModel>> stockLevelsSerialWise = stockLevels
          .stream()
          .collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));
      Collection<BlSerialProductModel> blSerialProducts = productDao
          .getBlSerialProductsForCodes(stockLevelsSerialWise.keySet());

      final List<BlSerialProductModel> nonBufferProducts = blSerialProducts.stream()
          .filter(serial -> BooleanUtils.isFalse(serial.getIsBufferedInventory()))
          .collect(Collectors.toList());
      blSerialProducts
          .remove(nonBufferProducts); // now it was only contain buffer product
      final List<BlSerialProductModel> consignerSerial = nonBufferProducts.stream()
          .filter(serial -> "BL".equalsIgnoreCase(serial.getOwnedBy()))
          .collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(consignerSerial)) {
        return assignSerial(consignerSerial,consignmentEntryModel,oldSerialProduct);
      }else {

      }

    }
    return false;
  }

  public Boolean assignSerial(List<BlSerialProductModel> availableSerials,ConsignmentEntryModel consignmentEntryModel,BlSerialProductModel oldSerialProduct){
    BlSerialProductModel newSerialProduct = availableSerials.get(0);
    consignmentEntryModel.getSerialProducts().add(newSerialProduct);

    Map<String, ItemStatusEnum> items = consignmentEntryModel.getItems();
    items.put(newSerialProduct.getCode(),items.get(oldSerialProduct.getCode()));


    Map<String, ConsignmentEntryStatusEnum> consignmentEntryStatus = consignmentEntryModel
        .getConsignmentEntryStatus();
    consignmentEntryStatus.put(newSerialProduct.getCode(),consignmentEntryStatus.get(oldSerialProduct.getCode()));

    consignmentEntryModel.getSerialProducts().remove(oldSerialProduct);
    items.remove(oldSerialProduct.getCode());
    consignmentEntryStatus.remove(oldSerialProduct.getCode());
    modelService.save(consignmentEntryModel);
    modelService.refresh(consignmentEntryModel);
    return true;
  }

  /**
   * It saves the stock record after updates
   * @param stockLevel
   * @param reservedStatus
   */
  private void saveStockRecord(final StockLevelModel stockLevel, final boolean reservedStatus)
  {
    stockLevel.setReservedStatus(reservedStatus);
    try {
      getModelService().save(stockLevel);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock {} updated for serial product {} for the date {} ",
          stockLevel.getPk(), stockLevel.getSerialProductCode(), stockLevel.getDate());
    }
    catch(final ModelSavingException ex) {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
          "Exception occurred while saving the stock record {} of the serial product {} for the date {} ",
          stockLevel.getPk(), stockLevel.getSerialProductCode(), stockLevel.getDate());
    }
  }

  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(BlOrderDao orderDao) {
    this.orderDao = orderDao;
  }

  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }


}
