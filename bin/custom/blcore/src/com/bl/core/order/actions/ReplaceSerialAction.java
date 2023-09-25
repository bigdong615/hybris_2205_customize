package com.bl.core.order.actions;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ConsignmentEntryStatusEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.ReallocateSerialProcessModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.task.RetryLaterException;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class ReplaceSerialAction extends AbstractSimpleDecisionAction<ReallocateSerialProcessModel> {

  private static final Logger LOG = Logger.getLogger(ReplaceSerialAction.class);

  private BlProductDao productDao;
  private BlOrderDao orderDao;
  private BlCommerceStockService blCommerceStockService;
  private BlStockLevelDao blStockLevelDao;
  private SearchRestrictionService searchRestrictionService;
  private SessionService sessionService;
  @Resource(name = "defaultBlUserService")
  private DefaultBlUserService defaultBlUserService;

  @Override
  public Transition executeAction(ReallocateSerialProcessModel serialProcessModel)
      throws RetryLaterException, Exception {

    final Date currentDate = Date
        .from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
    final Date futureDate = BlDateTimeUtils.getNextYearsSameDay();
    String serialCode = serialProcessModel.getOldSerialProduct().getCode();
    final Collection<StockLevelModel> stockLevels = getBlStockLevelDao()
        .findSerialStockLevelForDate(serialCode,
            currentDate, futureDate);
  BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Stock size {} for serial product {} for the rental duration {} to {} while replacement of serial{}",stockLevels.size(),serialCode,currentDate,futureDate,serialCode);
    List<StockLevelModel> associatedOrderStocks = stockLevels.stream()
        .filter(stockLevel -> stockLevel.getOrder() != null).collect(Collectors.toList());
    BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Stock size {} for associated order stock for serial {} while replacement of serial {}",associatedOrderStocks.size(),serialCode,serialCode);
    if(CollectionUtils.isNotEmpty(associatedOrderStocks)) {
      try {
            updateSerialOnOrder(serialProcessModel.getOldSerialProduct(), associatedOrderStocks);
      }catch (Exception ex){
        BlLogger.logMessage(LOG,Level.ERROR,"Some error occurred while replacement of serial :"+serialCode,ex);
        return Transition.NOK;
      }
    }

    return Transition.OK;
  }

  private void updateSerialOnOrder(final BlSerialProductModel blSerialProduct,List<StockLevelModel> associatedOrderStocks){

    final Map<String, List<StockLevelModel>> stockLevelsOrderWise = associatedOrderStocks.stream()
        .collect(Collectors.groupingBy(StockLevelModel::getOrder));
    BlLogger.logFormatMessageInfo(LOG,Level.INFO,"All the order {} in which need to replace serial from {} to new one",stockLevelsOrderWise.keySet().toString(),blSerialProduct.getCode());

    for(Map.Entry<String,List<StockLevelModel>> orderCodeEntry :stockLevelsOrderWise.entrySet()){
      String orderCode = orderCodeEntry.getKey();
      try {

        AtomicReference<Boolean> isSerialUpdated = new AtomicReference<>(false);
        final Map<String, List<StockLevelModel>> stockLevelsProductWise = orderCodeEntry.getValue()
            .stream().collect(Collectors.groupingBy(StockLevelModel::getProductCode));
        for (Map.Entry<String, List<StockLevelModel>> productCodeEntry : stockLevelsProductWise
            .entrySet()) {
          Set<String> productCode = new HashSet<>();
          productCode.add(productCodeEntry.getKey());
          Set<String> oldSerialProductCode = productCodeEntry.getValue().stream()
              .collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode)).keySet();
            final AbstractOrderModel order = getOrderDao().getOrderByCode(orderCode);
            if(BooleanUtils.isTrue(order.getIsRentalOrder())) {
              Date todaysDate = Date.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
              ConsignmentModel consignmentModel = order.getConsignments().iterator().next();
              if (null != consignmentModel &&  todaysDate.before(consignmentModel.getOptimizedShippingStartDate())) {
                filterOrderEntryAndAssignSerial(order, oldSerialProductCode, isSerialUpdated,
                        productCode);
              }
            }
          List<StockLevelModel> stockLevelModelList = orderCodeEntry.getValue();
          if (isSerialUpdated.get()) {
            stockLevelModelList.forEach(stockLevel -> {
              try {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                        "Remove order from stock for serial product {}, for stock date {} while replacing serial(old serial) before change Hard Assign {}, reserve status {}, associated order {}"
                                + ",current date {} current user {}", stockLevel.getSerialProductCode(), stockLevel.getDate(), stockLevel.getHardAssigned(), stockLevel.getReservedStatus(),
                        stockLevel.getOrder(), new Date(), (defaultBlUserService.getCurrentUser() != null ? defaultBlUserService.getCurrentUser().getUid() : "In Automation"));
              } catch (Exception e) {
                BlLogger.logMessage(LOG, Level.ERROR, "Some error occur while remove order from stock in replace serial flow", e);
              }
              stockLevel.setSerialStatus(blSerialProduct.getSerialStatus());
              stockLevel.setOrder(null);
              saveStockRecord(stockLevel, true);
            });
          }else {
            stockLevelModelList.forEach(stockLevel -> {
              stockLevel.setSerialStatus(blSerialProduct.getSerialStatus());
              saveStockRecord(stockLevel, true);
            });
          }
          break;
        }
      }catch (Exception ex){
        BlLogger.logMessage(LOG,Level.ERROR,"Some error occurred while replacement of serial for the order:"+orderCode,ex);
      }

    }

  }


  public void filterOrderEntryAndAssignSerial(AbstractOrderModel order ,Set<String> oldSerialProductCode, AtomicReference<Boolean> isSerialUpdated,Set<String> productCode){
    order.getConsignments().forEach(consignmentModel -> {
      consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
        consignmentEntryModel.getSerialProducts().forEach(blProductModel -> {
          if(blProductModel instanceof  BlSerialProductModel ){
            BlSerialProductModel olderSerialProduct = (BlSerialProductModel)blProductModel;
            if(olderSerialProduct.getCode().equals(oldSerialProductCode.iterator().next())){
              isSerialUpdated.set( findStockAndAssignSerial(productCode,consignmentModel,consignmentEntryModel,olderSerialProduct));
            }
          }
        });
      });
    });
  }


  private Boolean findStockAndAssignSerial(Set<String> productCode, ConsignmentModel consignmentModel,
      ConsignmentEntryModel consignmentEntryModel,BlSerialProductModel oldSerialProduct){
    final Collection<StockLevelModel> stockLevels = getBlCommerceStockService().getStockForProductCodesAndDate(productCode, consignmentModel.getWarehouse(), consignmentModel.getOptimizedShippingStartDate(), consignmentModel.getOptimizedShippingEndDate());
    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Stock size {} for product {} on warehouse {} for order {} for duration {} to {} while replacement of serial {}",
        stockLevels.size(), productCode.toString(), consignmentModel.getWarehouse().getCode(),
        consignmentModel.getOrder().getCode(),consignmentModel.getOptimizedShippingStartDate(),
        consignmentModel.getOptimizedShippingEndDate(),oldSerialProduct.getCode());
    BlSerialProductModel newSerial=null;
    if (CollectionUtils.isNotEmpty(stockLevels)) {
      final Map<String, List<StockLevelModel>> stockLevelsSerialWise = stockLevels
          .stream()
          .collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));

      Collection<BlSerialProductModel> blSerialProducts = getSessionService()
          .executeInLocalView(new SessionExecutionBody() {

            @Override
            public Collection<BlSerialProductModel> execute() {
              getSearchRestrictionService().disableSearchRestrictions();
              return productDao
                  .getBlSerialProductsForCodes(new HashSet<>(stockLevelsSerialWise.keySet()));
            }
          });


   BlLogger.logFormatMessageInfo(LOG,Level.INFO,"All the serial product {}",blSerialProducts.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));
      final List<BlSerialProductModel> nonBufferProducts = blSerialProducts.stream()
          .filter(serial -> BooleanUtils.isFalse(serial.getIsBufferedInventory()))
          .collect(Collectors.toList());
      final List<BlSerialProductModel> bufferProducts = new ArrayList(blSerialProducts);
      bufferProducts.removeAll(nonBufferProducts); // now it was only contain buffer product
      BlLogger.logFormatMessageInfo(LOG,Level.INFO,"All the buffer serial product {}",bufferProducts.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));
      BlLogger.logFormatMessageInfo(LOG,Level.INFO,"All the non buffer serial product {}",nonBufferProducts.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));

      if(CollectionUtils.isNotEmpty(nonBufferProducts)) {
        newSerial  = filterAndAssignSerial(nonBufferProducts,consignmentEntryModel,oldSerialProduct);
      }else{
        newSerial= filterAndAssignSerial(bufferProducts,consignmentEntryModel,oldSerialProduct);
      }
      if(null!=newSerial){
        List<StockLevelModel> stockLevelModels = stockLevelsSerialWise.get(newSerial.getCode());
        stockLevelModels.forEach(stockLevel ->{
          try {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                    "Reserve stock for serial product {}, for stock date {} while replacing serial before change Hard Assign {}, reserve status {}, associated order {}"
                            + ",current date {} current user {}", stockLevel.getSerialProductCode(), stockLevel.getDate(), stockLevel.getHardAssigned(), stockLevel.getReservedStatus(),
                    stockLevel.getOrder(), new Date(), (defaultBlUserService.getCurrentUser() != null ? defaultBlUserService.getCurrentUser().getUid() : "In Automation"));
          } catch (Exception e) {
            BlLogger.logMessage(LOG, Level.ERROR, "Some error occur while reserve stock in replace serial flow", e);
          }
          stockLevel.setOrder(consignmentModel.getOrder().getCode());
          stockLevel.setReservedStatus(true);
          BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                  "Stock status is changed to {} for the serial product {} for the order {} ", stockLevel.getReservedStatus(),
                  stockLevel.getSerialProductCode(), stockLevel.getOrder());


        } );
        modelService.saveAll(stockLevelModels);
        return true;
      }
    }
    return false;
  }

  public BlSerialProductModel filterAndAssignSerial(List<BlSerialProductModel> serialProducts,ConsignmentEntryModel consignmentEntryModel,BlSerialProductModel oldSerialProduct){

    final List<BlSerialProductModel> consignerSerial = serialProducts.stream()
        .filter(serial -> "BL".equalsIgnoreCase(serial.getOwnedBy()))
        .collect(Collectors.toList());
    BlLogger.logFormatMessageInfo(LOG,Level.INFO,"All the consigner serial product {}",consignerSerial.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));
      if (CollectionUtils.isNotEmpty(consignerSerial)) {
      return assignSerial(consignerSerial, consignmentEntryModel, oldSerialProduct);
    } else {
      final List<BlSerialProductModel> nonSaleAndNonBLSerials = serialProducts.stream()
          .filter(serial -> !"BL".equalsIgnoreCase(serial.getOwnedBy())).filter(serial -> !serial.getForSale()).collect(Collectors.toList());
      BlLogger.logFormatMessageInfo(LOG,Level.INFO,"All the non sale non BL serial product {}",nonSaleAndNonBLSerials.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));
      if(CollectionUtils.isNotEmpty(nonSaleAndNonBLSerials)){
        return assignSerial(nonSaleAndNonBLSerials, consignmentEntryModel, oldSerialProduct);
      }
    }

    return null;
  }

  public BlSerialProductModel assignSerial(List<BlSerialProductModel> availableSerials,ConsignmentEntryModel consignmentEntryModel,BlSerialProductModel oldSerialProduct){
    BlSerialProductModel newSerialProduct = availableSerials.get(0);

    List<BlProductModel> serialProducts = consignmentEntryModel.getSerialProducts();
      serialProducts = CollectionUtils.isNotEmpty(serialProducts)?new ArrayList<BlProductModel>(serialProducts) :new ArrayList<BlProductModel>();
      serialProducts.add(newSerialProduct);
      serialProducts.remove(oldSerialProduct);
      consignmentEntryModel.setSerialProducts(serialProducts);


    Map<String, ItemStatusEnum> items = consignmentEntryModel.getItems();
    items = (items == null || items.isEmpty()) ? new HashMap<>() : new HashMap<>(items);
    if(items.isEmpty()){
      items.put(newSerialProduct.getCode(),ItemStatusEnum.NOT_INCLUDED);
    }else {
      items.put(newSerialProduct.getCode(), items.get(oldSerialProduct.getCode()));
      items.remove(oldSerialProduct.getCode());
    }
    consignmentEntryModel.setItems(items);


    Map<String, ConsignmentEntryStatusEnum> consignmentEntryStatus = consignmentEntryModel
        .getConsignmentEntryStatus();
    consignmentEntryStatus= (consignmentEntryStatus == null || consignmentEntryStatus.isEmpty()) ? new HashMap<>() : new HashMap<>(consignmentEntryStatus);
    if (consignmentEntryStatus.isEmpty()){
      consignmentEntryStatus.put(newSerialProduct.getCode(),ConsignmentEntryStatusEnum.NOT_SHIPPED);
    }else {
      consignmentEntryStatus
          .put(newSerialProduct.getCode(), consignmentEntryStatus.get(oldSerialProduct.getCode()));
      consignmentEntryStatus.remove(oldSerialProduct.getCode());
    }
    consignmentEntryModel.setConsignmentEntryStatus(consignmentEntryStatus);

    List<BlProductModel> serialProductOnOrderEntry = consignmentEntryModel.getOrderEntry()
        .getSerialProducts();
    serialProductOnOrderEntry = CollectionUtils.isNotEmpty(serialProductOnOrderEntry) ? new ArrayList<>(serialProductOnOrderEntry) : new ArrayList<>();
      serialProductOnOrderEntry.add(newSerialProduct);
      serialProductOnOrderEntry.remove(oldSerialProduct);
      consignmentEntryModel.getOrderEntry().setSerialProducts(serialProductOnOrderEntry);

    modelService.save(consignmentEntryModel.getOrderEntry());
    modelService.refresh(consignmentEntryModel.getOrderEntry());
    modelService.save(consignmentEntryModel);
    modelService.refresh(consignmentEntryModel);
    return newSerialProduct;
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
  public BlProductDao getProductDao() {
    return productDao;
  }

  public void setProductDao(BlProductDao productDao) {
    this.productDao = productDao;
  }

  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }

  public SearchRestrictionService getSearchRestrictionService() {
    return searchRestrictionService;
  }

  public void setSearchRestrictionService(
      SearchRestrictionService searchRestrictionService) {
    this.searchRestrictionService = searchRestrictionService;
  }

  public SessionService getSessionService() {
    return sessionService;
  }

  public void setSessionService(SessionService sessionService) {
    this.sessionService = sessionService;
  }
}
