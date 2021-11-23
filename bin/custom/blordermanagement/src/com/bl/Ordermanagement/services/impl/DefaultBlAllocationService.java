package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.exceptions.BlShippingOptimizationException;
import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.core.services.order.BlOrderService;
import com.bl.core.shipping.strategy.BlShippingOptimizationStrategy;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.google.common.base.Strings;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.warehousing.allocation.impl.DefaultAllocationService;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

/**
 * It is used to allocate the order in consignments.
 *
 * @author Sunil
 */
public class DefaultBlAllocationService extends DefaultAllocationService implements
    BlAllocationService {

  private static final Logger LOG = Logger.getLogger(DefaultBlAllocationService.class);
  public static final String ERROR_WHILE_ALLOCATING_THE_ORDER = "Error while allocating the order.";
  public static final String ERROR_WHILE_OPTIMIZING_THE_ORDER = "Error while optimizing the order.";
  private BlStockLevelDao blStockLevelDao;
  private SessionService sessionService;
  private SearchRestrictionService searchRestrictionService;
  private BlShippingOptimizationStrategy blShippingOptimizationStrategy;
  private BlOrderService blOrderService;
  private BlDatePickerService blDatePickerService;
  private BlConsignmentEntryService blConsignmentEntryService;

  /**
   * Create consignment.
   *
   * @param order  -  the order
   * @param code   -  the consignment code
   * @param result -  the SourcingResults
   * @return ConsignmentModel
   */
  @Override
  public ConsignmentModel createConsignment(final AbstractOrderModel order, final String code,
      final SourcingResult result) {

    if (BooleanUtils.isFalse(order.getInternalTransferOrder()) && (MapUtils.isEmpty(result.getAllocation()) || (MapUtils
        .isEmpty(result.getSerialProductMap()) && !blOrderService
        .isAquatechProductsPresentInOrder(order)))) {

      throw new BlSourcingException(ERROR_WHILE_ALLOCATING_THE_ORDER);
    }

    try {

      ServicesUtil.validateParameterNotNullStandardMessage("result", result);
      ServicesUtil.validateParameterNotNullStandardMessage("order", order);
      Assert.isTrue(!Strings.isNullOrEmpty(code), "Parameter code cannot be null or empty");

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Creating consignment for Location: '{}'",
          result.getWarehouse().getCode());
      final ConsignmentModel consignment = (ConsignmentModel) this.getModelService()
          .create(ConsignmentModel.class);
      consignment.setCode(code);
      consignment.setOrder(order);
      consignment.setStatus(ConsignmentStatus.READY);
      consignment.setDeliveryMode(order.getDeliveryMode());

      if (null != order.getDeliveryAddress()) {
        consignment.setShippingAddress(order.getDeliveryAddress());
      } else if (null != order.getDeliveryMode()) {

        consignment.setShippingAddress(
            ((BlPickUpZoneDeliveryModeModel) order.getDeliveryMode()).getInternalStoreAddress());
      }


      //adding optimized shipping dates, this will be updated in optimized shipping strategy methods
      consignment.setOptimizedShippingStartDate(order.getActualRentalStartDate());
      consignment.setOptimizedShippingEndDate(order.getActualRentalEndDate());

      if (result.isOrderTransferConsignment()) {
        flagConsignmentAndSetShippingDateForOrderTransfers(consignment, order, result.getWarehouse());
      }

      if (isInternalTransferOder(order)) {
        consignment.setInternalTransferConsignment(order.getInternalTransferOrder());
      }

      consignment.setFulfillmentSystemConfig(
          this.getWarehousingFulfillmentConfigDao().getConfiguration(result.getWarehouse()));
      final Set<Entry<AbstractOrderEntryModel, Long>> resultEntries = result.getAllocation()
          .entrySet();

      final Set<ConsignmentEntryModel> entries = resultEntries.stream().map(mapEntry ->
          this.createConsignmentEntry(mapEntry.getKey(), mapEntry.getValue(), consignment, result)
      ).collect(Collectors.toSet());
      consignment.setConsignmentEntries(entries);
      consignment.setWarehouse(result.getWarehouse());
      if (consignment.getFulfillmentSystemConfig() == null) {
        this.getWarehousingConsignmentWorkflowService().startConsignmentWorkflow(consignment);
      }

      //update order notes
      setConsignmentsInNotes(order, consignment);

      //for internal transfer orders, saving and returning consignment without checking availability
      if (isInternalTransferOder(order)) {
        this.getModelService().save(consignment);
        return consignment;
      }

      if (BooleanUtils.isTrue(order.getIsRentalCart())) {

        final List<String> allocatedProductCodes = new ArrayList<>();
        if (null != result.getSerialProductMap()) {
          for (Set<BlSerialProductModel> productSet : result.getSerialProductMap().values()) {
            allocatedProductCodes
                .addAll(productSet.stream().map(BlSerialProductModel::getCode).collect(
                    Collectors.toSet()));
          }
        }

        if (!blOrderService.isAquatechProductOrder(order) && CollectionUtils
            .isNotEmpty(allocatedProductCodes)) {

          final Collection<StockLevelModel> serialStocks = getSerialsForDateAndCodes(order,
              new HashSet<>(allocatedProductCodes));

          if ((!serialStocks.isEmpty()) && serialStocks.stream()
              .allMatch(stock -> allocatedProductCodes.contains(stock.getSerialProductCode()))) {

            this.optimizeShippingMethodForConsignment(consignment, result);
            this.getModelService().save(consignment);

            final Collection<StockLevelModel> serialStocksForOptimizedDates = blStockLevelDao
                .findSerialStockLevelsForDateAndCodes(new HashSet<>(allocatedProductCodes),
                    consignment.getOptimizedShippingStartDate(),
                    consignment.getOptimizedShippingEndDate(), Boolean.FALSE);

            serialStocksForOptimizedDates.forEach(stock -> {
              stock.setReservedStatus(true);
              stock.setOrder(order.getCode());
              BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                  "Stock status is changed to {} for the serial product {} for the order {} ", stock.getReservedStatus(),
                  stock.getSerialProductCode(), stock.getOrder());
            });

            //setAssignedFlagOfSerialProduct(result.getSerialProductMap().values(), BlCoreConstants.SOFT_ASSIGNED);

            this.getModelService().saveAll(serialStocksForOptimizedDates);

            return consignment;

          } else {

            order.setStatus(OrderStatus.SUSPENDED);
            order.setConsignments(null);
            order.getEntries().stream().forEach(entry -> entry.setConsignmentEntries(null));
            getModelService().save(order);
            BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
                "At the time of consignment creation, the availability of the allocated serial products not found.");

            throw new BlSourcingException(ERROR_WHILE_ALLOCATING_THE_ORDER);
          }
        } else {  // for only aquatech products in order

          this.optimizeShippingMethodForConsignment(consignment, result);
          this.getModelService().save(consignment);
          return consignment;
        }
      } else{   // used gear cart

        //setAssignedFlagOfSerialProduct(result.getSerialProductMap().values(), BlCoreConstants.HARD_ASSIGNED);
        //this.optimizeShippingMethodForConsignment(consignment, result);   // need clarification
        this.getModelService().save(consignment);

        return consignment;
      }
    } catch (final Exception ex) {
      throw new BlSourcingException(ERROR_WHILE_ALLOCATING_THE_ORDER, ex);
    }
  }

  private boolean isInternalTransferOder(final AbstractOrderModel orderModel) {

    return orderModel.getInternalTransferOrder();
  }

  /**
   * Update consignment in order notes.
   *
   * @param abstractOrderModel - the order model
   * @param consignmentModel      - newly created consignment
   */
  private void setConsignmentsInNotes(final AbstractOrderModel abstractOrderModel,
      final ConsignmentModel consignmentModel) {

    final List<NotesModel> orderNotesFromOrder = abstractOrderModel.getOrderNotes();
    if (CollectionUtils.isNotEmpty(orderNotesFromOrder)) {

      orderNotesFromOrder.forEach(orderNote -> {
        final Set<ConsignmentModel> orderNoteConsignments = new HashSet<>(
            orderNote.getConsignment());
        orderNoteConsignments.add(consignmentModel);
        orderNote.setConsignment(orderNoteConsignments);
      });
      this.getModelService().saveAll(orderNotesFromOrder);

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment is set in to Order Notes");
    }
  }

  /**
   * Set order transfer related values to consignment
   *  @param consignment  -  the consignment
   * @param order   -  the order
   * @param warehouse
   */
  private void flagConsignmentAndSetShippingDateForOrderTransfers(
      final ConsignmentModel consignment,
      final AbstractOrderModel order,
      final WarehouseModel warehouse) {

    final List<Date> holidayBlackoutDates = blDatePickerService
        .getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);

    final Date actualShippingDateToCustomer = BlDateTimeUtils
        .getDateWithAddedDays(1, order.getActualRentalStartDate(), holidayBlackoutDates);

    consignment.setOrderTransferConsignment(true);
    consignment.setActualShippingDateToCustomer(actualShippingDateToCustomer);

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Optimized shipping start date / Order transfer date : {} from first warehouse : {} and actual shipping date to customer : {} set on consignment with code {}",
        consignment.getOptimizedShippingStartDate(), warehouse.getCode(),
        consignment.getActualShippingDateToCustomer(),
        consignment.getCode());
  }

  /**
   * Set assigned flag to hardAssign or softAssign for the serial products
   *
   * @param serialProducts  -  the serialProducts
   * @param assigned   -  the assigned
   */
  private void setAssignedFlagOfSerialProduct(final Collection<Set<BlSerialProductModel>> serialProducts,
      final String assigned) {

    List<BlSerialProductModel> serialProductModelList = new ArrayList<>();
    for (Set<BlSerialProductModel> serialProductSet : serialProducts) {

      if (StringUtils.equalsIgnoreCase(assigned, BlCoreConstants.HARD_ASSIGNED)) {

        serialProductSet.forEach(serialProduct -> serialProduct.setHardAssigned(true));
      } else {

        serialProductSet.forEach(serialProduct -> serialProduct.setSoftAssigned(true));
      }
      serialProductModelList.addAll(serialProductSet);
    }

    getSessionService()
        .executeInLocalView(new SessionExecutionBody() {
          @Override
          public void executeWithoutResult() {
            getSearchRestrictionService().disableSearchRestrictions();
            getModelService().saveAll(serialProductModelList);
          }
        });
  }

  /**
   * javadoc
   * this method will call optimization flow for consignment
   *
   * @param consignment model
   * @param result sourcingResult
   */
  private void optimizeShippingMethodForConsignment(final ConsignmentModel consignment, final SourcingResult result) {

    if (!consignment.isOrderTransferConsignment()) {
    try {
      if(!isFrontDeskOrder(consignment)) {
        consignment.setThreeDayGroundAvailability(result.isThreeDayGroundAvailability());
        getBlShippingOptimizationStrategy().getOptimizedShippingMethodForOrder(consignment);
      }
    } catch (final Exception e) {
       throw new BlShippingOptimizationException(ERROR_WHILE_OPTIMIZING_THE_ORDER, e);
      }
    }
  }

  /**
   * This method used for checking front desk.
   * @param consignment
   * @return
   */
  private boolean isFrontDeskOrder(final ConsignmentModel consignment) {
    if (consignment != null && (consignment.getDeliveryMode() != null)) {
        return consignment.getDeliveryMode() instanceof BlPickUpZoneDeliveryModeModel && consignment.getDeliveryMode().getCode().startsWith(BlCoreConstants.FRONT_DESK_DELIVERY_MODE_KEY_PREFIX);
    }
    return Boolean.FALSE;
  }
  /**
   * method will used to get serial for date and codes
 * @param order as Order
 * @param serialProductCodes as serialProductCodes
 * @return Collection<StockLevelModel serialStocks
 */
public Collection<StockLevelModel> getSerialsForDateAndCodes(final AbstractOrderModel order,
      final Set<String> serialProductCodes) {

    return blStockLevelDao
        .findSerialStockLevelsForDateAndCodes(serialProductCodes, order.getActualRentalStartDate(),
            order.getActualRentalEndDate(), Boolean.FALSE);
  }

  /**
   * Create consignment entry.
   * @param orderEntry as order Entry
   * @param quantity as quantity
   * @param consignment as consignment 
   * @param result as result 
   * @return consignment entry
   */
  public ConsignmentEntryModel createConsignmentEntry(final AbstractOrderEntryModel orderEntry,
      Long quantity, final ConsignmentModel consignment, final SourcingResult result) {

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "ConsignmentEntry :: Product [{}]: \tQuantity: '{}'",
        orderEntry.getProduct().getCode(), quantity);
    final ConsignmentEntryModel entry = (ConsignmentEntryModel) this.getModelService()
        .create(ConsignmentEntryModel.class);
    entry.setOrderEntry(orderEntry);
    entry.setQuantity(quantity);
    entry.setConsignment(consignment);
    //entry.setSerialProductCodes(result.getSerialProductCodes());   //setting serial products from result
    if (!blOrderService.isAquatechProductOrder(orderEntry.getOrder())) {

      final List<BlProductModel> consignmentEntrySerialProducts =
          null != entry.getSerialProducts() ? entry.getSerialProducts() : new ArrayList<>();

      final Set<BlSerialProductModel> serialProductModels =
          null != result.getSerialProductMap() ? result.getSerialProductMap()
              .get(orderEntry.getEntryNumber()) : new HashSet<>();

      if (CollectionUtils.isNotEmpty(serialProductModels)) {

        consignmentEntrySerialProducts.addAll(serialProductModels);
        entry.setSerialProducts(
            consignmentEntrySerialProducts);   //setting serial products from result

        getBlConsignmentEntryService().setItemsMap(entry, serialProductModels);
        setSerialCodesToBillingCharges(entry, serialProductModels);
      }

      final List<BlProductModel> productModels =
          null != result.getAquatechProductMap() ? result.getAquatechProductMap()
              .get(orderEntry.getEntryNumber()) : new ArrayList<>();

      if (CollectionUtils.isNotEmpty(productModels)) {

        consignmentEntrySerialProducts.addAll(productModels);
        entry.setSerialProducts(
            consignmentEntrySerialProducts);   //setting serial products from result

        setItemsMapForAquatechProducts(entry, productModels);
      }


    } else {

      final List<BlProductModel> productModels =
          null != result.getAquatechProductMap() ? result.getAquatechProductMap()
              .get(orderEntry.getEntryNumber()) : new ArrayList<>();

      entry.setSerialProducts(
          new ArrayList<>(productModels));   //setting aquatech products from result

      setItemsMapForAquatechProducts(entry, productModels);
    }

    if (isInternalTransferOder(orderEntry.getOrder())) {
      getBlConsignmentEntryService().setItemsMapForInternalTransferOrders(entry, orderEntry);
    }

    final Set<ConsignmentEntryModel> consignmentEntries = new HashSet<>();
    if (orderEntry.getConsignmentEntries() != null) {
      orderEntry.getConsignmentEntries().forEach(consignmentEntries::add);
    }

    consignmentEntries.add(entry);
    orderEntry.setConsignmentEntries(consignmentEntries);
    return entry;
  }

  /**
   * Created Map for aquatech products which will be stored in consignment entry.
   *
   */
  private void setItemsMapForAquatechProducts(final ConsignmentEntryModel entry,
      final List<BlProductModel> productModels) {

    final Map<String, ItemStatusEnum> itemsMap =
        null != entry.getItems() ? entry.getItems() : new HashMap<>();

    if (productModels.size() == 1) {

      itemsMap.put(productModels.get(0).getName(), ItemStatusEnum.NOT_INCLUDED);

    } else {
      for (int i = 1; i <= productModels.size(); i++) {

        itemsMap.put(productModels.get(i-1).getName() + BlCoreConstants.DOUBLE_HYPHEN + i,
            ItemStatusEnum.NOT_INCLUDED);
      }
    }

    entry.setItems(itemsMap);
  }

  /**
   * Sets the serial codes to billing charges.
   *
   * @param consignmentEntry
   *           the entry
   * @param serialProductModels
   *           the serial product models
   */
  public void setSerialCodesToBillingCharges(final ConsignmentEntryModel consignmentEntry,
		  final Set<BlSerialProductModel> serialProductModels)
  {
	  final Map<String, List<BlItemsBillingChargeModel>> billingCharges = serialProductModels.stream()
			  .collect(Collectors.toMap(BlSerialProductModel::getCode, serial -> new ArrayList<BlItemsBillingChargeModel>()));
	  consignmentEntry.setBillingCharges(billingCharges);
  }

  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }

  public SessionService getSessionService() {
    return sessionService;
  }

  public void setSessionService(final SessionService sessionService) {
    this.sessionService = sessionService;
  }


  public SearchRestrictionService getSearchRestrictionService() {
    return searchRestrictionService;
  }

  public void setSearchRestrictionService(
      final SearchRestrictionService searchRestrictionService) {
    this.searchRestrictionService = searchRestrictionService;
  }

  public BlShippingOptimizationStrategy getBlShippingOptimizationStrategy() {
    return blShippingOptimizationStrategy;
  }

  public void setBlShippingOptimizationStrategy(BlShippingOptimizationStrategy blShippingOptimizationStrategy) {
    this.blShippingOptimizationStrategy = blShippingOptimizationStrategy;
  }

  public BlOrderService getBlOrderService() {
    return blOrderService;
  }

  public void setBlOrderService(final BlOrderService blOrderService) {
    this.blOrderService = blOrderService;
  }

  public BlDatePickerService getBlDatePickerService() {
    return blDatePickerService;
  }

  public void setBlDatePickerService(final BlDatePickerService blDatePickerService) {
    this.blDatePickerService = blDatePickerService;
  }

/**
 * @return the blConsignmentEntryService
 */
public BlConsignmentEntryService getBlConsignmentEntryService()
{
	return blConsignmentEntryService;
}

/**
 * @param blConsignmentEntryService the blConsignmentEntryService to set
 */
public void setBlConsignmentEntryService(BlConsignmentEntryService blConsignmentEntryService)
{
	this.blConsignmentEntryService = blConsignmentEntryService;
}
}
