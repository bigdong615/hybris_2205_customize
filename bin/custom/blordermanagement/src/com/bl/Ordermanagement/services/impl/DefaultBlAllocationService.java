package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.exceptions.BlShippingOptimizationException;
import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlOptionsModel;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
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

    if (MapUtils.isEmpty(result.getAllocation()) || (MapUtils
        .isEmpty(result.getSerialProductMap()) && !blOrderService
        .isAquatechProductsPresentInOrder(order))) {

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

      consignment
          .setShippingDate(this.getShippingDateStrategy().getExpectedShippingDate(consignment));

      //adding optimized shipping dates, this will be updated in optimized shipping strategy methods
      consignment.setOptimizedShippingStartDate(order.getActualRentalStartDate());
      consignment.setOptimizedShippingEndDate(order.getActualRentalEndDate());

      if (result.isOrderTransferConsignment()) {
        flagConsignmentAndSetShippingDateForOrderTransfers(consignment, order);
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
            serialStocks.forEach(stock -> stock.setReservedStatus(true));

            //setAssignedFlagOfSerialProduct(result.getSerialProductMap().values(), BlCoreConstants.SOFT_ASSIGNED);

            this.getModelService().saveAll(serialStocks);

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

  /**
   * Set order transfer related values to consignment
   *
   * @param consignment  -  the consignment
   * @param order   -  the order
   */
  private void flagConsignmentAndSetShippingDateForOrderTransfers(final ConsignmentModel consignment,
      final AbstractOrderModel order) {

    final List<Date> holidayBlackoutDates = blDatePickerService
        .getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);

    final Date actualShippingDateToCustomer = BlDateTimeUtils
        .getDateWithAddedDays(1, order.getActualRentalStartDate(), holidayBlackoutDates);

    consignment.setOrderTransferConsignment(true);
    consignment.setActualShippingDateToCustomer(actualShippingDateToCustomer);

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Optimized shipping start date / Order transfer date : {} from first warehouse : {} and actual shipping date to customer : {} set on consignment with code {}",
        consignment.getOptimizedShippingStartDate(), consignment.getWarehouse().getCode(),
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
      consignment.setThreeDayGroundAvailability(result.isThreeDayGroundAvailability());
      getBlShippingOptimizationStrategy().getOptimizedShippingMethodForOrder(consignment);
    } catch (final Exception e) {
       throw new BlShippingOptimizationException(ERROR_WHILE_OPTIMIZING_THE_ORDER, e);
      }
    }
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
            order.getActualRentalEndDate());
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

        setItemsMap(entry, serialProductModels);
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
   * Created Map to display Shipper what all items are attached to the consignment. So that agent can verify and scan the serial.
   * Sub-parts and serials both will be added to this Map.
   * During Sub-parts scanning, please replace sub-part name with sub-part serial code
   * ex:
   * BEFORE SCANNING --->
   * 54356 NOT_INCLUDED
   * 46363 NOT_INCLUDED
   * Lens Hood-1 NOT_INCLUDED (Sub-parts Name associated)
   * Lens Hood-2 NOT_INCLUDED (Sub-parts Name associated)
   * Battery NOT_INCLUDED (Sub-parts Name associated)
   *
   * AFTER SCANNING --->
   * 54356 INCLUDED
   * 46363 INCLUDED
   * GHDKD INCLUDED (Sub-parts Serial associated)
   * EGDBD INCLUDED (Sub-parts Serial associated)
   * Battery INCLUDED (This Sub-parts has no barcode, So manually INCLUDED by shipper)
   *
   * @param entry
   * @param serialProductModels
   * @return
   */
  private void setItemsMap(final ConsignmentEntryModel entry,
      final Set<BlSerialProductModel> serialProductModels) {

    final Map<String, ItemStatusEnum> itemsMap = new HashMap<>();
    final List<BlProductModel> allSerialSubPartProducts = new ArrayList<>();

    serialProductModels.forEach(serial -> {

      itemsMap.put(serial.getCode(), ItemStatusEnum.NOT_INCLUDED);

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "Serial product with code {} added to the products list on consignment entry with consignment code {}",
          serial.getCode(), entry.getConsignment().getCode());

      allSerialSubPartProducts.addAll(getSessionService()
          .executeInLocalView(new SessionExecutionBody() {
            @Override
            public List<BlProductModel> execute() {
              getSearchRestrictionService().disableSearchRestrictions();
              if (null != serial.getBlProduct() && CollectionUtils
                  .isNotEmpty(serial.getBlProduct().getSubParts())) {

                return (List<BlProductModel>) serial.getBlProduct().getSubParts();
              }
              return new ArrayList<>();
            }
          }));
    });

    putSubPartProductsInToItemsMap(entry, itemsMap, allSerialSubPartProducts);

    putProductOptionsInToItemsMap(entry, itemsMap);

    entry.setItems(itemsMap);
  }

  /**
   * Add subpart product to items map of consignment entry.
   * @param consignmentEntry
   * @param itemsMap
   * @param allSerialSubPartProducts
   */
  private void putSubPartProductsInToItemsMap(final ConsignmentEntryModel consignmentEntry,
      final Map<String, ItemStatusEnum> itemsMap,
      final List<BlProductModel> allSerialSubPartProducts) {

    final Map<BlProductModel, Integer> allSerialSubPartProductMap = new HashMap<>();
    for (BlProductModel productModel : allSerialSubPartProducts) {

      if (null != allSerialSubPartProductMap.get(productModel)) {

        allSerialSubPartProductMap.put(productModel,
            allSerialSubPartProductMap.get(productModel) + productModel.getSubpartQuantity());
      } else {

        allSerialSubPartProductMap.put(productModel, productModel.getSubpartQuantity());
      }
    }

    allSerialSubPartProductMap.entrySet().forEach(mapEntry -> {

      final BlProductModel subPartProduct = mapEntry.getKey();
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "Sub part with code {} and quantity {} added to the products list on consignment entry.",
          subPartProduct.getCode(), mapEntry.getValue());

      if (mapEntry.getValue() == 1) {

        itemsMap.put(subPartProduct.getName(), ItemStatusEnum.NOT_INCLUDED);

        addSubPartToConsignmentEntry(consignmentEntry, subPartProduct);
      } else {
        for (int i = 1; i <= mapEntry.getValue(); i++) {

          itemsMap.put(subPartProduct.getName() + BlCoreConstants.DOUBLE_HYPHEN + i,
              ItemStatusEnum.NOT_INCLUDED);

          addSubPartToConsignmentEntry(consignmentEntry, subPartProduct);
        }
      }

    });
  }

  /**
   * Add subpart product to consignment entry.
   * @param consignmentEntry
   * @param subPartProduct
   */
  private void addSubPartToConsignmentEntry(final ConsignmentEntryModel consignmentEntry,
      final BlProductModel subPartProduct) {

    consignmentEntry.getSerialProducts().add(subPartProduct);
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "Sub part with name {} added to the products list on consignment entry with consignment code {}",
        subPartProduct.getName(), consignmentEntry.getConsignment().getCode());
  }

  /**
   * Update consignment entry with product options.
   * @param consignmentEntry
   * @param itemsMap
   */
  private void putProductOptionsInToItemsMap(final ConsignmentEntryModel consignmentEntry,
      final Map<String, ItemStatusEnum> itemsMap) {

    final AbstractOrderEntryModel orderEntry = consignmentEntry.getOrderEntry();

    if (CollectionUtils.isNotEmpty(orderEntry.getOptions())) {

      final BlOptionsModel optionsModel = orderEntry.getOptions().get(0);
      if (consignmentEntry.getQuantity() == 1) {

        itemsMap.put(optionsModel.getName(), ItemStatusEnum.NOT_INCLUDED);

        addProductOptionsToConsignmentEntry(consignmentEntry, optionsModel);
      } else {
        for (int i = 1; i <= consignmentEntry.getQuantity(); i++) {

          itemsMap.put(optionsModel.getName() + BlCoreConstants.DOUBLE_HYPHEN + i,
              ItemStatusEnum.NOT_INCLUDED);

          addProductOptionsToConsignmentEntry(consignmentEntry, optionsModel);
        }
      }
    }

  }

  /**
   * Add product options to consignment entry.
   * @param consignmentEntry
   * @param optionsModel
   */
  private void addProductOptionsToConsignmentEntry(final ConsignmentEntryModel consignmentEntry,
      final BlOptionsModel optionsModel) {

    final List<BlOptionsModel> productOptionsToAdd =
        CollectionUtils.isNotEmpty(consignmentEntry.getOptions()) ? new ArrayList<>(
            consignmentEntry.getOptions()) : new ArrayList<>();
    productOptionsToAdd.add(optionsModel);
    consignmentEntry.setOptions(productOptionsToAdd);

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "Product option with name {} added to the options list on consignment entry with consignment code {}",
        optionsModel.getName(), consignmentEntry.getConsignment().getCode());
  }
  
  /**
   * Sets the serial codes to billing charges.
   *
   * @param consignmentEntry
   *           the entry
   * @param serialProductModels
   *           the serial product models
   */
  private void setSerialCodesToBillingCharges(final ConsignmentEntryModel consignmentEntry,
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
}
