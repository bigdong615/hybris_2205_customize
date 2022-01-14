package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.OptimizedShippingMethodEnum;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.shipping.strategy.BlShippingOptimizationStrategy;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Sets;

/**
 * It is used to assign serial products to sourcing results in context.
 *
 * @author Sunil
 */
public class DefaultBlAssignSerialService implements BlAssignSerialService {

  private static final Logger LOG = Logger.getLogger(DefaultBlAssignSerialService.class);
  private BlProductDao blProductDao;
  private ModelService modelService;
  private SearchRestrictionService searchRestrictionService;
  private SessionService sessionService;
  private BlShippingOptimizationStrategy blShippingOptimizationStrategy;
  private BlDeliveryModeService zoneDeliveryModeService;
  private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;

  /**
   * {@inheritDoc}
   */
  public boolean assignSerialsFromLocation(final SourcingContext context,
      SourcingLocation sourcingLocation) throws BlSourcingException {

    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Assigning serials from warehouse {}",
        sourcingLocation.getWarehouse().getCode());

    if(null != sourcingLocation.getAllocatedMap()) {
      BlLogger
          .logFormatMessageInfo(LOG, Level.INFO, "Allocation map before shipping optimization:- {}",
              sourcingLocation.getAllocatedMap().toString());
    }

    final List<AtomicBoolean> allEntrySourceComplete = new ArrayList<>();
    SourcingResult result = new SourcingResult();
    final SourcingLocation finalSourcingLocation;
        if(!isFrontDeskOrder(context)){
        finalSourcingLocation = isOrderBeingTransferredToOtherWarehouse(context, sourcingLocation.getWarehouse())
            ?  sourcingLocation : getBlShippingOptimizationStrategy()
            .getProductAvailabilityForThreeDayGround(context, sourcingLocation);
        }else{
          finalSourcingLocation = sourcingLocation;
        }
    if(null != finalSourcingLocation.getAllocatedMap()) {
      BlLogger
          .logFormatMessageInfo(LOG, Level.INFO, "Allocation map after shipping optimization:- {}",
              finalSourcingLocation.getAllocatedMap().toString());
    }

      context.getOrderEntries().forEach(entry -> {
        final Long quantity = entry.getQuantity();
        fulfillEachEntry(context, result, finalSourcingLocation, entry, allEntrySourceComplete, quantity);
    });

    //Ground availability status
    if(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode().equals(finalSourcingLocation.getGroundAvailabilityCode()) &&
            finalSourcingLocation.isGroundAvailability()) {
        result.setThreeDayGroundAvailability(finalSourcingLocation.isGroundAvailability());
    }

    return allEntrySourceComplete.stream().allMatch(AtomicBoolean::get) && isAllQuantityFulfilled(context);
  }

  /**
   *  This method used for checking front desk.
   * @param context
   * @return
   */
  private boolean isFrontDeskOrder(final SourcingContext context) {
    if (context != null && CollectionUtils.isNotEmpty(context.getOrderEntries())) {
      final Optional<AbstractOrderEntryModel> orderEntryModel = context.getOrderEntries().stream().findFirst();
      if (orderEntryModel.isPresent() && null != orderEntryModel.get().getOrder().getDeliveryMode()) {
        final DeliveryModeModel deliveryModeModel = orderEntryModel.get().getOrder().getDeliveryMode();
        return (deliveryModeModel instanceof BlPickUpZoneDeliveryModeModel && deliveryModeModel.getCode().startsWith(BlCoreConstants.FRONT_DESK_DELIVERY_MODE_KEY_PREFIX));
      }
    }
    return Boolean.FALSE;
  }

  public void fulfillEachEntry(final SourcingContext context, final SourcingResult result,
      final SourcingLocation finalSourcingLocation, final AbstractOrderEntryModel entry,
      final List<AtomicBoolean> allEntrySourceComplete, final Long quantity) {
    if (!isAquatechProductInEntry(entry)) {  //Skip for aquatech product entries
      final List<StockLevelModel> stocks = finalSourcingLocation.getAvailabilityMap()
          .get(entry.getProduct().getCode());

      if (CollectionUtils.isNotEmpty(stocks)) {
        validateAllocationRulesAndAssignSerials(context, context.getResult().getResults(),
            finalSourcingLocation.getWarehouse(), result, allEntrySourceComplete, entry,
            stocks, quantity);
      } else {
        allEntrySourceComplete.add(new AtomicBoolean(false));
      }
    } else {   //for aquatech product entries

      final List<BlProductModel> aquatechProductsToAssign = new ArrayList<>();
      for (int i = 0; i < quantity; i++){
        aquatechProductsToAssign.add((BlProductModel) entry.getProduct());
      }

      final Map<Integer, List<BlProductModel>> resultAquatechProductMap =
          (null != result.getAquatechProductMap()) ? new HashMap<>(result.getAquatechProductMap()) : new HashMap<>();
      resultAquatechProductMap.put(entry.getEntryNumber(), aquatechProductsToAssign);

      final Map<AbstractOrderEntryModel, Long> resultAllocationMap =
          (null != result.getAllocation()) ? new HashMap<>(result.getAllocation())
              : new HashMap<>();
      resultAllocationMap.put(entry, (long) aquatechProductsToAssign.size());

      result.setAquatechProductMap(resultAquatechProductMap);
      result.setAllocation(resultAllocationMap);
      result.setWarehouse(finalSourcingLocation.getWarehouse());
      context.getResult().getResults().add(result);

      allEntrySourceComplete.add(new AtomicBoolean(true));
    }
  }

  /**
   * Check whether the order will be sourced and transferred to different warehouse.
   *
   * @param context
   * @param warehouseToBeSourced
   * @return true if delivery mode is order transfer eligible and order is sourced from other
   * warehouse
   */
  private boolean isOrderBeingTransferredToOtherWarehouse(final SourcingContext context,
      final WarehouseModel warehouseToBeSourced) {

    final AbstractOrderModel order = context.getOrderEntries().iterator().next().getOrder();
    final WarehouseModel primaryWarehouse = blDeliveryStateSourcingLocationFilter
        .applyFilter(order);

    return zoneDeliveryModeService
        .isEligibleDeliveryModeForOrderTransfer((ZoneDeliveryModeModel) order.getDeliveryMode())
        && !primaryWarehouse.getCode().equalsIgnoreCase(warehouseToBeSourced.getCode());
  }

  /**
   * Check whether aquatech product is in given order entry.
   *
   * @param orderEntry
   * @return true if aquatech product is in this entry.
   */
  private boolean isAquatechProductInEntry(final AbstractOrderEntryModel orderEntry) {

    return BlCoreConstants.AQUATECH_BRAND_ID.equals(orderEntry.getProduct().getManufacturerAID());
  }

  /**
   * {@inheritDoc}
   */
  public boolean isAllQuantityFulfilled(final SourcingContext context) {
    final List<AtomicBoolean> allEntryQuantityFulfilled = new ArrayList<>();
    context.getOrderEntries().stream().forEach(entry -> {
      Long allResultQuantityAllocated = 0l;
      for (SourcingResult result : context.getResult().getResults()) {
        if(null != result.getAllocation().get(entry)) {
          allResultQuantityAllocated += result.getAllocation().get(entry);
        }
      }

      if (!isAquatechProductInEntry(entry)) {
        if (allResultQuantityAllocated.equals(entry.getQuantity())) {
          allEntryQuantityFulfilled.add(new AtomicBoolean(true));
        } else {
          allEntryQuantityFulfilled.add(new AtomicBoolean(false));
        }
      } else {
        allEntryQuantityFulfilled.add(new AtomicBoolean(true));
      }
    });

    return !allEntryQuantityFulfilled.isEmpty() && allEntryQuantityFulfilled.stream().allMatch(AtomicBoolean::get);
  }

  /**
   * Validating the rules and assign the serials.
   * @param context
   * @param results
   * @param warehouse
   * @param result
   * @param allEntrySourceComplete
   * @param entry
   * @param stocks
   */
  private void validateAllocationRulesAndAssignSerials(final SourcingContext context, final Set<SourcingResult> results,
      final WarehouseModel warehouse, final SourcingResult result, final List<AtomicBoolean> allEntrySourceComplete,
      final AbstractOrderEntryModel entry, final List<StockLevelModel> stocks, final Long quantity) {

    final List<String> serialProductCodes = stocks.stream().map(StockLevelModel::getSerialProductCode)
        .collect(Collectors.toList());

    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Stock size {} for product code {}",
        stocks.size(), entry.getProduct().getCode());

    Collection<BlSerialProductModel> allSerialProducts = getSessionService()
        .executeInLocalView(new SessionExecutionBody() {

          @Override
          public Collection<BlSerialProductModel> execute() {
            getSearchRestrictionService().disableSearchRestrictions();
            return blProductDao
                .getBlSerialProductsForCodes(new HashSet<>(serialProductCodes));
          }
        });

    final List<String> assignedSerials = new ArrayList<>();
    //proceed with below checks
    //1. get all bl consigners, and check quantity
    final Set<BlSerialProductModel> nonBufferProducts = getAllNonBufferProducts(allSerialProducts);
    final Set<BlSerialProductModel> bufferProducts = new HashSet<>(allSerialProducts);
    bufferProducts.removeAll(nonBufferProducts);
    if(CollectionUtils.isNotEmpty(nonBufferProducts)) {
      prioritizeSerialProducts(context, results, warehouse, result, allEntrySourceComplete, entry,
          quantity,
          nonBufferProducts, assignedSerials);
    }
    if(CollectionUtils.isNotEmpty(bufferProducts) &&
        !(allEntrySourceComplete.stream().allMatch(AtomicBoolean::get) && isAllQuantityFulfilled(context))) {
      prioritizeSerialProducts(context, results, warehouse, result, allEntrySourceComplete, entry,
          quantity,
          bufferProducts, assignedSerials);
    }
  }

  /**
   * It assigns the serial products as per the priority rule
   * @param context the context
   * @param results the results
   * @param warehouse the warehouse
   * @param result the result
   * @param allEntrySourceComplete flag to identify whether sourcing completed or not
   * @param entry the order entry
   * @param quantity the quantity
   * @param allSerialProducts all serial products
   * @param assignedSerials the assigned serials
   */
  public void prioritizeSerialProducts(final SourcingContext context, final Set<SourcingResult> results,
      final WarehouseModel warehouse, final SourcingResult result, final List<AtomicBoolean> allEntrySourceComplete,
      final AbstractOrderEntryModel entry, final Long quantity, final Collection<BlSerialProductModel> allSerialProducts,
      final List<String> assignedSerials) {
    final Set<BlSerialProductModel> blConsignerSerials = getAllBLConsignerSerials(allSerialProducts);
    if (blConsignerSerials.size() > 0 ) {

      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BL consigner serials found {}",
          blConsignerSerials.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));

      if (validateFulfilledQuantityAndAssignSerials(context, results, warehouse, result, entry,
          assignedSerials, blConsignerSerials, quantity) && isFulfilledQuantityEqualToEntryQuantity(entry, context, quantity)) {
        allEntrySourceComplete.add(new AtomicBoolean(true));
      } else {

        final Set<BlSerialProductModel> forSaleFalseSerials = getAllForSaleSerials(
            blConsignerSerials, false);
        final Set<BlSerialProductModel> unAssignedForSaleFalseSerials = new HashSet<>(
            getUnAssignedSerials(forSaleFalseSerials, assignedSerials));

        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Unassigned forSale False Serials found {}",
            unAssignedForSaleFalseSerials.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));

        if (unAssignedForSaleFalseSerials.size() != 0 && ((quantity - getFulfilledQuantity(context, entry)) < unAssignedForSaleFalseSerials.size()
            || validateFulfilledQuantityAndAssignSerials(context, results, warehouse, result, entry,
            assignedSerials, unAssignedForSaleFalseSerials, quantity))) {

          if (!isFulfilledQuantityEqualToEntryQuantity(entry, context, quantity)) {
            final List<BlSerialProductModel> oldestSerials = getOldestSerials(
                new ArrayList<>(unAssignedForSaleFalseSerials));

            getUnAssignedAndFilteredSerials(context, result, results, entry, warehouse,
                oldestSerials, quantity);
          }
          allEntrySourceComplete.add(new AtomicBoolean(true));
        } else {

          final Set<BlSerialProductModel> forSaleTrueSerials = getAllForSaleSerials(
              blConsignerSerials, true);
          final Set<BlSerialProductModel> unAssignedForSaleTrueSerials = new HashSet<>(
              getUnAssignedSerials(forSaleTrueSerials, assignedSerials));

          BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Unassigned forSale True Serials found {}",
              unAssignedForSaleTrueSerials.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));

          if (unAssignedForSaleTrueSerials.size() != 0 && ((quantity - getFulfilledQuantity(context, entry))  < unAssignedForSaleTrueSerials.size()
              || validateFulfilledQuantityAndAssignSerials(context, results, warehouse, result, entry,
              assignedSerials, unAssignedForSaleTrueSerials, quantity))) {

            if (!isFulfilledQuantityEqualToEntryQuantity(entry, context, quantity)) {

              final List<BlSerialProductModel> oldestSerials = getOldestSerials(
                  new ArrayList<>(unAssignedForSaleTrueSerials));

              getUnAssignedAndFilteredSerials(context, result, results, entry, warehouse,
                  oldestSerials, quantity);
            }
            allEntrySourceComplete.add(new AtomicBoolean(true));
          } else {

            final Set<BlSerialProductModel> nonSaleAndNonBLSerials = getAllNonSaleAndNonBLConsignerSerials(
                allSerialProducts);

            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Non-sale and non-BL Serials found {}",
                nonSaleAndNonBLSerials.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));

            if (CollectionUtils.isNotEmpty(nonSaleAndNonBLSerials)) {
              validateAndAssignNonBlSerials(context, results, warehouse, result,
                  allEntrySourceComplete, entry, nonSaleAndNonBLSerials, assignedSerials, new HashSet<>(allSerialProducts), quantity);
            }
          }
        }
      }

    } else {

      final Set<BlSerialProductModel> nonSaleAndNonBLSerials = getAllNonSaleAndNonBLConsignerSerials(
          allSerialProducts);

      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Non-sale and non-BL Serials found :: {}",
          nonSaleAndNonBLSerials.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));

      if (nonSaleAndNonBLSerials.size() > 0) {
        validateAndAssignNonBlSerials(context, results, warehouse, result, allEntrySourceComplete,
            entry,
            nonSaleAndNonBLSerials, assignedSerials, new HashSet<>(allSerialProducts), quantity);
      } else {
        final List<BlSerialProductModel> unAssignedSerials = new ArrayList<>(
            getUnAssignedSerials(allSerialProducts, assignedSerials));

        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "All unassigned Serials found {}",
            unAssignedSerials.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));

        boolean isEntrySourced = getUnAssignedAndFilteredSerials(context, result, results,
            entry, warehouse, unAssignedSerials, quantity);
        allEntrySourceComplete.add(new AtomicBoolean(isEntrySourced));
      }
    }
  }

  /**
   * It filters the non buffer products
   * @param allSerialProducts all serial products
   * @return set of bl serial product instance
   */
  private Set<BlSerialProductModel> getAllNonBufferProducts(final Collection<BlSerialProductModel> allSerialProducts) {
    return allSerialProducts.stream()
        .filter(serial -> BooleanUtils.isFalse(serial.getIsBufferedInventory()))
        .collect(Collectors.toSet());
  }

  private long getFulfilledQuantity(final SourcingContext context, final AbstractOrderEntryModel entry) {

    final long[] fulfilledQuantity = {0l};
    context.getResult().getResults().stream().forEach(result ->
      fulfilledQuantity[0] += (null != result.getSerialProductMap() && null != result.getSerialProductMap()
          .get(entry.getEntryNumber())) ? result.getSerialProductMap()
          .get(entry.getEntryNumber()).size() : 0
    );

    return fulfilledQuantity[0];
  }

  private void validateAndAssignNonBlSerials(final SourcingContext context,
      final Set<SourcingResult> results, final WarehouseModel warehouse,
      final SourcingResult result, final List<AtomicBoolean> allEntrySourceComplete,
      final AbstractOrderEntryModel entry, final Set<BlSerialProductModel> nonSaleAndNonBLSerials,
      final List<String> assignedSerials, final Set<BlSerialProductModel> allSerialProducts, final Long quantity) {

    if ((quantity - getFulfilledQuantity(context, entry)) < nonSaleAndNonBLSerials.size()
        || validateFulfilledQuantityAndAssignSerials(context, results, warehouse, result, entry,
        assignedSerials, nonSaleAndNonBLSerials, quantity)) {
      if (!isFulfilledQuantityEqualToEntryQuantity(entry, context, quantity)){
        final List<BlSerialProductModel> oldestSerials = getOldestSerials(
            new ArrayList<>(nonSaleAndNonBLSerials));
        getUnAssignedAndFilteredSerials(context, result, results, entry, warehouse,
            oldestSerials, quantity);
      }

      allEntrySourceComplete.add(new AtomicBoolean(true));
    } else {

      final List<BlSerialProductModel> unAssignedSerials = new ArrayList<>(
          getUnAssignedSerials(allSerialProducts, assignedSerials));
      boolean isEntrySourced = getUnAssignedAndFilteredSerials(context, result, results,
          entry, warehouse, unAssignedSerials, quantity);
      allEntrySourceComplete.add(new AtomicBoolean(isEntrySourced));
    }
  }

  /**
   * Validate the fulfilled quantity against the assigned serials.
   * @param context
   * @param results
   * @param warehouse
   * @param result
   * @param entry
   * @param assignedSerials
   * @param serialProducts
   * @return true if fulfilled quantity
   */
  private boolean validateFulfilledQuantityAndAssignSerials(final SourcingContext context,
      final Set<SourcingResult> results, final WarehouseModel warehouse,
      final SourcingResult result, final AbstractOrderEntryModel entry,
      final List<String> assignedSerials, final Set<BlSerialProductModel> serialProducts, final Long quantity) {

    final Set<BlSerialProductModel> unAssignedSerialProducts = serialProducts.stream()
        .filter(serialProduct -> !assignedSerials.contains(serialProduct.getCode())).collect(
            Collectors.toSet());
    if (CollectionUtils.isNotEmpty(unAssignedSerialProducts)) {

      return validateSerialsAndFulfilledQuantityWithOrderEntry(context, result, results, entry,
          warehouse, unAssignedSerialProducts, assignedSerials, quantity);
    } else {
      return false;
    }
  }

 /**
   * Validate the fulfilled quantity equality with the entry quantity.
   * @param entry
   * @param context
   * @return true if equal.
   */
  private boolean isFulfilledQuantityEqualToEntryQuantity(final AbstractOrderEntryModel entry,
      final SourcingContext context, final Long quantity) {

    return getFulfilledQuantity(context, entry) == quantity;
  }

  /**
   * Assign serials.
   *
   * @param context
   * @param result
   * @param results
   * @param entry
   * @param warehouse
   * @param serialProducts
   * @return
   */
  private boolean getUnAssignedAndFilteredSerials(final SourcingContext context,
      final SourcingResult result,
      final Set<SourcingResult> results, final AbstractOrderEntryModel entry,
      final WarehouseModel warehouse,
      final List<BlSerialProductModel> serialProducts, final Long quantity) {

    if (CollectionUtils.isNotEmpty(serialProducts) && serialProducts.size() >= (quantity
        - getFulfilledQuantity(context, entry))) {
      final Set<BlSerialProductModel> toAssign = new HashSet<>();
      for (int i = 0; i < (quantity - getFulfilledQuantity(context, entry)); i++) {
        toAssign.add(serialProducts.get(i));
      }

      createResultAndAssignSerials(context, result, results, entry, warehouse, toAssign);
      return isFulfilledQuantityEqualToEntryQuantity(entry, context, quantity);
    } else {
      createResultAndAssignSerials(context, result, results, entry, warehouse, Sets.newHashSet(serialProducts));
      return isFulfilledQuantityEqualToEntryQuantity(entry, context, quantity);
    }
  }

  /**
   * Get un-assigned serials.
   * @param allSerialProducts
   * @param assignedSerials
   * @return set of un-assigned serials.
   */
  private Set<BlSerialProductModel> getUnAssignedSerials(
      final Collection<BlSerialProductModel> allSerialProducts,
      final List<String> assignedSerials) {

    if (CollectionUtils.isNotEmpty(assignedSerials)) {

      return allSerialProducts.stream()
          .filter(serialProduct -> !assignedSerials.contains(serialProduct.getCode()))
          .collect(Collectors.toSet());
    } else {

      return new HashSet<>(allSerialProducts);
    }
  }

  /**
   * Create sourcing result and assign serials.
   * @param context
   * @param result
   * @param results
   * @param entry
   * @param warehouse
   * @param serialProducts
   */
  private void createResultAndAssignSerials(final SourcingContext context, final SourcingResult result,
      final Set<SourcingResult> results, final AbstractOrderEntryModel entry, final WarehouseModel warehouse,
      final Set<BlSerialProductModel> serialProducts) {

    final Set<BlSerialProductModel> serialProductsToAssign = new HashSet<>(
        (null != result.getSerialProductMap() && null != result.getSerialProductMap()
            .get(entry.getEntryNumber())) ? result.getSerialProductMap().get(entry.getEntryNumber())
            : Collections.emptySet());
    final Set<BlSerialProductModel> serialProductSet = serialProducts.stream()
        .filter(product -> !serialProductsToAssign.contains(product))
        .collect(Collectors.toSet());
    serialProductsToAssign.addAll(serialProductSet);

    final Map<AbstractOrderEntryModel, Long> allocationMap =
        MapUtils.isNotEmpty(result.getAllocation()) ? result.getAllocation() : new HashMap<>();
    allocationMap.put(entry, (long) serialProductsToAssign.size());

    final Set<BlProductModel> entrySerialProducts = new HashSet<>(entry.getSerialProducts());
    entrySerialProducts.addAll(serialProductsToAssign);
    entry.setSerialProducts(new ArrayList<>(entrySerialProducts));

    final Map<Integer, Set<BlSerialProductModel>> serialProductMap =
        MapUtils.isNotEmpty(result.getSerialProductMap()) ? result.getSerialProductMap()
            : new HashMap<>();
    serialProductMap.put(entry.getEntryNumber(), serialProductsToAssign);

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Serial products {} are assigned for product code {}",
        serialProductMap.get(entry.getEntryNumber()), entry.getProduct().getCode());

    result.setSerialProductMap(serialProductMap);
    result.setAllocation(allocationMap);
    result.setWarehouse(warehouse);
    results.add(result);
    context.getResult().setResults(results);

  }

  /**
   * Validate fulfilled quantity and order entry quantity.
   * @param context
   * @param result
   * @param results
   * @param entry
   * @param warehouse
   * @param serialProducts
   * @param assignedSerials
   * @return true if quantities are equal.
   */
  private boolean validateSerialsAndFulfilledQuantityWithOrderEntry(final SourcingContext context, final SourcingResult result,
      final Set<SourcingResult> results, final AbstractOrderEntryModel entry, final WarehouseModel warehouse,
      final Set<BlSerialProductModel> serialProducts, final List<String> assignedSerials, final Long quantity) {

    if (serialProducts.size() == (quantity - getFulfilledQuantity(context, entry)) && getFulfilledQuantity(context, entry) <= quantity) {
      createResultAndAssignSerials(context, result, results, entry, warehouse, serialProducts);
      assignedSerials
          .addAll(serialProducts.stream().map(BlSerialProductModel :: getCode).collect(Collectors.toList()));
      return isFulfilledQuantityEqualToEntryQuantity(entry, context, quantity);
    }

    if (serialProducts.size() < (quantity - getFulfilledQuantity(context, entry)) && getFulfilledQuantity(context, entry) <= quantity) {
      createResultAndAssignSerials(context, result, results, entry, warehouse, serialProducts);
      assignedSerials
          .addAll(serialProducts.stream().map(BlSerialProductModel :: getCode).collect(Collectors.toList()));
      return isFulfilledQuantityEqualToEntryQuantity(entry, context, quantity);
    } else {

      return false;
    }
  }

  /**
   * Get all non-sale and non-BL consigner serials.
   * @param serialProducts
   * @return set of serials.
   */
  private Set<BlSerialProductModel> getAllNonSaleAndNonBLConsignerSerials(
      final Collection<BlSerialProductModel> serialProducts) {

    return serialProducts.stream()
        .filter(serial -> !"BL".equalsIgnoreCase(serial.getOwnedBy()))
        .filter(serial -> !serial.getForSale())
        .collect(Collectors.toSet());
  }

  /**
   * Get list of oldest serials.
   * @param serialProducts
   * @return list of oldest serials.
   */
  private List<BlSerialProductModel> getOldestSerials(final List<BlSerialProductModel> serialProducts) {

    //set 0 for null values of no of days rented attribute of serial product
    serialProducts.forEach(serialProduct -> {
      if (null == serialProduct.getNoDaysRented()) {
        serialProduct.setNoDaysRented(0l);
      }
    });
    serialProducts.sort(Comparator.comparing(BlSerialProductModel::getNoDaysRented));
    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Sorted serial products with oldest no of days rented {}",
        serialProducts.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));
    return serialProducts;
  }

  /**
   * Get all sale-able serials.
   * @param consignerSerials
   * @param forSale
   * @return set of serials.
   */
  private Set<BlSerialProductModel> getAllForSaleSerials(
      final Set<BlSerialProductModel> consignerSerials,
      boolean forSale) {

    return consignerSerials.stream().filter(serial -> serial.getForSale() == forSale)
        .collect(Collectors.toSet());
  }

  /**
   * Get all BL consigner serials.
   * @param allSerialProducts
   * @return BL consigner serials.
   */
  private Set<BlSerialProductModel> getAllBLConsignerSerials(
      final Collection<BlSerialProductModel> allSerialProducts) {

    return allSerialProducts.stream().filter(serial -> "BL".equalsIgnoreCase(serial.getOwnedBy()))
        .collect(Collectors.toSet());
  }

  public BlProductDao getBlProductDao() {
    return blProductDao;
  }

  public void setBlProductDao(final BlProductDao blProductDao) {
    this.blProductDao = blProductDao;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }

  public SearchRestrictionService getSearchRestrictionService() {
    return searchRestrictionService;
  }

  public void setSearchRestrictionService(
      final SearchRestrictionService searchRestrictionService) {
    this.searchRestrictionService = searchRestrictionService;
  }

  public SessionService getSessionService() {
    return sessionService;
  }

  public void setSessionService(final SessionService sessionService) {
    this.sessionService = sessionService;
  }

  public BlShippingOptimizationStrategy getBlShippingOptimizationStrategy() {
      return blShippingOptimizationStrategy;
  }

  public void setBlShippingOptimizationStrategy(BlShippingOptimizationStrategy blShippingOptimizationStrategy) {
      this.blShippingOptimizationStrategy = blShippingOptimizationStrategy;
  }

  public BlDeliveryModeService getZoneDeliveryModeService() {
    return zoneDeliveryModeService;
  }

  public void setZoneDeliveryModeService(
      final BlDeliveryModeService zoneDeliveryModeService) {
    this.zoneDeliveryModeService = zoneDeliveryModeService;
  }

  public BlDeliveryStateSourcingLocationFilter getBlDeliveryStateSourcingLocationFilter() {
    return blDeliveryStateSourcingLocationFilter;
  }

  public void setBlDeliveryStateSourcingLocationFilter(
      final BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter) {
    this.blDeliveryStateSourcingLocationFilter = blDeliveryStateSourcingLocationFilter;
  }
}
