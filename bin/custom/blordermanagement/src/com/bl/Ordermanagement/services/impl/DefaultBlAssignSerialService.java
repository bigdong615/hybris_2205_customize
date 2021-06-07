package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is used to assign serial products to sourcing results in context.
 *
 * @author Sunil
 */
public class DefaultBlAssignSerialService implements BlAssignSerialService {

  private static final Logger LOG = Logger.getLogger(DefaultBlAssignSerialService.class);
  private BlProductDao blProductDao;
  private ModelService modelService;

  /**
   * {@inheritDoc}
   */
  public boolean assignSerialsFromLocation(final SourcingContext context,
      final SourcingLocation sourcingLocation) throws BlSourcingException {

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Assigning serials from warehouse {}",
        sourcingLocation.getWarehouse().getCode());
    final List<AtomicBoolean> allEntrySourceComplete = new ArrayList<>();

    context.getOrderEntries().forEach(entry -> {

      final List<StockLevelModel> stocks = sourcingLocation.getAvailabilityMap()
          .get(entry.getProduct().getCode());

      if (CollectionUtils.isNotEmpty(stocks)) {
        validateAllocationRulesAndAssignSerials(context, context.getResult().getResults(),
            sourcingLocation.getWarehouse(), new SourcingResult(), allEntrySourceComplete, entry,
            stocks);
      } else {
        allEntrySourceComplete.add(new AtomicBoolean(false));
      }
    });

    return allEntrySourceComplete.stream().allMatch(AtomicBoolean::get);
  }

  private void validateAllocationRulesAndAssignSerials(final SourcingContext context, final Set<SourcingResult> results,
      final WarehouseModel warehouse, final SourcingResult result, final List<AtomicBoolean> allEntrySourceComplete,
      final AbstractOrderEntryModel entry, final List<StockLevelModel> stocks) {

    final List<String> serialProductCodes = stocks.stream().map(StockLevelModel::getSerialProductCode)
        .collect(Collectors.toList());

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock size {} for product code {}",
        stocks.size(), entry.getProduct().getCode());

    final Collection<BlSerialProductModel> allSerialProducts = blProductDao
        .getBlSerialProductsForCodes(new HashSet<>(serialProductCodes));

    final List<String> assignedSerials = new ArrayList<>();
    //proceed with below checks
    //1. get all bl consigners, and check quantity
    final Set<BlSerialProductModel> blConsignerSerials = getAllBLConsignerSerials(allSerialProducts);
    long fulfilledQuantity = (null != result.getSerialProductMap()) ? result.getSerialProductMap().get(entry.getEntryNumber()).size() : 0;

    if (validateFulfilledQuantityAndAssignSerials(context, results, warehouse, result, entry,
        assignedSerials, blConsignerSerials)) {
      allEntrySourceComplete.add(new AtomicBoolean(true));
    } else {

      final Set<BlSerialProductModel> forSaleFalseSerials = getAllForSaleSerials(
          blConsignerSerials, false);
      if (validateFulfilledQuantityAndAssignSerials(context, results, warehouse, result, entry,
          assignedSerials, forSaleFalseSerials)) {
        allEntrySourceComplete.add(new AtomicBoolean(true));
      } else {

        final Set<BlSerialProductModel> forSaleTrueSerials = getAllForSaleSerials(
            blConsignerSerials, true);
        if (validateFulfilledQuantityAndAssignSerials(context, results, warehouse, result, entry,
            assignedSerials, forSaleTrueSerials)) {
          allEntrySourceComplete.add(new AtomicBoolean(true));
        } else {

          final Set<BlSerialProductModel> nonSaleAndNonBLSerials = getAllNonSaleAndNonBLConsignerSerials(
              allSerialProducts);
          if (validateFulfilledQuantityAndAssignSerials(context, results, warehouse, result, entry,
              assignedSerials, nonSaleAndNonBLSerials)) {
            allEntrySourceComplete.add(new AtomicBoolean(true));
          } else {

            final List<BlSerialProductModel> unAssignedSerials = new ArrayList<>(
                getUnAssignedSerials(allSerialProducts, assignedSerials));
            boolean isEntrySourced = getUnAssignedAndFilteredSerials(context, result, results,
                entry, warehouse, unAssignedSerials, fulfilledQuantity);
            allEntrySourceComplete.add(new AtomicBoolean(isEntrySourced));
          }
        }
      }
    }
  }

  private boolean validateFulfilledQuantityAndAssignSerials(final SourcingContext context,
      final Set<SourcingResult> results, final WarehouseModel warehouse,
      final SourcingResult result, final AbstractOrderEntryModel entry,
      final List<String> assignedSerials, final Set<BlSerialProductModel> blConsignerSerials) {

    return validateSerialsAndFulfilledQuantityWithOrderEntry(context, result, results, entry, warehouse, blConsignerSerials,
        assignedSerials) && isFulfilledQuantityEqualToEntryQuantity(entry, result);
  }

  private boolean isFulfilledQuantityEqualToEntryQuantity(final AbstractOrderEntryModel entry,
      final SourcingResult result) {

    long fulfilledQuantity = (null != result.getSerialProductMap()) ? result.getSerialProductMap().get(entry.getEntryNumber()).size() : 0;
    return fulfilledQuantity == entry.getQuantity();
  }

  private boolean getUnAssignedAndFilteredSerials(final SourcingContext context, final SourcingResult result,
      final Set<SourcingResult> results, final AbstractOrderEntryModel entry, final WarehouseModel warehouse,
      final List<BlSerialProductModel> serialProducts, final long fulfilledQuantity) {

    if (CollectionUtils.isNotEmpty(serialProducts) && serialProducts.size() > fulfilledQuantity) {
      final Set<BlSerialProductModel> toAssign = new HashSet<>();
      for (int i = 0; i < (entry.getQuantity() - fulfilledQuantity); i++) {
        toAssign.add(serialProducts.get(i));
      }

      createResultAndAssignSerials(context, result, results, entry, warehouse, toAssign);
      return isFulfilledQuantityEqualToEntryQuantity(entry, result);
    } else {

      final AbstractOrderModel orderModel = entry.getOrder();
      orderModel.setStatus(OrderStatus.SUSPENDED);
      modelService.save(orderModel);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "All products can not be sourced.");
      throw new BlSourcingException("All products can not be sourced.");
    }
  }

  private Set<BlSerialProductModel> getUnAssignedSerials(
      final Collection<BlSerialProductModel> allSerialProducts,
      final List<String> assignedSerials) {

    return allSerialProducts.stream()
        .filter(serialProducts -> assignedSerials.stream().anyMatch(s -> !s.equalsIgnoreCase(serialProducts.getCode())))
        .collect(Collectors.toSet());
  }

  private void createResultAndAssignSerials(final SourcingContext context, final SourcingResult result,
      final Set<SourcingResult> results, final AbstractOrderEntryModel entry, final WarehouseModel warehouse,
      final Set<BlSerialProductModel> serialProducts) {

    final Map<AbstractOrderEntryModel, Long> allocationMap =
        MapUtils.isNotEmpty(result.getAllocation()) ? result.getAllocation() : new HashMap<>();
    allocationMap.put(entry, (long) serialProducts.size());

    final Set<String> serialProductCodesToAssign = new HashSet<>(entry.getSerialProductCodes());
    final Set<String> serialProductCodes = serialProducts.stream().map(BlSerialProductModel::getCode)
        .collect(Collectors.toSet());
    serialProductCodesToAssign.addAll(serialProductCodes);
    entry.setSerialProductCodes(serialProductCodesToAssign);

    final Map<Integer, Set<String>> serialProductMap =
        MapUtils.isNotEmpty(result.getSerialProductMap()) ? result.getSerialProductMap()
            : new HashMap<>();
    serialProductMap.put(entry.getEntryNumber(), serialProductCodes);

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "Serial products {} are assigned for product code {}",
        serialProductMap.get(entry.getEntryNumber()), entry.getProduct().getCode());

    result.setSerialProductMap(serialProductMap);
    result.setAllocation(allocationMap);
    result.setWarehouse(warehouse);
    results.add(result);
    context.getResult().setResults(results);
  }

  private boolean validateSerialsAndFulfilledQuantityWithOrderEntry(final SourcingContext context, final SourcingResult result,
      final Set<SourcingResult> results, final AbstractOrderEntryModel entry, final WarehouseModel warehouse,
      final Set<BlSerialProductModel> serialProducts, final List<String> assignedSerials) {

    long fulfilledQuantity = (null != result.getSerialProductMap()) ? result.getSerialProductMap().get(entry.getEntryNumber()).size() : 0;
    if (serialProducts.size() == entry.getQuantity() && fulfilledQuantity <= entry.getQuantity()) {
      createResultAndAssignSerials(context, result, results, entry, warehouse, serialProducts);
      assignedSerials
          .addAll(serialProducts.stream().map(BlSerialProductModel :: getCode).collect(Collectors.toList()));
      return true;
    }

    if (serialProducts.size() < entry.getQuantity() && fulfilledQuantity <= entry.getQuantity()) {
      createResultAndAssignSerials(context, result, results, entry, warehouse, serialProducts);
      assignedSerials
          .addAll(serialProducts.stream().map(BlSerialProductModel :: getCode).collect(Collectors.toList()));
      return false;
    } else {

      final List<BlSerialProductModel> oldestSerials = getOldestSerials(new ArrayList<>(serialProducts));
      return getUnAssignedAndFilteredSerials(context, result, results, entry, warehouse,
          oldestSerials, fulfilledQuantity);
    }
  }

  private Set<BlSerialProductModel> getAllNonSaleAndNonBLConsignerSerials(
      final Collection<BlSerialProductModel> serialProducts) {

    return serialProducts.stream()
        .filter(serial -> !serial.getOwnedBy().equals("BL") && !serial.getForSale())
        .collect(Collectors.toSet());
  }

  private List<BlSerialProductModel> getOldestSerials(final List<BlSerialProductModel> serialProducts) {

    //set 0 for null values of no of days rented attribute of serial product
    serialProducts.forEach(serialProduct -> {
      if (null == serialProduct.getNoDaysRented()) {
        serialProduct.setNoDaysRented(0l);
      }
    });
    serialProducts.sort(Comparator.comparing(BlSerialProductModel::getNoDaysRented));
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "Sorted serial products with oldest no of days rented {}",
        serialProducts.stream().map(BlSerialProductModel::getCode).collect(Collectors.toList()));
    return serialProducts;
  }

  private Set<BlSerialProductModel> getAllForSaleSerials(
      final Set<BlSerialProductModel> consignerSerials,
      boolean forSale) {

    return consignerSerials.stream().filter(serial -> serial.getForSale() == forSale)
        .collect(Collectors.toSet());
  }

  private Set<BlSerialProductModel> getAllBLConsignerSerials(
      final Collection<BlSerialProductModel> allSerialProducts) {

    return allSerialProducts.stream().filter(serial -> serial.getOwnedBy().equalsIgnoreCase("BL"))
        .collect(Collectors.toSet());
  }

  public BlProductDao getBlProductDao() {
    return blProductDao;
  }

  public void setBlProductDao(BlProductDao blProductDao) {
    this.blProductDao = blProductDao;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

}
