package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
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

  private static final Logger LOG = Logger
      .getLogger(DefaultBlAssignSerialService.class);
  private BlProductDao blProductDao;
  private ModelService modelService;

  /**
   * {@inheritDoc}
   */
  public boolean assignSerialsFromLocation(SourcingContext context,
      SourcingLocation sourcingLocation) throws BlSourcingException {
    Map<String, List<StockLevelModel>> availabilityMap = sourcingLocation
        .getAvailabilityMap();
    Set<SourcingResult> results = context.getResult().getResults();
    WarehouseModel warehouse = sourcingLocation.getWarehouse();
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Assigning serials from warehouse {}",
        warehouse.getCode());
    SourcingResult result = new SourcingResult();
    AtomicBoolean sourceComplete = new AtomicBoolean(false);
    List<AtomicBoolean> allEntrySourceComplete = new ArrayList<>();
    context.getOrderEntries().forEach(entry -> {
      List<StockLevelModel> stocks = availabilityMap.get(entry.getProduct().getCode());

      if (CollectionUtils.isNotEmpty(stocks)) {
        List<String> serialProductCodes = stocks.stream().map(stock -> stock.getSerialProductCode())
            .collect(
                Collectors.toList());
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock size {} for product code {}",
            stocks.size(), entry.getProduct().getCode());

        Collection<BlSerialProductModel> allSerialProducts = blProductDao
            .getBlSerialProductsForCodes(new HashSet<>(serialProductCodes));
        List<String> assignedSerials = new ArrayList<>();
        //proceed with below checks
        //1. get all bl consigners, and check quantity
        Set<BlSerialProductModel> blConsignerSerials = getAllBLConsignerSerials(allSerialProducts);
        Long fulfilledQuantity = 0L;

        if (sizeEqualToQuantity(context, result, results, entry, warehouse, blConsignerSerials,
            fulfilledQuantity, assignedSerials)) {
          allEntrySourceComplete.add(new AtomicBoolean(true));
        } else {
          Set<BlSerialProductModel> forSaleFalseSerials = getAllForSaleSerials(
              blConsignerSerials, false);
          if (sizeEqualToQuantity(context, result, results, entry, warehouse, forSaleFalseSerials,
              fulfilledQuantity, assignedSerials)) {
            allEntrySourceComplete.add(new AtomicBoolean(true));
          } else {
            Set<BlSerialProductModel> forSaleTrueSerials = getAllForSaleSerials(
                blConsignerSerials, true);
            if (sizeEqualToQuantity(context, result, results, entry, warehouse, forSaleTrueSerials,
                fulfilledQuantity, assignedSerials) && fulfilledQuantity == entry.getQuantity()) {
              allEntrySourceComplete.add(new AtomicBoolean(true));
            } else {
              Set<BlSerialProductModel> nonSaleAndNonBLSerials = getAllNonSaleAndNonBLConsignerSerials(
                  allSerialProducts);
              if (sizeEqualToQuantity(context, result, results, entry, warehouse,
                  nonSaleAndNonBLSerials,
                  fulfilledQuantity, assignedSerials) && fulfilledQuantity == entry.getQuantity()) {
                allEntrySourceComplete.add(new AtomicBoolean(true));
              } else {
                List<BlSerialProductModel> unAssignedSerials = new ArrayList<BlSerialProductModel>(
                    getUnAssignedSerials(allSerialProducts, assignedSerials));
                boolean isEntrySourced = getUnAssignedAndFilteredSerials(context, result, results,
                    entry, warehouse,
                    unAssignedSerials, fulfilledQuantity);
                allEntrySourceComplete.add(new AtomicBoolean(isEntrySourced));
              }
            }
          }
        }
      } else {
        allEntrySourceComplete.add(new AtomicBoolean(false));
      }

    });

    return allEntrySourceComplete.stream()
        .allMatch(isSourceComplete -> isSourceComplete.get() == true);
  }

  private boolean getUnAssignedAndFilteredSerials(SourcingContext context, SourcingResult result,
      Set<SourcingResult> results, AbstractOrderEntryModel entry, WarehouseModel warehouse,
      List<BlSerialProductModel> serialProducts, Long fulfilledQuantity) {
    if (CollectionUtils.isNotEmpty(serialProducts) && serialProducts.size() > fulfilledQuantity) {
      Set<BlSerialProductModel> toAssign = new HashSet<>();
      for (int i = 0; i < (entry.getQuantity() - fulfilledQuantity); i++) {
        toAssign.add(serialProducts.get(i));
      }
      createResultAndAssignSerials(context, result, results, entry, warehouse, toAssign);
      fulfilledQuantity += Long.valueOf(toAssign.size());
      return fulfilledQuantity == entry.getQuantity() ? true : false;
    } else {
      entry.getOrder().setStatus(OrderStatus.SUSPENDED);
      modelService.save(entry.getOrder());
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "All products can not be sourced.");
      throw new BlSourcingException("All products can not be sourced.");
    }
  }

  private Set<BlSerialProductModel> getUnAssignedSerials(
      Collection<BlSerialProductModel> allSerialProducts, List<String> assignedSerials) {
    return allSerialProducts.stream()
        .filter(p -> assignedSerials.stream().anyMatch(s -> !s.equalsIgnoreCase(p.getCode())))
        .collect(Collectors.toSet());
  }

  private void createResultAndAssignSerials(SourcingContext context, SourcingResult result,
      Set<SourcingResult> results, AbstractOrderEntryModel entry, WarehouseModel warehouse,
      Set<BlSerialProductModel> serialProducts) {
    Map<AbstractOrderEntryModel, Long> allocation =
        MapUtils.isNotEmpty(result.getAllocation()) ? result.getAllocation()
            : new HashMap<AbstractOrderEntryModel, Long>();
    allocation.put(entry, Long.valueOf(serialProducts.size()));

    Set<String> serialProductCodesToAssign = new HashSet<String>();
    serialProductCodesToAssign.addAll(entry.getSerialProductCodes());
    Set<String> serialProductCodes = serialProducts.stream().map(serial -> serial.getCode())
        .collect(
            Collectors.toSet());
    serialProductCodesToAssign.addAll(serialProductCodes);
    entry.setSerialProductCodes(serialProductCodesToAssign);

    Map<Integer, Set<String>> serialProductMap =
        MapUtils.isNotEmpty(result.getSerialProductMap()) ? result.getSerialProductMap()
            : new HashMap<Integer, Set<String>>();
    serialProductMap.put(entry.getEntryNumber(), serialProductCodes);

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "Serial products {} are assigned for product code {}",
        serialProductMap.get(entry.getEntryNumber()), entry.getProduct().getCode());

    result.setSerialProductMap(serialProductMap);
    result.setAllocation(allocation);
    result.setWarehouse(warehouse);
    results.add(result);
    context.getResult().setResults(results);
  }

  private boolean sizeEqualToQuantity(SourcingContext context, SourcingResult result,
      Set<SourcingResult> results, AbstractOrderEntryModel entry, WarehouseModel warehouse,
      Set<BlSerialProductModel> serialProducts, Long fulfilledQuantity,
      List<String> assignedSerials) {
    if (serialProducts.size() == entry.getQuantity() && fulfilledQuantity <= entry.getQuantity()) {
      createResultAndAssignSerials(context, result, results, entry, warehouse, serialProducts);
      fulfilledQuantity += Long.valueOf(serialProducts.size());
      assignedSerials
          .addAll(serialProducts.stream().map(p -> p.getCode()).collect(Collectors.toList()));
      return true;
    }

    if (serialProducts.size() < entry.getQuantity() && fulfilledQuantity <= entry.getQuantity()) {
      createResultAndAssignSerials(context, result, results, entry, warehouse, serialProducts);
      fulfilledQuantity += Long.valueOf(serialProducts.size());
      assignedSerials
          .addAll(serialProducts.stream().map(p -> p.getCode()).collect(Collectors.toList()));
      return false;
    } else {
      List<BlSerialProductModel> oldestSerials = getOldestSerials(new ArrayList<>(serialProducts));
      return getUnAssignedAndFilteredSerials(context, result, results, entry, warehouse,
          oldestSerials, fulfilledQuantity);
    }
  }

  private Set<BlSerialProductModel> getAllNonSaleAndNonBLConsignerSerials(
      Collection<BlSerialProductModel> serialProducts) {
    return serialProducts.stream()
        .filter(serial -> serial.getOwnedBy() != "BL" && serial.getForSale() == false).collect(
            Collectors.toSet());
  }

  private List<BlSerialProductModel> getOldestSerials(List<BlSerialProductModel> serialProducts) {
    //set 0 for null values of no of days rented attribute of serial product
    serialProducts.stream().forEach(sp -> {
      if (null == sp.getNoDaysRented()) {
        sp.setNoDaysRented(0l);
      }
    });
    Collections.sort(serialProducts, Comparator.comparing(BlSerialProductModel::getNoDaysRented));
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "Sorted serial products with oldest no of days rented {}",
        serialProducts.stream().map(p -> p.getCode()).collect(Collectors.toList()));
    return serialProducts;
  }

  private Set<BlSerialProductModel> getAllForSaleSerials(Set<BlSerialProductModel> consignerSerials,
      boolean forSale) {
    return consignerSerials.stream().filter(serial -> serial.getForSale() == forSale)
        .collect(Collectors.toSet());
  }

  private Set<BlSerialProductModel> getAllBLConsignerSerials(
      Collection<BlSerialProductModel> allSerialProducts) {
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
