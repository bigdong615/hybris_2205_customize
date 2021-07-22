package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.exceptions.BlShippingOptimizationException;
import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.shipping.strategy.BlShippingOptimizationStrategy;
import com.bl.core.stock.BlStockLevelDao;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.MapUtils;
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

    if (MapUtils.isEmpty(result.getAllocation()) || MapUtils
        .isEmpty(result.getSerialProductMap())) {
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
      consignment.setShippingAddress(order.getDeliveryAddress());
      consignment
          .setShippingDate(this.getShippingDateStrategy().getExpectedShippingDate(consignment));

      consignment.setFulfillmentSystemConfig(
          this.getWarehousingFulfillmentConfigDao().getConfiguration(result.getWarehouse()));
      final Set<Entry<AbstractOrderEntryModel, Long>> resultEntries = result.getAllocation()
          .entrySet();
//      Optional<PointOfServiceModel> pickupPos = resultEntries.stream().map(entry ->
//          ((AbstractOrderEntryModel) entry.getKey()).getDeliveryPointOfService()
//      ).filter(Objects::nonNull).findFirst();

//      if (pickupPos.isPresent()) {
//        consignment.setStatus(ConsignmentStatus.READY);
//        consignment.setDeliveryMode(this.getDeliveryModeService().getDeliveryModeForCode("pickup"));
//        consignment.setShippingAddress(((PointOfServiceModel) pickupPos.get()).getAddress());
//        consignment.setDeliveryPointOfService(pickupPos.get());
//      }

      final Set<ConsignmentEntryModel> entries = resultEntries.stream().map(mapEntry ->
          this.createConsignmentEntry(mapEntry.getKey(), mapEntry.getValue(), consignment, result)
      ).collect(Collectors.toSet());
      consignment.setConsignmentEntries(entries);
      consignment.setWarehouse(result.getWarehouse());
      if (consignment.getFulfillmentSystemConfig() == null) {
        this.getWarehousingConsignmentWorkflowService().startConsignmentWorkflow(consignment);
      }

//      if (!consignment.getWarehouse().isExternal()) {
//        this.getInventoryEventService().createAllocationEvents(consignment);
//      }

      final List<String> allocatedProductCodes = new ArrayList<>();
      for (Set<BlSerialProductModel> productSet : result.getSerialProductMap().values()) {
        allocatedProductCodes.addAll(productSet.stream().map(BlSerialProductModel::getCode).collect(
            Collectors.toSet()));
      }

      final Collection<StockLevelModel> serialStocks = getSerialsForDateAndCodes(order,
              new HashSet<>(allocatedProductCodes));

      if ((!serialStocks.isEmpty()) && serialStocks.stream()
          .allMatch(stock -> allocatedProductCodes.contains(stock.getSerialProductCode()))) {
        this.optimizeShippingMethodForConsignment(consignment, result);
        this.getModelService().save(consignment);
        serialStocks.forEach(stock -> stock.setReservedStatus(true));
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

    } catch (final Exception ex) {
      throw new BlSourcingException(ERROR_WHILE_ALLOCATING_THE_ORDER, ex);
    }
  }

  /**
   * javadoc
   * this method will call optimization flow for consignment
   *
   * @param consignment model
   * @param result sourcingResult
   */
  private void optimizeShippingMethodForConsignment(final ConsignmentModel consignment, final SourcingResult result) {
    try {
      consignment.setThreeDayGroundAvailability(result.isThreeDayGroundAvailability());
      getBlShippingOptimizationStrategy().getOptimizedShippingMethodForOrder(consignment);
    } catch (final Exception e) {
       throw new BlShippingOptimizationException(ERROR_WHILE_OPTIMIZING_THE_ORDER, e);
    }
  }

  private Collection<StockLevelModel> getSerialsForDateAndCodes(final AbstractOrderModel order,
      final Set<String> serialProductCodes) {

    return blStockLevelDao
        .findSerialStockLevelsForDateAndCodes(serialProductCodes, order.getActualRentalStartDate(),
            order.getActualRentalEndDate());
  }

  /**
   * Create consignment entry.
   * @param orderEntry
   * @param quantity
   * @param consignment
   * @param result
   * @return consignment entry
   */
  protected ConsignmentEntryModel createConsignmentEntry(final AbstractOrderEntryModel orderEntry,
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
    final Set<BlSerialProductModel> serialProductModels = result.getSerialProductMap().get(orderEntry.getEntryNumber());
    entry.setSerialProducts(new ArrayList<>(serialProductModels));   //setting serial products from result

    setItemsMap(entry, serialProductModels);

    final Set<ConsignmentEntryModel> consignmentEntries = new HashSet<>();
    if (orderEntry.getConsignmentEntries() != null) {
      orderEntry.getConsignmentEntries().forEach(consignmentEntries::add);
    }

    consignmentEntries.add(entry);
    orderEntry.setConsignmentEntries(consignmentEntries);
    return entry;
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
    serialProductModels.forEach(serial -> {

      itemsMap.put(serial.getCode(), ItemStatusEnum.NOT_INCLUDED);

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "Serial product with code {} added to the products list on consignment entry.",
          serial.getCode());

      final List<BlProductModel> subPartProducts = getSessionService()
          .executeInLocalView(new SessionExecutionBody() {
            @Override
            public List<BlProductModel> execute() {
              getSearchRestrictionService().disableSearchRestrictions();
              if (null != serial.getBlProduct()) {

                return (List<BlProductModel>) serial.getBlProduct().getSubParts();
              }
              return new ArrayList<>();
            }
          });

      subPartProducts.forEach(product -> {

        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "Sub part with code {} and quantity {} added to the products list on consignment entry.",
            product.getCode(), product.getSubpartQuantity());

        for (int i = 1; i <= product.getSubpartQuantity(); i++) {
          entry.getSerialProducts().add(product);
          itemsMap.put(product.getName() + BlCoreConstants.HYPHEN + i, ItemStatusEnum.NOT_INCLUDED);

          BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
              "Sub part with name {} added to the products list on consignment entry.",
              product.getName());
        }
      });

    });

    entry.setItems(itemsMap);
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
}
