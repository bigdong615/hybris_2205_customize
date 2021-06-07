package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.google.common.base.Strings;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.storelocator.model.PointOfServiceModel;
import de.hybris.platform.warehousing.allocation.impl.DefaultAllocationService;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
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

  private static final Logger LOG = Logger
      .getLogger(DefaultBlAllocationService.class);
  private BlStockLevelDao blStockLevelDao;

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

    ServicesUtil.validateParameterNotNullStandardMessage("result", result);
    ServicesUtil.validateParameterNotNullStandardMessage("order", order);
    Assert.isTrue(!Strings.isNullOrEmpty(code), "Parameter code cannot be null or empty");
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Creating consignment for Location: '{}'",
        result.getWarehouse().getCode());
    final ConsignmentModel consignment = (ConsignmentModel) this.getModelService()
        .create(ConsignmentModel.class);
    consignment.setCode(code);
    consignment.setOrder(order);

    try {
      consignment.setFulfillmentSystemConfig(
          this.getWarehousingFulfillmentConfigDao().getConfiguration(result.getWarehouse()));
      final Set<Entry<AbstractOrderEntryModel, Long>> resultEntries = result.getAllocation().entrySet();
      Optional<PointOfServiceModel> pickupPos = resultEntries.stream().map(entry ->
          ((AbstractOrderEntryModel) entry.getKey()).getDeliveryPointOfService()
      ).filter(Objects::nonNull).findFirst();

      if (pickupPos.isPresent()) {
        consignment.setStatus(ConsignmentStatus.READY);
        consignment.setDeliveryMode(this.getDeliveryModeService().getDeliveryModeForCode("pickup"));
        consignment.setShippingAddress(((PointOfServiceModel) pickupPos.get()).getAddress());
        consignment.setDeliveryPointOfService(pickupPos.get());
      } else {
        consignment.setStatus(ConsignmentStatus.READY);
        consignment.setDeliveryMode(order.getDeliveryMode());
        consignment.setShippingAddress(order.getDeliveryAddress());
        consignment
            .setShippingDate(this.getShippingDateStrategy().getExpectedShippingDate(consignment));
      }

      final Set<ConsignmentEntryModel> entries = resultEntries.stream().map(mapEntry ->
          this.createConsignmentEntry(mapEntry.getKey(), mapEntry.getValue(), consignment, result)
      ).collect(Collectors.toSet());
      consignment.setConsignmentEntries(entries);
      consignment.setWarehouse(result.getWarehouse());
      if (consignment.getFulfillmentSystemConfig() == null) {
        this.getWarehousingConsignmentWorkflowService().startConsignmentWorkflow(consignment);
      }

      if (!consignment.getWarehouse().isExternal()) {
        this.getInventoryEventService().createAllocationEvents(consignment);
      }
    } catch (final AmbiguousIdentifierException var8) {
      consignment.setStatus(ConsignmentStatus.CANCELLED);
      BlLogger
          .logFormatMessageInfo(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_ALLOCATION_ERROR.getCode(),
              "Cancelling consignment with code {}  since only one fulfillment system configuration is allowed per consignment.",
              consignment.getCode(), var8);
    } catch (Exception ex) {
      consignment.setStatus(ConsignmentStatus.CANCELLED);
      BlLogger
          .logFormatMessageInfo(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_ALLOCATION_ERROR.getCode(),
              "Cancelling consignment with code {}",
              consignment.getCode(), ex);
    }

    final List<String> allocatedProductCodes = new ArrayList<>();
    for (Set<String> aSet : result.getSerialProductMap().values()) {
      allocatedProductCodes.addAll(aSet);
    }
    final Collection<StockLevelModel> serialStocks = getSerialsForDateAndCodes(order,
        new HashSet<>(allocatedProductCodes));

    if ((!serialStocks.isEmpty()) && serialStocks.stream()
        .allMatch(stock -> allocatedProductCodes.contains(stock.getSerialProductCode()))) {
      serialStocks.forEach(stock -> stock.setReservedStatus(true));
      this.getModelService().saveAll(serialStocks);
      this.getModelService().save(consignment);
      return consignment;
    } else {
      return null;
    }

  }

  private Collection<StockLevelModel> getSerialsForDateAndCodes(final AbstractOrderModel order,
      final Set<String> serialProductCodes) {

    return blStockLevelDao
        .findSerialStockLevelsForDateAndCodes(serialProductCodes, order.getRentalStartDate(),
            order.getRentalEndDate());
  }

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
    entry.setSerialProductCodes(result.getSerialProductMap()
        .get(orderEntry.getEntryNumber()));   //setting serial products from result
    final Set<ConsignmentEntryModel> consignmentEntries = new HashSet<>();
    if (orderEntry.getConsignmentEntries() != null) {
      orderEntry.getConsignmentEntries().forEach(consignmentEntries::add);
    }

    consignmentEntries.add(entry);
    orderEntry.setConsignmentEntries(consignmentEntries);
    return entry;
  }

  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }

}
