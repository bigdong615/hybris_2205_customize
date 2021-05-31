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


public class DefaultBlAllocationService extends DefaultAllocationService implements
    BlAllocationService {
  private static final Logger LOG = Logger
      .getLogger(DefaultBlAllocationService.class);
  private BlStockLevelDao blStockLevelDao;


  @Override
  public ConsignmentModel createConsignment(AbstractOrderModel order, String code, SourcingResult result) {
    ServicesUtil.validateParameterNotNullStandardMessage("result", result);
    ServicesUtil.validateParameterNotNullStandardMessage("order", order);
    Assert.isTrue(!Strings.isNullOrEmpty(code), "Parameter code cannot be null or empty");
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Creating consignment for Location: '{}'",
        result.getWarehouse().getCode());
    ConsignmentModel consignment = (ConsignmentModel)this.getModelService().create(ConsignmentModel.class);
    consignment.setCode(code);
    consignment.setOrder(order);

    try {
      consignment.setFulfillmentSystemConfig(this.getWarehousingFulfillmentConfigDao().getConfiguration(result.getWarehouse()));
      Set<Entry<AbstractOrderEntryModel, Long>> resultEntries = result.getAllocation().entrySet();
      Optional<PointOfServiceModel> pickupPos = resultEntries.stream().map((entry) -> {
        return ((AbstractOrderEntryModel)entry.getKey()).getDeliveryPointOfService();
      }).filter(Objects::nonNull).findFirst();
      if (pickupPos.isPresent()) {
        consignment.setStatus(ConsignmentStatus.READY);
        consignment.setDeliveryMode(this.getDeliveryModeService().getDeliveryModeForCode("pickup"));
        consignment.setShippingAddress(((PointOfServiceModel)pickupPos.get()).getAddress());
        consignment.setDeliveryPointOfService((PointOfServiceModel)pickupPos.get());
      } else {
        consignment.setStatus(ConsignmentStatus.READY);
        consignment.setDeliveryMode(order.getDeliveryMode());
        consignment.setShippingAddress(order.getDeliveryAddress());
        consignment.setShippingDate(this.getShippingDateStrategy().getExpectedShippingDate(consignment));
      }

      Set<ConsignmentEntryModel> entries = (Set)resultEntries.stream().map((mapEntry) -> {
        return this.createConsignmentEntry((AbstractOrderEntryModel)mapEntry.getKey(), (Long)mapEntry.getValue(), consignment, result);
      }).collect(Collectors.toSet());
      consignment.setConsignmentEntries(entries);
      consignment.setWarehouse(result.getWarehouse());
      if (consignment.getFulfillmentSystemConfig() == null) {
        this.getWarehousingConsignmentWorkflowService().startConsignmentWorkflow(consignment);
      }

      if (!consignment.getWarehouse().isExternal()) {
        this.getInventoryEventService().createAllocationEvents(consignment);
      }
    } catch (AmbiguousIdentifierException var8) {
      consignment.setStatus(ConsignmentStatus.CANCELLED);
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_ALLOCATION_ERROR.getCode(),"Cancelling consignment with code {}  since only one fulfillment system configuration is allowed per consignment.",
          consignment.getCode(), var8);
    }
    List<String> assignedSerialProducts = new ArrayList<>();
        for(Set<String> aSet : result.getSerialProductMap().values()) {
          assignedSerialProducts.addAll(aSet);
        }
    Collection<StockLevelModel> serialStocks = reserveSerialsAndSave(order, new HashSet<>(assignedSerialProducts));
    serialStocks.forEach(stock -> {
      stock.setReservedStatus(true);
    });
    if ((!serialStocks.isEmpty()) && serialStocks.stream().allMatch(stock -> assignedSerialProducts.contains(stock.getSerialProductCode()))) {
      this.getModelService().saveAll(serialStocks);
    }
    this.getModelService().save(consignment);
    return consignment;
  }

  private Collection<StockLevelModel> reserveSerialsAndSave(AbstractOrderModel order, Set<String> serialProductCodes) {
    return blStockLevelDao.findSerialStockLevelsForDateAndCodes(serialProductCodes, order.getRentalStartDate(), order.getRentalEndDate());
  }

  protected ConsignmentEntryModel createConsignmentEntry(AbstractOrderEntryModel orderEntry, Long quantity, ConsignmentModel consignment, SourcingResult result) {
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "ConsignmentEntry :: Product [{}]: \tQuantity: '{}'",
        orderEntry.getProduct().getCode(), quantity);
    ConsignmentEntryModel entry = (ConsignmentEntryModel)this.getModelService().create(ConsignmentEntryModel.class);
    entry.setOrderEntry(orderEntry);
    entry.setQuantity(quantity);
    entry.setConsignment(consignment);
    //entry.setSerialProductCodes(result.getSerialProductCodes());   //setting serial products from result
    entry.setSerialProductCodes(result.getSerialProductMap().get(orderEntry.getEntryNumber()));   //setting serial products from result
    Set<ConsignmentEntryModel> consignmentEntries = new HashSet();
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

  public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }

}
