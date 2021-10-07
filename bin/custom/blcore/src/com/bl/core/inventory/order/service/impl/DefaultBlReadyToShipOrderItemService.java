package com.bl.core.inventory.order.service.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlReadyToShipOrderItemDao;
import com.bl.core.inventory.order.service.BlReadyToShipOrderItemService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.ReadyToShipOrderItemModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Service class for ReadyToShipOrderItem
 *
 * @author Sunil Sahu
 */
public class DefaultBlReadyToShipOrderItemService implements BlReadyToShipOrderItemService {

  private static final Logger LOG = Logger.getLogger(DefaultBlReadyToShipOrderItemService.class);

  private BlReadyToShipOrderItemDao blReadyToShipOrderItemDao;
  private ModelService modelService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void createReadyToShipOrderItems(final List<ConsignmentModel> consignmentModels,
      final Integer membersCount) {

    final List<String> membersList = getMembersNameList(membersCount);
    int counter = 0;

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "Started creating ReadyToShipOrderItems for {} consignments", consignmentModels.size());

    final List<ReadyToShipOrderItemModel> orderItems = new ArrayList<>();
    for (ConsignmentModel consignmentModel : consignmentModels) {

      if (membersList.size() <= counter) {
        counter = 0;
      }

      final String memberName = membersList.get(counter);
      counter++;

      for (ConsignmentEntryModel entry : consignmentModel.getConsignmentEntries()) {

        final List<BlProductModel> blProductModels = new ArrayList<>(entry.getSerialProducts());

        final List<BlSerialProductModel> serialProductModels = new ArrayList<>();
        blProductModels.stream().forEach(serial -> {
          if (serial instanceof BlSerialProductModel) {
            serialProductModels.add((BlSerialProductModel) serial);
          }
        });

        for (BlSerialProductModel serialProduct : serialProductModels) {

          orderItems
              .add(createReadyToShipOrderItem(consignmentModel, memberName, serialProduct));
        }
      }
    }

    modelService.saveAll(orderItems);
  }

  /**
   * This method will remove all  ReadyToShipOrderItems for specified shipdate and warehouse.
   *
   * @param warehouse the warehouse
   */
  @Override
  public void removeReadyToShipOrderItemsForWarehouse(final WarehouseModel warehouse) {

    final List<ReadyToShipOrderItemModel> orderItemModels = blReadyToShipOrderItemDao
        .getReadyToShipOrderItemsForWarehouse(warehouse);

    if (CollectionUtils.isEmpty(orderItemModels)) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "No order items available to remove for warehouse with code {}.", warehouse.getCode());
    } else {
      modelService.removeAll(orderItemModels);
    }
  }

  /**
   * It creates and returns ReadyToShipOrderItem
   * @param consignmentModel
   * @param memberName
   * @param serialProduct
   * @return ReadyToShipOrderItemModel
   */
  private ReadyToShipOrderItemModel createReadyToShipOrderItem(
      final ConsignmentModel consignmentModel,
      final String memberName, final BlSerialProductModel serialProduct) {

    final ReadyToShipOrderItemModel orderItem = modelService
        .create(ReadyToShipOrderItemModel.class);

    orderItem.setShipDate(consignmentModel.getOptimizedShippingStartDate());
    orderItem.setEmployeeName(memberName);  // memberName
    orderItem.setWarehouse(consignmentModel.getWarehouse());
    orderItem.setOrderNumber(consignmentModel.getOrder().getCode());
    orderItem.setOrderType(
        null != consignmentModel.getOrderType() ? consignmentModel.getOrderType()
            .getCode() : BlCoreConstants.EMPTY_STRING);

    orderItem.setIsVipOrder(consignmentModel.getOrder().getIsVipOrder());

    orderItem.setIsUsedGearOrder(
        null != consignmentModel.getOrder().getIsRentalCart() && !consignmentModel.getOrder()
            .getIsRentalCart());

    orderItem.setOrderNotes(
        null != consignmentModel.getOrder().getConsolidatedOrderNote() ? consignmentModel.getOrder()
            .getConsolidatedOrderNote() : BlCoreConstants.EMPTY_STRING);

    orderItem.setProductId(serialProduct.getBlProduct().getCode());
    orderItem.setProductName(serialProduct.getBlProduct().getName());

    orderItem.setProductCount(BlCoreConstants.DEFAULT_PRODUCT_QUANTITY);
    orderItem.setSerialNumber(serialProduct.getCode());

    orderItem.setChildLocation(
        null != serialProduct.getOcLocation() ? (serialProduct).getOcLocation()
            : BlCoreConstants.EMPTY_STRING);
    orderItem.setParentLocation(
        null != (serialProduct).getLastLocationScanParent() ? (serialProduct)
            .getLastLocationScanParent() : BlCoreConstants.EMPTY_STRING);

    orderItem.setShippingMethod(
        null != (ZoneDeliveryModeModel) consignmentModel.getDeliveryMode()
            ? ((ZoneDeliveryModeModel) consignmentModel.getDeliveryMode()).getName()
            : BlCoreConstants.EMPTY_STRING);

    BlLogger
        .logFormatMessageInfo(LOG, Level.DEBUG,
            "Created ReadyToShipOrderItem for consignment code {} and ship date {}",
            consignmentModel.getCode(), consignmentModel.getOptimizedShippingStartDate());

    return orderItem;
  }

  /**
   * This method returns the list of members for the specified members count in the cronjob.
   *
   * @param membersCount
   * @return List<String>
   */
  private List<String> getMembersNameList(final Integer membersCount) {

    final List<String> membersList = new ArrayList<>();
    for (int i = 1; i <= membersCount; i++) {
      membersList.add(BlCoreConstants.EMPLOYEE + i);
    }
    return membersList;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }

  public BlReadyToShipOrderItemDao getBlReadyToShipOrderItemDao() {
    return blReadyToShipOrderItemDao;
  }

  public void setBlReadyToShipOrderItemDao(
      final BlReadyToShipOrderItemDao blReadyToShipOrderItemDao) {
    this.blReadyToShipOrderItemDao = blReadyToShipOrderItemDao;
  }

}
