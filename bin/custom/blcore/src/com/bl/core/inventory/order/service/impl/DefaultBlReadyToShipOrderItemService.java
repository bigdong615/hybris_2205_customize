package com.bl.core.inventory.order.service.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlReadyToShipOrderItemDao;
import com.bl.core.inventory.order.service.BlReadyToShipOrderItemService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.ReadyToShipOrderItemModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.log4j.Logger;

/**
 * Service class for ReadyToShipOrderItem
 *
 * @author Sunil Sahu
 */
public class DefaultBlReadyToShipOrderItemService implements BlReadyToShipOrderItemService {

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
              .add(createReadyToShipOrderItem(consignmentModel, memberName, entry, serialProduct));
        }
      }
    }

    modelService.saveAll(orderItems);
  }

  /**
   * This method will remove all  ReadyToShipOrderItems for specified shipdate and warehouse.
   *
   * @param shipDate  the shipDate
   * @param warehouse the warehouse
   */
  @Override
  public void removeReadyToShipOrderItemsForDateAndWareshouse(final Date shipDate,
      final WarehouseModel warehouse) {

    blReadyToShipOrderItemDao
        .removeReadyToShipOrderItemsForDate(shipDate, warehouse);
  }

  /**
   * It creates and returns ReadyToShipOrderItem
   * @param consignmentModel
   * @param memberName
   * @param entry
   * @param serialProduct
   * @return ReadyToShipOrderItemModel
   */
  private ReadyToShipOrderItemModel createReadyToShipOrderItem(
      final ConsignmentModel consignmentModel,
      final String memberName, final ConsignmentEntryModel entry,
      final BlSerialProductModel serialProduct) {

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
    orderItem.setProductId(entry.getOrderEntry().getProduct().getCode());
    orderItem.setProductName(entry.getOrderEntry().getProduct().getName());
    orderItem.setProductCount(BlCoreConstants.DEFAULT_PRODUCT_QUANTITY);
    orderItem.setSerialNumber(serialProduct.getCode());

    orderItem.setChildLocation(
        null != serialProduct.getOcLocation() ? (serialProduct).getOcLocation()
            : BlCoreConstants.EMPTY_STRING);
    orderItem.setParentLocation(
        null != (serialProduct).getLastLocationScanParent() ? (serialProduct)
            .getLastLocationScanParent() : BlCoreConstants.EMPTY_STRING);

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
