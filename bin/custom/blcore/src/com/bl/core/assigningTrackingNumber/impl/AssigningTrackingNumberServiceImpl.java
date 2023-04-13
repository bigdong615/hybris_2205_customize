package com.bl.core.assigningTrackingNumber.impl;

import com.bl.core.assigningTrackingNumber.AssigningTrackingNumberService;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.warehousing.shipping.strategy.DeliveryTrackingIdStrategy;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Logger;

import java.util.List;

/**
 * This service assigns the Tracking Number to Consignment if the order status is shipped.
 *
 * @author Sunil
 */
public class AssigningTrackingNumberServiceImpl implements AssigningTrackingNumberService {
    private static final Logger LOG = Logger.getLogger(AssigningTrackingNumberService.class);
    private BlInventoryScanToolDao blInventoryScanToolDao;
    private DeliveryTrackingIdStrategy deliveryTrackingIdStrategy;
    private ModelService modelService;

    public List<OrderModel> assigningTracking() {
        final List<OrderModel> orders = blInventoryScanToolDao.getOrders();
        for (OrderModel orderModel : orders) {
            if (CollectionUtils.isNotEmpty(orders) && (orderModel.getStatus() == OrderStatus.SHIPPED)) {
                for (final ConsignmentModel consignment : orderModel.getConsignments()) {
                    ServicesUtil.validateParameterNotNull(consignment, "Consignment cannot be null");
                    String trackingId;
                    if (consignment.getTrackingID() == null) {
                        trackingId = this.getDeliveryTrackingIdStrategy().generateTrackingId(consignment);
                        consignment.setTrackingID(trackingId);
                        this.getModelService().save(consignment);
                    }
                }
            }
        }
        return orders;
    }

    public BlInventoryScanToolDao getBlInventoryScanToolDao() {
        return blInventoryScanToolDao;
    }

    public void setBlInventoryScanToolDao(BlInventoryScanToolDao blInventoryScanToolDao) {
        this.blInventoryScanToolDao = blInventoryScanToolDao;
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

    public DeliveryTrackingIdStrategy getDeliveryTrackingIdStrategy() {
        return deliveryTrackingIdStrategy;
    }

    public void setDeliveryTrackingIdStrategy(DeliveryTrackingIdStrategy deliveryTrackingIdStrategy) {
        this.deliveryTrackingIdStrategy = deliveryTrackingIdStrategy;
    }
}

