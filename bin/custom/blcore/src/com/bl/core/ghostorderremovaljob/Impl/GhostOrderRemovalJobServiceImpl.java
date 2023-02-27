package com.bl.core.ghostorderremovaljob.Impl;

import com.bl.core.ghostorderremovaljob.GhostOrderRemovalJobService;
import com.bl.core.order.dao.BlOrderDao;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.collections4.CollectionUtils;

import java.util.List;

public class GhostOrderRemovalJobServiceImpl implements GhostOrderRemovalJobService {

    private BlOrderDao orderDao;

    private ModelService modelService;

    @Override
    public List<OrderModel> removeGhostOrders() {

        final List<OrderModel> orders = orderDao.getGhostOrders();

        for (OrderModel orderModel : orders) {
            if (CollectionUtils.isNotEmpty(orders) && orderModel.getPaymentTransactions().size() >= 2) {
                getModelService().save(orderModel);
            } else {
                getModelService().remove(orderModel);
            }
        }

        return null;
    }


    private ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

    public BlOrderDao getOrderDao() {
        return orderDao;
    }

    public void setOrderDao(BlOrderDao orderDao) {
        this.orderDao = orderDao;
    }

}
