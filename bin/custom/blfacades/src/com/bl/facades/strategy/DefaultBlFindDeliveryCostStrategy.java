package com.bl.facades.strategy;

import com.bl.core.shipping.service.BlDeliveryModeService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.order.strategies.calculation.impl.DefaultFindDeliveryCostStrategy;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.util.PriceValue;
import java.util.Objects;
import org.apache.log4j.Logger;

import javax.annotation.Resource;

public class DefaultBlFindDeliveryCostStrategy extends DefaultFindDeliveryCostStrategy {

    private static final Logger LOG = Logger.getLogger(DefaultBlFindDeliveryCostStrategy.class);

    @Resource(name = "blDeliveryModeService")
    BlDeliveryModeService blDeliveryModeService;

    //step 1 : delegate to jalo
    @Override
    public PriceValue getDeliveryCost(final AbstractOrderModel order)
    {
        ServicesUtil.validateParameterNotNullStandardMessage("order", order);
        try
        {
            DeliveryModeModel deliveryMode = order.getDeliveryMode();
            getModelService().save(order);

            return new PriceValue(order.getCurrency().getIsocode(),
                Objects.nonNull(order.getRefundShippingTotalAmount()) && order.getRefundShippingTotalAmount() > 0.0 ?  0.0 : getBlDeliveryModeService().getShippingCostAmount(order,
                    deliveryMode), order.getNet());
        }
        catch (final Exception e)
        {
            LOG.warn("Could not find deliveryCost for order [" + order.getCode() + "] due to : " + e + "... skipping!");
            return new PriceValue(order.getCurrency().getIsocode(), 0.0, order.getNet());
        }
    }

    public BlDeliveryModeService getBlDeliveryModeService() {
        return blDeliveryModeService;
    }

    public void setBlDeliveryModeService(BlDeliveryModeService blDeliveryModeService) {
        this.blDeliveryModeService = blDeliveryModeService;
    }
}
