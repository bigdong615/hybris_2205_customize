package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.product.replacement.ProductReplacementEventRequest;
import com.bl.esp.dto.product.replacement.data.ProductReplacementData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

import java.text.SimpleDateFormat;
import java.util.Objects;

public class BlReplacementProductRequestPopulator extends ESPEventCommonPopulator<OrderModel, ProductReplacementEventRequest> {
    private static final Logger LOG = Logger.getLogger(BlReplacementProductRequestPopulator.class);

    @Override
    public void populate(final OrderModel orderModel,final ProductReplacementEventRequest productReplacementEventRequest) throws ConversionException {
        Assert.notNull(orderModel, "Parameter order cannot be null.");
        Assert.notNull(productReplacementEventRequest, "Parameter request cannot be null.");

        final UserModel userModel = orderModel.getUser();
        if (Objects.nonNull(userModel)) {
            productReplacementEventRequest.setContactKey(getRequestValue(userModel.getUid()));
        }
        productReplacementEventRequest
                .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
                        getString(BlCoreConstants.PRODUCT_REPLACEMENT_EVENT_DEFINITION_KEY)));

        final ProductReplacementData replacementData = new ProductReplacementData();
        replacementData.setSubscriberID(getRequestValue(userModel.getUid()));
        replacementData.setOrderID(getRequestValue(orderModel.getCode()));
        final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
        replacementData.setRentalStartDate(formatter.format(orderModel.getRentalStartDate()));
        replacementData.setRentalEndDate(formatter.format(orderModel.getRentalEndDate()));
        productReplacementEventRequest.setData(replacementData);
    }
}
