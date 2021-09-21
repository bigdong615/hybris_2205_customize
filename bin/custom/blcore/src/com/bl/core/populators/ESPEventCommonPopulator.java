/**
 *
 */
package com.bl.core.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.ESPEventCommonRequest;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.util.Objects;
import org.springframework.util.Assert;


/**
 * BlOrderConfirmationRequestPopulator
 *
 *
 */
public class ESPEventCommonPopulator implements Populator<OrderModel, ESPEventCommonRequest> {

    private ConfigurationService configurationService;

    /**
     * Populate the ESPEventCommonRequest instance with values from the OrderModel.
     *
     * @param order        the source object
     * @param espEventCommonRequest the target to fill
     * @throws ConversionException if an error occurs
     */
    @Override
    public void populate(final OrderModel order, final ESPEventCommonRequest espEventCommonRequest)
        throws ConversionException {
        Assert.notNull(order, "Parameter order cannot be null.");
        Assert.notNull(espEventCommonRequest, "Parameter espEventCommonRequest cannot be null.");

        populateCommonData(order, espEventCommonRequest);
    }

    private void populateCommonData(final OrderModel orderModel,
        final ESPEventCommonRequest espEventCommonRequest) {

        espEventCommonRequest.setOrderid(orderModel.getCode());
        espEventCommonRequest.setTemplate(getConfigurationService().getConfiguration()
            .getString(BlCoreConstants.ORDER_CONFIRMATION_EVENT_TEMPLATE));
        espEventCommonRequest.setSubscriberid("718628824577");
        if (Objects.nonNull(orderModel.getUser())) {
            espEventCommonRequest.setEmailaddress(orderModel.getUser().getUid());
        }
        espEventCommonRequest.setVerificationlevel(1);
    }

    public ConfigurationService getConfigurationService() {
        return configurationService;
    }

    public void setConfigurationService(ConfigurationService configurationService) {
        this.configurationService = configurationService;
    }
}
