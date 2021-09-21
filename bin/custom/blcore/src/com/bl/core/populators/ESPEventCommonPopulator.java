/**
 *
 */
package com.bl.core.populators;

import com.bl.core.utils.BlDateTimeUtils;
import com.bl.esp.dto.ESPEventCommonRequest;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import java.util.Objects;


/**
 * Abstract class for ESPEvent requests. Conversion methods should be implemented in inheriting
 * class.
 */
public abstract class ESPEventCommonPopulator<SOURCE extends AbstractOrderModel, TARGET extends ESPEventCommonRequest> implements
    Populator<SOURCE, TARGET> {

    private ConfigurationService configurationService;

    /**
     * Populate common attributes with values from the OrderModel.
     *
     * @param orderModel            the source object
     * @param espEventCommonRequest the target to fill
     */
    protected void populateCommonData(final AbstractOrderModel orderModel,
        final ESPEventCommonRequest espEventCommonRequest) {

        espEventCommonRequest.setOrderid(orderModel.getCode());
        if (Objects.nonNull(orderModel.getUser())) {
            espEventCommonRequest.setEmailaddress(orderModel.getUser().getUid());
            espEventCommonRequest.setSubscriberid(orderModel.getUser().getUid());
        }
        espEventCommonRequest.setVerificationlevel(1);
    }

    /**
     * Populate rental duration with values from the OrderModel.
     *
     * @param orderModel the source object
     */
    protected long getRentalDuration(final AbstractOrderModel orderModel) {

        return BlDateTimeUtils
            .getDaysBetweenDates(orderModel.getRentalStartDate(), orderModel.getRentalEndDate());
    }

    public ConfigurationService getConfigurationService() {
        return configurationService;
    }

    public void setConfigurationService(ConfigurationService configurationService) {
        this.configurationService = configurationService;
    }
}
