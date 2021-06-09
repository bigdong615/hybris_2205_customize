package com.braintree.fraud.symptom.impl;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.fraud.impl.FraudServiceResponse;
import de.hybris.platform.fraud.impl.FraudSymptom;
import de.hybris.platform.fraud.symptom.impl.OrderThresholdSymptom;

public class BraintreeOrderThresholdSymptom  extends OrderThresholdSymptom {

    @Override
    public FraudServiceResponse recognizeSymptom(final FraudServiceResponse fraudResponse, final AbstractOrderModel order) {
        final Double orderTotalPrice = getOrderTotalPrice(order);

        if (orderTotalPrice.compareTo(this.getThresholdLimit()) > 0) {
            double difference = order.getTotalPrice() - this.getThresholdLimit();
            fraudResponse.addSymptom(new FraudSymptom(this.getSymptomName(), this.getIncrement(difference)));
        } else {
            fraudResponse.addSymptom(this.createSymptom(false));
        }

        return fraudResponse;
    }

    private double getOrderTotalPrice(final AbstractOrderModel order) {
        return order.getTotalPrice() / order.getCurrency().getConversion();
    }
}
