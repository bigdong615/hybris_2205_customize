package com.bl.tax.service.impl;

import com.bl.logging.BlLogger;
import com.bl.tax.DetailResponse;
import com.bl.tax.TaxLineResponse;
import com.bl.tax.TaxResponse;
import com.google.common.util.concurrent.AtomicDouble;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * This class is created for setting avalara tax with order
 * @author Manikandan
 */

public class BlAvalaraTaxCalculationService {
	
	private static final Logger LOG = Logger.getLogger(BlAvalaraTaxCalculationService.class);

  /**
   * This method set the order tax exempt bases on condition
   * @param abstractOrderModel the order model
   * @param taxResponse tax response
   */
  public void calculateTaxWithOrderTotal(final AbstractOrderModel abstractOrderModel,
      final TaxResponse taxResponse) {
    abstractOrderModel.setTotalTax(setTotalTaxToOrder(taxResponse));
    abstractOrderModel.setIsOrderTaxExempt(0.0 < taxResponse.getTotalExempt()
        && 0.0 == taxResponse.getTotalTax() ? Boolean.TRUE : Boolean.FALSE);
    setTaxRateOnOrderModel(taxResponse , abstractOrderModel);
  }

  /**
	 * This method created to set the tax rate on order model
	 *
	 * @param taxResponse
	 *           taxresponse
	 * @param abstractOrderModel
	 *           abstractOrderModel
	 */
	private void setTaxRateOnOrderModel(final TaxResponse taxResponse, final AbstractOrderModel abstractOrderModel)
	{
		if (Objects.isNull(abstractOrderModel))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"BlAvalaraTaxCalculationService :: setTaxRateOnOrderModel :: Cart/Order is Null", StringUtils.EMPTY);
		}
		else if (Objects.isNull(taxResponse))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"BlAvalaraTaxCalculationService :: setTaxRateOnOrderModel :: Tax Response is Null for Cart/Order : {}",
					abstractOrderModel.getCode());
		}
		else if (CollectionUtils.isEmpty(taxResponse.getTaxLines()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"BlAvalaraTaxCalculationService :: setTaxRateOnOrderModel :: Tax Lines are EMPTY for Cart/Order : {}",
					abstractOrderModel.getCode());
		}
		else
		{
			final Optional<TaxLineResponse> productLineItem = taxResponse.getTaxLines().stream()
					.filter(line -> StringUtils.isNotBlank(line.getItemCode())
							&& BooleanUtils.isFalse(line.getItemCode().equalsIgnoreCase("shipping")))
					.findFirst();
			if (productLineItem.isEmpty())
			{
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
						"BlAvalaraTaxCalculationService :: setTaxRateOnOrderModel :: No Product Line Item Details found for Cart/Order : {}",
						abstractOrderModel.getCode());
			}
			else
			{
				final List<DetailResponse> taxDetails = productLineItem.get().getDetails();
				if(CollectionUtils.isEmpty(taxDetails))
				{
					BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
							"BlAvalaraTaxCalculationService :: setTaxRateOnOrderModel :: Product Line Item Tax Details is Empty for Cart/Order : {}",
							abstractOrderModel.getCode());
				}
				else
				{
					final AtomicDouble taxRateValue = new AtomicDouble(0.0d);
					taxDetails.forEach(taxLineDetail -> {
						final Double taxRatePercentage = taxLineDetail.getRate() * 100;
						BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
								"BlAvalaraTaxCalculationService :: setTaxRateOnOrderModel :: jurisType : {} and jurisName : {} and tax name : {} and Tax Rate : {} for Cart/Order : {}",
								taxLineDetail.getJurisType(), taxLineDetail.getJurisName(), taxLineDetail.getTaxName(),
								taxRatePercentage.doubleValue(), abstractOrderModel.getCode());
						taxRateValue.addAndGet(taxRatePercentage);
					});
					final double twoDecimalTaxRateValue = getTwoDecimalDoubleValue(taxRateValue.get());
					abstractOrderModel.setTaxRate(twoDecimalTaxRateValue);
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
							"BlAvalaraTaxCalculationService :: setTaxRateOnOrderModel :: Total Tax Rate : {} for Cart/Order : {}",
							twoDecimalTaxRateValue, abstractOrderModel.getCode());
				}				
			}
		}
	}

	/**
	 * Gets the two decimal double value.
	 *
	 * @param value
	 *           the value
	 * @return the two decimal double value
	 */
	private double getTwoDecimalDoubleValue(final double value)
	{
		return BigDecimal.valueOf(value).setScale(2, RoundingMode.HALF_EVEN).doubleValue();
	}


  /**
   * this method set the total tax return from the avalara
   */
  private Double setTotalTaxToOrder(final TaxResponse taxResponse) {
    return taxResponse.getTotalTax() > 0.0 ? taxResponse.getTotalTax() : 0.0;
  }
}
