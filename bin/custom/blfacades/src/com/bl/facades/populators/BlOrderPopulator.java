package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.OrderPopulator;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;

import java.math.BigDecimal;


/**
 * @author Admin
 *
 */
public class BlOrderPopulator extends OrderPopulator
{

  @Override
  public void populate(final OrderModel source, final OrderData target)
  {
    super.populate(source, target);
    final double rawSubTotal = source.getSubtotal() != null ? source.getSubtotal().doubleValue() : 0.0d;
    final PriceData subTotalRawPriceData = createPrice(source, Double.valueOf(rawSubTotal));
    //		if (subTotalRawPriceData != null)
    //		{
    //			target.setRawSubTotal(subTotalRawPriceData);
    //		}
    String paymentProvider = "CREDIT_CARD";

    final PriceDataType priceType = PriceDataType.BUY;
    if (source.getTotalPrice() != null && source.getGiftCardAmount() != null)
    {
      final PriceData grandTotal = getPriceDataFactory().create(priceType, BigDecimal.valueOf(source.getGrandTotal()),
          source.getCurrency() != null ? source.getCurrency().getIsocode() : "");
      if (grandTotal != null)
      {
        target.setGrandTotal(grandTotal);
      }

    }

    //		if (source.getTaxRate() != null)
    //		{
    //			target.setTaxRate(source.getTaxRate());
    //		}
    //
    //		if (source.getGiftCardCode() != null)
    //		{
    //			target.setGiftCode(source.getGiftCardCode());
    //		}

    if (source.getGiftCardAmount() != null)
    {

      final PriceData giftDiscount = getPriceDataFactory().create(priceType,
          BigDecimal.valueOf(source.getGiftCardAmount().doubleValue()), source.getCurrency().getIsocode());
      if (giftDiscount != null)
      {
        target.setGiftCardDiscount(giftDiscount);
      }
    }

    //		if (source.getDiscountDelivery() != null)
    //		{
    //
    //			final PriceData discountDelivery = getPriceDataFactory().create(priceType,
    //					BigDecimal.valueOf(source.getDiscountDelivery().doubleValue()), source.getCurrency().getIsocode());
    //			if (discountDelivery != null)
    //			{
    //				target.setDiscountDelivery(discountDelivery);
    //			}
    //		}

    //		if (source.getTotalDeliveryWithFreeShipping() != null)
    //		{
    //
    //			final PriceData totalDeliveryWithFreeShipping = getPriceDataFactory().create(priceType,
    //					BigDecimal.valueOf(source.getTotalDeliveryWithFreeShipping().doubleValue()), source.getCurrency().getIsocode());
    //			if (totalDeliveryWithFreeShipping != null)
    //			{
    //				target.setTotalDeliveryWithFreeShipping(totalDeliveryWithFreeShipping);
    //			}
    //		}

    //		try
    //		{
    //			if (source.getPaymentTransactions() != null)
    //			{
    //				final List<PaymentTransactionModel> paymentTransactionModelList = source.getPaymentTransactions();
    //
    //
    //				if (paymentTransactionModelList != null && paymentTransactionModelList.size() > 0)
    //				{
    //
    //					for (final PaymentTransactionModel PaymentTransaction : paymentTransactionModelList)
    //					{
    //						final List<PaymentTransactionEntryModel> Entries = PaymentTransaction.getEntries();
    //						for (final PaymentTransactionEntryModel PaymentEntries : Entries)
    //						{
    //
    //							if (((PaymentEntries.getTransactionStatus().equalsIgnoreCase("ACCEPT")
    //									&& PaymentEntries.getType().equals(PaymentTransactionType.AUTHORIZATION)
    //									&& PaymentEntries.getTransactionStatusDetails().equals("100")
    //									&& PaymentTransaction.getPaymentProvider().equalsIgnoreCase("CREDIT_CARD"))) ||
    //
    //									((PaymentEntries.getTransactionStatus().equalsIgnoreCase("REVIEW")
    //											&& PaymentEntries.getType().equals(PaymentTransactionType.AUTHORIZATION)
    //											&& PaymentEntries.getTransactionStatusDetails().equals("480")
    //											&& PaymentTransaction.getPaymentProvider().equalsIgnoreCase("CREDIT_CARD")))
    //									||
    //
    //									((PaymentEntries.getTransactionStatus().equalsIgnoreCase("OPEN")
    //											&& PaymentEntries.getType().equals(PaymentTransactionType.AUTHORIZATION)
    //											&& PaymentTransaction.getPaymentProvider().equalsIgnoreCase("amazonpay")))
    //									||
    //
    //									((PaymentEntries.getTransactionStatus().equalsIgnoreCase("ACCEPT")
    //											&& PaymentEntries.getType().equals(PaymentTransactionType.AUTHORIZATION)
    //											&& PaymentEntries.getTransactionStatusDetails().equals("100")
    //											&& PaymentTransaction.getPaymentProvider().equalsIgnoreCase("PAY_PAL")))
    //									||
    //
    //									((PaymentEntries.getTransactionStatus().equalsIgnoreCase("REVIEW")
    //											&& PaymentEntries.getType().equals(PaymentTransactionType.AUTHORIZATION)
    //											&& PaymentEntries.getTransactionStatusDetails().equals("480")
    //											&& PaymentTransaction.getPaymentProvider().equalsIgnoreCase("PAY_PAL")))
    //									)
    //
    //							{
    //								paymentProvider = PaymentTransaction.getPaymentProvider();
    //							}
    //						}
    //					}
    //				}
    //			}
    //		}
    //		catch (final Exception ex)
    //		{
    //			LOGGER.error("Error while setting PaymentProvider : " + ex);
    //		}

    if (source.getTotalPrice() != null && source.getGiftCardAmount() != null)
    {
      //SUS-988- Added extra condition to fixed payment type related issue in case of order Cancellation.

      if ((Double.compare(source.getTotalPrice(), Double.valueOf(0.0)) > 0 || (source.getStatus() != null
          && source.getStatus().equals(OrderStatus.CANCELLED) && source.getTotalPrice().doubleValue() == 0)))
      {
        if (Double.compare(source.getGiftCardAmount(), Double.valueOf(0.0)) > 0)
        {
          if (source.getGrandTotal().doubleValue() == source.getGiftCardAmount().doubleValue())
          {
            paymentProvider = "Gift Card";
          }
          else
          {
            paymentProvider = paymentProvider + ":" + "GIFT_CARD";
          }
        }
      }
      else
      {
        paymentProvider = "Gift Card";
      }
    }

    //		if (paymentProvider != null)
    //		{
    //			target.setPaymentProvider(paymentProvider);
    //		}
    //
    //		if (target.getPaymentProvider() == null)
    //		{
    //			target.setPaymentProvider("CREDIT_CARD");
    //		}

    //		if (source.getAllPromotionResults() != null && !source.getAllPromotionResults().isEmpty())
    //		{
    //			final Set<PromotionResultModel> promotionResult = source.getAllPromotionResults();
    //			final Collection<String> appliedCouponCodes = source.getAppliedCouponCodes();
    //
    //			if (appliedCouponCodes != null && appliedCouponCodes.size() > 0)
    //			{
    //
    //				final List<String> validCoupons = new ArrayList<String>();
    //				for (final PromotionResultModel promotion : promotionResult)
    //				{
    //
    //					promotion.getActions().stream().forEach(act -> {
    //						if (act instanceof AbstractRuleBasedPromotionActionModel)
    //						{
    //							final AbstractRuleBasedPromotionActionModel ruleBasedPromotionAction = (AbstractRuleBasedPromotionActionModel) act;
    //							final Collection<String> coupon = ruleBasedPromotionAction.getUsedCouponCodes();
    //							if (CollectionUtils.isNotEmpty(coupon))
    //							{
    //								for (final String appliedCoupon : appliedCouponCodes)
    //								{
    //									if (coupon.contains(appliedCoupon))
    //									{
    //										validCoupons.add(appliedCoupon);
    //									}
    //								}
    //							}
    //						}
    //					});
    //				}
    //
    //				target.setValidCoupons(validCoupons);
    //			}
    //		}
    //		target.setCancelledEntries(getCancelledEntriesData(source));
    //
    //		//Get previously applied and removed promotions on this order
    //		setAppliedOrderPromotionsHistory(source, target);
    //		//SUS-1130 commented below code as part of this defect
    //		// target.setOrderDiscountsHistory(getTotalDiscountsHistory(source));
    //
    //		if (source.getAdjustmentRequests() != null && !source.getAdjustmentRequests().isEmpty())
    //		{
    //			Assert.notNull(source.getAdjustmentRequests(), "Parameter returnEntries in return cannot be null.");
    //			target.setAdjustmentRequets(Converters.convertAll(source.getAdjustmentRequests(), getAdjustmentRequestConverter()));
    //		}
    //		//COR-1701
    //				if(source.getPaymentTransactions()!=null && CollectionUtils.isNotEmpty(source.getPaymentTransactions()))
    //					for (final PaymentTransactionModel transaction : source.getPaymentTransactions())
    //					{
    //						if(transaction instanceof CybsPaymentTransactionModel && ((CybsPaymentTransactionModel) transaction).getKlarnaTransactionId()!=null && !((CybsPaymentTransactionModel) transaction).getKlarnaTransactionId().isEmpty())
    //						{
    //							target.setKlarnaTransactionId(((CybsPaymentTransactionModel) transaction).getKlarnaTransactionId());
    //							target.setCybersourceTransactionId(transaction.getRequestId());
    //							break;
    //						}
    //
    //					}
    //
    //		// SUS-103 - This method will set the order status display attribute to display the order level status to the guest user on order details page.
    //		setOrderStatusDisplay(source, target);

  }
}