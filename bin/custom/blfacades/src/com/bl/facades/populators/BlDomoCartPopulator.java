/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.order.EntryGroup;
import de.hybris.platform.payment.model.PaymentTransactionModel;

import java.math.BigDecimal;
import java.util.stream.Collectors;

import org.apache.commons.lang.BooleanUtils;
import org.apache.log4j.Logger;

import com.bl.core.model.GiftCardModel;


/**
 * @author kumar
 *
 */
public class BlDomoCartPopulator extends BlCartPopulator
{
	private static final Logger LOG = Logger.getLogger(BlDomoCartPopulator.class);

	/**
	 * It populates data.
	 *
	 * @param source
	 * @param target
	 */
	@Override
	public void populate(final CartModel source, final CartData target)
	{
		LOG.info("Cart Code : " + source.getCode());
		super.populate(source, target);
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
		if (source.getGiftCardAmount() != null)
		{
			final PriceData giftDiscount = getPriceDataFactory().create(priceType,
					BigDecimal.valueOf(source.getGiftCardAmount().doubleValue()), source.getCurrency().getIsocode());
			if (giftDiscount != null)
			{
				target.setGiftCardDiscount(giftDiscount);
			}
		}
		if (BooleanUtils.isTrue(source.getIsRetailGearOrder()))
		{
			target.setIsRetailGearOrder(source.getIsRetailGearOrder());
		}
		target.setOrderReturnedToWarehouse(source.isOrderReturnedToWarehouse());

		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		target.setOrderUnboxingTimestamp(source.getOrderUnboxingTimestamp());
		target.setIsOrderCommittedToAvalara(source.getIsOrderCommittedToAvalara());
		target.setTaxRate(source.getTaxRate());
		target.setRefundedShippingTaxAmount(source.getRefundedShippingTaxAmount());
		target.setRefundedTotalGiftCardAmount(source.getRefundedTotalGiftCardAmount());
		target.setRefundTotalDamageWaiverAmount(source.getRefundTotalDamageWaiverAmount());
		target.setIsAuthorizationAttempted(source.isIsAuthorizationAttempted());
		if (source.getOrderCancellationHistoryLog() != null)
		{
			target.setBlOrderCancellationHistory(source.getOrderCancellationHistoryLog().getCancelReason());
		}
		target.setIsSAPOrder(source.getIsSAPOrder());
		target.setOrderID(source.getOrderID());
		target.setRunTot_daysRented(source.getRunTot_daysRented());
		target.setIsLatestOrder(source.isIsLatestOrder());
		target.setRunTot_totalTax(source.getRunTot_totalTax());
		target.setRunTot_totalPrice(source.getRunTot_totalPrice());
		target.setRunTot_totalOptionsCost(source.getRunTot_totalOptionsCost());
		target.setRunTot_subtotal(source.getRunTot_subtotal());
		target.setRunTot_grandTotal(source.getRunTot_grandTotal());
		target.setDepositAmount(source.getDepositAmount());
		target.setIsVideoOrder(source.getIsVideoOrder());
		if (source.getSentOrderFeedToSalesforce() != null)
		{
			target.setSentOrderFeedToSalesforce(source.getSentOrderFeedToSalesforce().getCode());
		}
		target.setCoiAmount(source.getCoiAmount());
		target.setUpdatedTime(source.getUpdatedTime());
		target.setOrderBillModifiedDate(source.getOrderBillModifiedDate());
		target.setOrderModifiedDate(source.getOrderModifiedDate());
		if (source.getVerificationStatus() != null)
		{
			target.setVerificationStatusEnum(source.getVerificationStatus().getCode());
		}
		target.setVerificationLevel(source.getVerificationLevel());
		target.setOrderCompletedDate(source.getOrderCompletedDate());
		target.setManualReviewStatusByReshuffler(source.isManualReviewStatusByReshuffler());
		target.setTempModifiedOrderAppliedGcList(
				source.getTempModifiedOrderAppliedGcList().stream().map(GiftCardModel::getCode).collect(Collectors.joining(", ")));
		target.setModifiedOrderAppliedGcList(
				source.getModifiedOrderAppliedGcList().stream().map(GiftCardModel::getCode).collect(Collectors.joining(", ")));
		target.setModifiedOrderPoNotes(source.getModifiedOrderPoNotes());
		target.setModifiedOrderPoNumber(source.getModifiedOrderPoNumber());
		target.setModifiedOrderPoAmount(source.getModifiedOrderPoAmount());
		target.setDepositAmountTotal(source.getDepositAmountTotal());
		target.setDepositPaymentTransactions(source.getDepositPaymentTransactions().stream().map(PaymentTransactionModel::getCode)
				.collect(Collectors.joining(", ")));
		target.setDepositPaymentInfo(
				source.getDepositPaymentInfo().stream().map(PaymentInfoModel::getCode).collect(Collectors.joining(", ")));
		target.setInternalTransferOrder(source.getInternalTransferOrder());
		if (source.getInternalTransferOrderWarehouse() != null)
		{
			target.setInternalTransferOrderWarehouse(source.getInternalTransferOrderWarehouse().getCode());
		}
		target.setCreateConsignment(source.getCreateConsignment());
		target.setShareASaleSent(source.getShareASaleSent());
		target.setOrderTotalBeforeGCAdd(source.getOrderTotalBeforeGCAdd());
		target.setIsOrderSubmit(source.getIsOrderSubmit());
		target.setIsUnPaidBillPresent(source.isUnPaidBillPresent());
		target.setExtendRentalStartDate(source.getExtendRentalStartDate());
		target.setIsReplacementOrder(source.getIsReplacementOrder());
		target.setIsRetailGearOrder(source.getIsRetailGearOrder());
		target.setOrderReturnedToWarehouse(source.isOrderReturnedToWarehouse());
		if (source.getReturnRequestForOrder() != null)
		{
			target.setReturnRequestForOrder(source.getReturnRequestForOrder().getCode());
		}
		target.setIsVipOrder(source.getIsVipOrder());
		target.setPromotionalRentalEndDate(source.getPromotionalRentalEndDate());
		target.setConsolidatedOrderNote(source.getConsolidatedOrderNote());
		target.setRefundTotalAmount(source.getRefundTotalAmount());
		target.setRefundTaxTotalAmount(source.getRefundTaxTotalAmount());
		target.setRefundShippingTotalAmount(source.getRefundShippingTotalAmount());
		target.setOriginalOrderTotalAmount(source.getOriginalOrderTotalAmount());
		target.setTotalRefundedAmount(source.getTotalRefundedAmount());
		target.setTotalWeight(source.getTotalWeight());
		target.setGiftCardCost(source.getGiftCardCost());
		target.setDimensionalWeight(source.getDimensionalWeight());
		target.setPickUpByMe(source.isPickUpByMe());
		target.setGiftCardOrder(source.isGiftCardOrder());
		target.setPickUpPersonFirstName(source.getPickUpPersonFirstName());
		target.setPickUpPersonLastName(source.getPickUpPersonLastName());
		target.setPickUpPersonPhone(source.getPickUpPersonPhone());
		target.setPickUpPersonEmail(source.getPickUpPersonEmail());
		target.setStatusUpdate(source.isStatusUpdate());
		target.setDeliveryNotes(source.getDeliveryNotes());
		target.setActualRentalStartDate(source.getActualRentalEndDate());
		target.setActualRentalEndDate(source.getActualRentalEndDate());
		target.setIsRentalOrder(source.getIsRentalOrder());
		target.setGiftCardAmount(source.getGiftCardAmount());
		target.setGiftCardAvailableAmount(source.getGiftCardAvailableAmount());
		target.setGiftCardAmountTransactions(source.getGiftCardAmountTransactions().stream().collect(Collectors.joining(", ")));
		if (source.getOrderType() != null)
		{
			target.setOrderType(source.getOrderType().getCode());
		}
		target.setIsAuthorised(source.getIsAuthorised());
		target.setIsCaptured(source.getIsCaptured());
		target.setIsAuthorizationVoided(source.getIsAuthorizationVoided());
		target.setPoNumber(source.getPoNumber());
		target.setPoNotes(source.getPoNotes());
		target.setIsExtendedOrder(source.getIsExtendedOrder());
		if (source.getExtendOrderStatus() != null)
		{
			target.setExtendOrderStatus(source.getExtendOrderStatus().getCode());
		}
		target.setExtendRentalEndDate(source.getExtendRentalEndDate());
		target.setTotalExtendDays(source.getTotalExtendDays());
		target.setExtendedOrderCopyList(
				source.getExtendedOrderCopyList().stream().map(AbstractOrderModel::getCode).collect(Collectors.joining(", ")));
		if (source.getQuoteReference() != null)
		{
			target.setQuotereference(source.getQuoteReference().getCode());
		}
		//target.setPotentiallyfraudulent(source.getPotentiallyFraudulent());
		target.setIsordertaxexempt(source.getIsOrderTaxExempt());
		target.setAvalarataxcalculated(source.getAvalaraTaxCalculated());
		if (source.getVerificationStatus() != null)
		{
			target.setVerificationstatus(source.getVerificationStatus().getCode());
		}
		target.setRentalstartdate(source.getRentalStartDate());
		target.setRentalenddate(source.getRentalEndDate());
		target.setTotaldamagewaivercost(source.getTotalDamageWaiverCost());
		if (source.getAppliedCouponCodes() != null)
		{
			target.setAppliedcouponcodes(source.getAppliedCouponCodes().stream().collect(Collectors.joining(", ")));
		}
		target.setQuotediscountvaluesinternal(source.getQuoteDiscountValuesInternal());
		target.setGuid(source.getGuid());
		if (source.getStore() != null)
		{
			target.setStore(source.getStore().getUid());
		}
		if (source.getSite() != null)
		{
			target.setSite(source.getSite().getUid());
		}
		if (source.getPreviousDeliveryMode() != null)
		{
			target.setPreviousdeliverymode(source.getPreviousDeliveryMode().getCode());
		}
		if (source.getEurope1PriceFactory_UDG() != null)
		{
			target.setEurope1pricefactory_udg(source.getEurope1PriceFactory_UDG().getCode());
		}
		if (source.getEurope1PriceFactory_UPG() != null)
		{
			target.setEurope1pricefactory_upg(source.getEurope1PriceFactory_UPG().getCode());
		}
		if (source.getEurope1PriceFactory_UTG() != null)
		{
			target.setEurope1pricefactory_utg(source.getEurope1PriceFactory_UTG().getCode());
		}
		if (source.getCurrency() != null)
		{
			target.setCurrency(source.getCurrency().getIsocode());
		}
		if (source.getDeliveryAddress() != null)
		{
			target.setDeliveryAddressid(source.getDeliveryAddress().getAddressID());
		}
		target.setDeliverycost(source.getDeliveryCost());
		if (source.getDeliveryMode() != null)
		{
			target.setDeliveryModeCode(source.getDeliveryMode().getCode());
		}
		target.setDescription(source.getDescription());
		target.setExpirationTime(source.getExpirationTime());
		target.setGlobaldiscountvaluesinternal(source.getGlobalDiscountValuesInternal());
		target.setName(source.getName());
		target.setNet(source.getNet());
		if (source.getPaymentAddress() != null)
		{
			target.setPaymentaddress(source.getPaymentAddress().getAddressID());
		}
		target.setPaymentcost(source.getPaymentCost());
		if (source.getPaymentInfo() != null)
		{
			target.setPaymentinfo(source.getPaymentInfo().getCode());
		}
		if (source.getPaymentMode() != null)
		{
			target.setPaymentmode(source.getPaymentMode().getCode());
		}
		if (source.getPaymentStatus() != null)
		{
			target.setPaymentstatus(source.getPaymentStatus().getCode());
		}
		if (source.getExportStatus() != null)
		{
			target.setExportstatus(source.getExportStatus().getCode());
		}
		if (source.getEntryGroups() != null)
		{
			target.setEntrygroups(source.getEntryGroups().stream().map(EntryGroup::getLabel).collect(Collectors.joining(", ")));
		}
		if (source.getUser() != null)
		{
			target.setUserId(source.getUser().getUid());
		}
		if (source.getDeliveryMode() != null)
		{
			target.setDeliveryModeCode(source.getDeliveryMode().getCode());
		}
		target.setTotaltaxvaluesinternal(source.getTotalTaxValuesInternal());
		target.setShopperIp(source.getShopperIp());
		target.setPrimaryKey(source.getPk().toString());
	}
}
