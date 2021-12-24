package com.bl.core.model.interceptor;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.core.model.user.CustomerModel;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.esp.dto.orderexceptions.data.OrderExceptionsExtraData;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;

import de.hybris.platform.servicelayer.model.ItemModelContextImpl;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * The Class BlConsignmentEntryPrepareInterceptor used to intercept the model and modify the attributes before saving
 * the data.
 *
 * @author Ravikumar
 *
 */
public class BlConsignmentEntryPrepareInterceptor implements PrepareInterceptor<ConsignmentEntryModel>
{
	private static final Logger LOG = Logger.getLogger(BlConsignmentEntryPrepareInterceptor.class);
	private static final String EVENT_EXCEPTION_MSG = "Failed to trigger exception missing/broken Event";
	private DefaultBlESPEventService blEspEventService;

	@Resource(name = "defaultBlUserService")
	private DefaultBlUserService defaultBlUserService;
	
	private BlConsignmentEntryService blConsignmentEntryService;

	@Override
	public void onPrepare(final ConsignmentEntryModel consignmentEntryModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		validateParameterNotNull(consignmentEntryModel,
				"ERROR : BlConsignmentEntryPrepareInterceptor : Parameter ConsignmentEntryModel is NULL");
		modifySerialCodeOnBillingChargesMap(consignmentEntryModel, interceptorContext);
		validateBillingCharges(consignmentEntryModel, interceptorContext);
		triggerExceptionBrokenOrMissingEvent(consignmentEntryModel, interceptorContext);
		doChangePriorityStatus(consignmentEntryModel, interceptorContext); //BL-822 AC.2
		addSerialAndOrderCodeOnItemBillingCharge(consignmentEntryModel, interceptorContext);
	}

	/**
	 * Validate billing charges if modified.
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 * @param interceptorContext
	 *           the interceptor context
	 */
	private void validateBillingCharges(final ConsignmentEntryModel consignmentEntryModel,
			final InterceptorContext interceptorContext)
	{
		final Map<String, List<BlItemsBillingChargeModel>> billingCharges = consignmentEntryModel.getBillingCharges();
		final List<BlProductModel> serialProducts = consignmentEntryModel.getSerialProducts();
		if (interceptorContext.isModified(consignmentEntryModel, ConsignmentEntryModel.BILLINGCHARGES)
				&& CollectionUtils.isNotEmpty(serialProducts) && MapUtils.isNotEmpty(billingCharges))
		{
			final Map<String, List<BlItemsBillingChargeModel>> validatedBillingCharges = new HashMap<>();
			billingCharges.forEach((serialCode, listOfBillingCharges) -> {
				if (checkUnboxedStatus(serialCode, serialProducts))
				{
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
							"Serial Status is UNBOXED for Serial Code : {} , Clearing all billing charges against this serial",
							serialCode);
					final List<BlItemsBillingChargeModel> updatedCharge = new ArrayList<>();
					listOfBillingCharges.forEach(charge -> {
						if (!ItemBillingChargeTypeEnum.valueOf(BlCoreConstants.MISSING_CHARGE).equals(charge.getBillChargeType()))
						{
							updatedCharge.add(charge);
						}
					});

					validatedBillingCharges.put(serialCode, updatedCharge);
				}
				else
				{
					validatedBillingCharges.put(serialCode, listOfBillingCharges);
				}
			});
			consignmentEntryModel.setBillingCharges(validatedBillingCharges);
			setTotalAmountPastDue(consignmentEntryModel, interceptorContext);
		}
	}

	/**
	 * It updates the total amount past due when new billing charges are added
	 * @param consignmentEntryModel the consignment entry model
	 * @param interceptorContext interceptor context
	 */
	private void setTotalAmountPastDue(final ConsignmentEntryModel consignmentEntryModel, final
			InterceptorContext interceptorContext) {
		final Object originalBillingCharges = getInitialValue(consignmentEntryModel, BlCoreConstants.BILLING_CHARGES);
		if(Objects.nonNull(originalBillingCharges)) {
			final Map<String, List<BlItemsBillingChargeModel>> billingCharges = (Map<String, List<BlItemsBillingChargeModel>>) originalBillingCharges;
			final Map<String, List<BlItemsBillingChargeModel>> currentBillingCharges = consignmentEntryModel
					.getBillingCharges();
			currentBillingCharges.entrySet().forEach(billingCharge -> {
				if (billingCharge.getValue() != null && billingCharges.get(billingCharge.getKey()) != null
						&& billingCharge.getValue().size() > billingCharges.get(billingCharge.getKey()).size())
				{
					final List<BlItemsBillingChargeModel> charges = billingCharges
							.get(billingCharge.getKey());
					final List<BlItemsBillingChargeModel> currentCharges = billingCharge.getValue();
					final List<BlItemsBillingChargeModel> tempCurrentCharges = new ArrayList<>();
					tempCurrentCharges.addAll(currentCharges);
					tempCurrentCharges.removeAll(charges);
					final CustomerModel customerModel = (CustomerModel) consignmentEntryModel.getConsignment()
							.getOrder().getUser();
					final BigDecimal newlyAddedCharges = tempCurrentCharges.stream()
							.map(BlItemsBillingChargeModel::getChargedAmount)
							.reduce(BigDecimal.ZERO, BigDecimal::add);
					BigDecimal totalAmountPastDue =
							Objects.isNull(customerModel.getTotalAmountPastDue()) ? BigDecimal.ZERO :
									customerModel.getTotalAmountPastDue();
					totalAmountPastDue = totalAmountPastDue.add(newlyAddedCharges);
					customerModel.setTotalAmountPastDue(totalAmountPastDue);
					setOutstandingBills(customerModel, tempCurrentCharges);
					interceptorContext.getModelService().save(customerModel);
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
							"Total amount past due : {} updated for the customer {} ",
							totalAmountPastDue, customerModel.getUid());
				}
			});
		}
	}

	/**
	 * It sets the due bill payment model on customer
	 * @param customerModel the customer model
	 * @param tempCurrentCharges the billing charges
	 */
	private void setOutstandingBills(final CustomerModel customerModel,
			final List<BlItemsBillingChargeModel> tempCurrentCharges) {
		final List<BlItemsBillingChargeModel> outstandingBills = new ArrayList<>(customerModel.getOutstandingBills());
		outstandingBills.addAll(tempCurrentCharges);
		customerModel.setOutstandingBills(outstandingBills);
	}

	/**
	 * It gets the initial value of the attribute before update
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 */
	private Object getInitialValue(final ConsignmentEntryModel consignmentEntryModel, final String billingCharges) {
		final ItemModelContextImpl itemModelCtx = (ItemModelContextImpl) consignmentEntryModel
				.getItemModelContext();
		return itemModelCtx.exists() ? itemModelCtx.getOriginalValue(billingCharges) : null;
	}

	/**
	 * It triggers Exception Broken/Missing Event.
	 * @param consignmentEntryModel the ConsignmentEntryModel
	 * @param interceptorContext    InterceptorContext
	 */
	private void triggerExceptionBrokenOrMissingEvent(
			final ConsignmentEntryModel consignmentEntryModel,
			final InterceptorContext interceptorContext) {
		final List<BlProductModel> serialProducts = consignmentEntryModel.getSerialProducts();
		if (BooleanUtils.isTrue(defaultBlUserService.isTechEngOrRepairUser()) && interceptorContext
				.isModified(consignmentEntryModel, ConsignmentEntryModel.BILLINGCHARGES)
				&& CollectionUtils.isNotEmpty(serialProducts)) {
			final Map<String, List<BlItemsBillingChargeModel>> modifiedBillingCharges = Maps
					.newHashMap(consignmentEntryModel.getBillingCharges());
			final Map<String, List<BlItemsBillingChargeModel>> newModifiedBillingCharges = Maps.newHashMap();
			final Map<String, List<BlItemsBillingChargeModel>> previousChangedBillingChargesList = getPreviousChangedBillingChargesList(
					consignmentEntryModel);
			final OrderModel orderModel = (OrderModel) consignmentEntryModel.getConsignment().getOrder();
			modifiedBillingCharges.forEach((serialCode, charges) -> {
				if (previousChangedBillingChargesList.containsKey(serialCode)) {
					final List<BlItemsBillingChargeModel> previousCharges = previousChangedBillingChargesList
							.get(serialCode);
					List<BlItemsBillingChargeModel> updatedCharges = Lists
							.newArrayList(modifiedBillingCharges.get(serialCode));
					updatedCharges.removeIf(previousCharges::contains);
					newModifiedBillingCharges.put(serialCode, updatedCharges);
				}
			});

			if (MapUtils.isNotEmpty(newModifiedBillingCharges)) {
				addModifiedChargesToNewList(newModifiedBillingCharges, orderModel);
			}
		}

		if(interceptorContext
				.isModified(consignmentEntryModel, ConsignmentEntryModel.BILLINGCHARGES)) {
			final OrderModel orderModel = (OrderModel) consignmentEntryModel.getConsignment().getOrder();
			orderModel.setOrderModifiedDate(new Date());
			orderModel.setUpdatedTime(new Date());
			interceptorContext.getModelService().save(orderModel);
			interceptorContext.getModelService().refresh(orderModel);
		}
	}

	/**
	 * If any new charge has been added then adding that charge to its corresponding new list and
	 * invoke trigger event methods.
	 *
	 * @param newModifiedBillingCharges
	 * @param orderModel
	 */
	private void addModifiedChargesToNewList(
			final Map<String, List<BlItemsBillingChargeModel>> newModifiedBillingCharges,
			final OrderModel orderModel) {
		newModifiedBillingCharges.forEach((serialCode, updatedChargesList) ->
		{
			if (CollectionUtils.isNotEmpty(updatedChargesList)) {
				final List<BlItemsBillingChargeModel> missingChargeList = Lists.newArrayList();
				final List<BlItemsBillingChargeModel> lateChargeList = Lists.newArrayList();
				final List<BlItemsBillingChargeModel> repairChargeList = Lists.newArrayList();

				updatedChargesList.forEach(charge -> {
					if (BooleanUtils.isFalse(charge.isBillPaid())) {
						switch (charge.getBillChargeType().getCode()) {
							case BlCoreConstants.LATE_CHARGE:
								lateChargeList.add(charge);
								break;
							case BlCoreConstants.REPAIR_CHARGE:
								repairChargeList.add(charge);
								break;
							case BlCoreConstants.MISSING_CHARGE:
								missingChargeList.add(charge);
								break;
							default:
								BlLogger.logMessage(LOG, Level.INFO,
										"Billing charge type {} is not eligible for ESP event",
										charge.getBillChargeType().getCode());
								break;
						}
					}
				});
				orderModel.setOrderModifiedDate(new Date());
				orderModel.setUpdatedTime(new Date());
				eventTriggerForLateCharge(orderModel, serialCode, lateChargeList);
				eventTriggerForRepairAndMissingCharge(orderModel, serialCode, repairChargeList,
						missingChargeList);
				eventTriggerForRepairCharge(orderModel, serialCode, repairChargeList, missingChargeList);
				eventTriggerForMissingCharge(orderModel, serialCode, repairChargeList, missingChargeList);
			}
		});
	}

	/**
	 * It triggers exception missing/broken event for late charge.
	 *
	 * @param orderModel
	 * @param serialCode
	 * @param lateChargeList
	 */
	private void eventTriggerForLateCharge(final OrderModel orderModel, final String serialCode,
			final List<BlItemsBillingChargeModel> lateChargeList) {
		if (CollectionUtils.isNotEmpty(lateChargeList)) {
      eventTriggerForCharge(orderModel, serialCode, lateChargeList);
		}
	}

	/**
	 * It triggers exception missing/broken event for repair and missing charges.
	 *
	 * @param orderModel
	 * @param serialCode
	 * @param repairChargeList
	 * @param missingChargeList
	 */
	private void eventTriggerForRepairAndMissingCharge(final OrderModel orderModel,
			final String serialCode,
			final List<BlItemsBillingChargeModel> repairChargeList,
			final List<BlItemsBillingChargeModel> missingChargeList) {
		final OrderExceptionsExtraData orderExceptionsExtraData = new OrderExceptionsExtraData();
		if (CollectionUtils.isNotEmpty(repairChargeList) && CollectionUtils
				.isNotEmpty(missingChargeList)) {
			BigDecimal chargedAmount = BigDecimal.ZERO;
			StringBuilder unPaidBillNotes = new StringBuilder();

			//consolidated data for repair charge.
			for (BlItemsBillingChargeModel blItemsBillingChargeModel : repairChargeList) {
				chargedAmount = chargedAmount.add(blItemsBillingChargeModel.getChargedAmount());
				unPaidBillNotes.append(blItemsBillingChargeModel.getUnPaidBillNotes()).append(StringUtils.SPACE);
			}

			//consolidated data for missing charge.
			for (BlItemsBillingChargeModel blItemsBillingChargeModel : missingChargeList) {
				chargedAmount = chargedAmount.add(blItemsBillingChargeModel.getChargedAmount());
				unPaidBillNotes.append(blItemsBillingChargeModel.getUnPaidBillNotes()).append(StringUtils.SPACE);
			}
			orderExceptionsExtraData.setSerialCode(serialCode);
			orderExceptionsExtraData.setTotalChargedAmount(chargedAmount.toString());
			orderExceptionsExtraData.setAllUnPaidBillNotes(unPaidBillNotes.toString());

			try {
				getBlEspEventService().sendOrderMissingBrokenLateEvent(orderModel, orderExceptionsExtraData);
			} catch (final Exception exception) {
				BlLogger.logMessage(LOG, Level.ERROR, EVENT_EXCEPTION_MSG,
						exception);
			}
		}
	}

	/**
	 * It triggers exception missing/broken event for repair charge.
	 *
	 * @param orderModel
	 * @param serialCode
	 * @param repairChargeList
	 * @param missingChargeList
	 */
	private void eventTriggerForRepairCharge(final OrderModel orderModel, final String serialCode,
			final List<BlItemsBillingChargeModel> repairChargeList,
			final List<BlItemsBillingChargeModel> missingChargeList) {
		if (CollectionUtils.isNotEmpty(repairChargeList) && CollectionUtils
				.isEmpty(missingChargeList)) {
      eventTriggerForCharge(orderModel, serialCode, repairChargeList);
		}
	}

	/**
	 * It triggers exception missing/broken event for missing charge.
	 *
	 * @param orderModel
	 * @param serialCode
	 * @param repairChargeList
	 * @param missingChargeList
	 */
	private void eventTriggerForMissingCharge(final OrderModel orderModel, final String serialCode,
			final List<BlItemsBillingChargeModel> repairChargeList,
			final List<BlItemsBillingChargeModel> missingChargeList) {

		if (CollectionUtils.isEmpty(repairChargeList) && CollectionUtils
				.isNotEmpty(missingChargeList)) {
      eventTriggerForCharge(orderModel, serialCode, missingChargeList);
    }
	}

  /**
   * It is responsible for triggering Exception broken/missing ESP event with consolidated data.
   *
   * @param orderModel
   * @param serialCode
   * @param billingChargeList
   */
  private void eventTriggerForCharge(final OrderModel orderModel, final String serialCode,
      final List<BlItemsBillingChargeModel> billingChargeList) {
    final OrderExceptionsExtraData orderExceptionsExtraData = new OrderExceptionsExtraData();
    BigDecimal chargedAmount = BigDecimal.ZERO;
    final StringBuilder unPaidBillNotes = new StringBuilder();
    //consolidated data for billing charge.
    for (BlItemsBillingChargeModel blItemsBillingChargeModel : billingChargeList) {
      chargedAmount = chargedAmount.add(blItemsBillingChargeModel.getChargedAmount());
      unPaidBillNotes.append(blItemsBillingChargeModel.getUnPaidBillNotes()).append(
					StringUtils.SPACE);
    }
    orderExceptionsExtraData.setSerialCode(serialCode);
    orderExceptionsExtraData.setTotalChargedAmount(chargedAmount.toString());
    orderExceptionsExtraData.setAllUnPaidBillNotes(unPaidBillNotes.toString());
    try {
      getBlEspEventService().sendOrderMissingBrokenLateEvent(orderModel, orderExceptionsExtraData);
    } catch (final Exception exception) {
      BlLogger.logMessage(LOG, Level.ERROR, EVENT_EXCEPTION_MSG,
          exception);
    }
  }

  /**
	 * It fetches billing charges list.
	 * @param consignmentEntryModel
	 * @return
	 */
	private Map<String, List<BlItemsBillingChargeModel>> getPreviousChangedBillingChargesList(final ConsignmentEntryModel consignmentEntryModel)
	{
		final Object previousValue = consignmentEntryModel.getItemModelContext()
				.getOriginalValue(ConsignmentEntryModel.BILLINGCHARGES);
		if (previousValue instanceof Map)
		{
			return  Maps.newHashMap((Map<String, List<BlItemsBillingChargeModel>>)previousValue);
		}
		return Maps.newHashMap();
	}

	/**
	 * Checks the serial status to unboxed status.
	 *
	 * @param serialCode
	 *           the serial code
	 * @param serialProducts
	 *           the serial products
	 * @return true, if successful
	 */
	private boolean checkUnboxedStatus(final String serialCode, final List<BlProductModel> serialProducts)
	{
		final BlProductModel serial = serialProducts.stream().filter(serialItem -> serialCode.equals(serialItem.getCode()))
				.findAny().orElse(null);
		if (serial instanceof BlSerialProductModel)
		{
			return SerialStatusEnum.UNBOXED.equals(((BlSerialProductModel) serial).getSerialStatus());
		}
		return Boolean.FALSE;
	}

	/**
	 * Modify serial code on billing charges map if Serial is modified on consignment entry.
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 * @param interceptorContext
	 *           the interceptor context
	 */
	private void modifySerialCodeOnBillingChargesMap(final ConsignmentEntryModel consignmentEntryModel,
			final InterceptorContext interceptorContext)
	{
		if (!interceptorContext.isNew(consignmentEntryModel)
				&& interceptorContext.isModified(consignmentEntryModel, ConsignmentEntryModel.SERIALPRODUCTS))
		{
			final Map<String, List<BlItemsBillingChargeModel>> billingCharges = Maps.newHashMap();
			consignmentEntryModel.getSerialProducts().forEach(serial -> {
				if (!billingCharges.containsKey(serial.getCode()))
				{
					billingCharges.put(serial.getCode(), new ArrayList<BlItemsBillingChargeModel>());
				}
			});
			consignmentEntryModel.setBillingCharges(billingCharges);
		}
	}

	/**
	 * Do change priority status of serial if modified.
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 * @param interceptorContext
	 *           the interceptor context
	 */
	private void doChangePriorityStatus(final ConsignmentEntryModel consignmentEntryModel,
			final InterceptorContext interceptorContext)
	{
		if (!interceptorContext.isNew(consignmentEntryModel)
				&& interceptorContext.isModified(consignmentEntryModel, ConsignmentEntryModel.SERIALPRODUCTS)
				&& isEligibleToChangeSerialPriorityStatus(consignmentEntryModel))
		{
			final List<Object> previousChangedSerialsList = getPreviousChangedSerialsList(consignmentEntryModel);
			if (CollectionUtils.isNotEmpty(previousChangedSerialsList))
			{
				final List<BlProductModel> updatedSerialProducts = consignmentEntryModel.getSerialProducts();
				previousChangedSerialsList.removeIf(updatedSerialProducts::contains);
				previousChangedSerialsList.forEach(serial -> checkAndChangePriorityStatusOnSerial(serial, interceptorContext));
			}
		}
	}

	/**
	 * Checks if is eligible to change serial priority status.
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 * @return true, if is eligible to change serial priority status
	 */
	private boolean isEligibleToChangeSerialPriorityStatus(final ConsignmentEntryModel consignmentEntryModel)
	{
		final ConsignmentModel consignment = consignmentEntryModel.getConsignment();
		return Objects.nonNull(consignment) && Objects.nonNull(consignment.getOptimizedShippingStartDate())
				&& DateUtils.isSameDay(consignment.getOptimizedShippingStartDate(), new Date());
	}

	/**
	 * Gets the previous changed serials list.
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 * @return the previous changed serials list
	 */
	private List<Object> getPreviousChangedSerialsList(final ConsignmentEntryModel consignmentEntryModel)
	{
		final Object previousValue = consignmentEntryModel.getItemModelContext()
				.getOriginalValue(ConsignmentEntryModel.SERIALPRODUCTS);
		if (previousValue instanceof List)
		{
			return Lists.newArrayList((List) previousValue);
		}
		return Collections.emptyList();
	}

	/**
	 * Check and change priority status on serial.
	 *
	 * @param entryItem
	 *           the entry item
	 * @param interceptorContext
	 *           the interceptor context
	 */
	private void checkAndChangePriorityStatusOnSerial(final Object entryItem, final InterceptorContext interceptorContext)
	{
		if (entryItem instanceof BlSerialProductModel)
		{
			final BlSerialProductModel serialItem = ((BlSerialProductModel) entryItem);
			if (serialItem.isDirtyPriorityStatus())
			{
				serialItem.setDirtyPriorityStatus(Boolean.FALSE);
				interceptorContext.getModelService().save(serialItem);
				interceptorContext.getModelService().refresh(serialItem);
			}
		}
	}
	
	/**
	 * Adds the serial and order code on item billing charge.
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 * @param interceptorContext
	 *           the interceptor context
	 */
	private void addSerialAndOrderCodeOnItemBillingCharge(final ConsignmentEntryModel consignmentEntryModel,
			final InterceptorContext interceptorContext)
	{
		try
		{
			if (interceptorContext.isModified(consignmentEntryModel, ConsignmentEntryModel.BILLINGCHARGES))
			{
				getBlConsignmentEntryService().assignSerialAndOrderCodeOnBillingCharges(consignmentEntryModel);
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY,
					"Exception Occured while setting order code and serial code on Item billing charges", exception);
		}
	}

	public DefaultBlESPEventService getBlEspEventService() {
		return blEspEventService;
	}

	public void setBlEspEventService(DefaultBlESPEventService blEspEventService) {
		this.blEspEventService = blEspEventService;
	}

	/**
	 * @return the blConsignmentEntryService
	 */
	public BlConsignmentEntryService getBlConsignmentEntryService()
	{
		return blConsignmentEntryService;
	}

	/**
	 * @param blConsignmentEntryService the blConsignmentEntryService to set
	 */
	public void setBlConsignmentEntryService(BlConsignmentEntryService blConsignmentEntryService)
	{
		this.blConsignmentEntryService = blConsignmentEntryService;
	}
}
