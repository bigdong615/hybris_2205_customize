package com.bl.core.model.interceptor;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.core.model.user.CustomerModel;
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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

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

	@Override
	public void onPrepare(final ConsignmentEntryModel consignmentEntryModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		validateParameterNotNull(consignmentEntryModel,
				"ERROR : BlConsignmentEntryPrepareInterceptor : Parameter ConsignmentEntryModel is NULL");
		modifySerialCodeOnBillingChargesMap(consignmentEntryModel, interceptorContext);
		validateBillingCharges(consignmentEntryModel, interceptorContext);
		doChangePriorityStatus(consignmentEntryModel, interceptorContext); //BL-822 AC.2
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
		final ItemModelContextImpl itemModelCtx = (ItemModelContextImpl) consignmentEntryModel
				.getItemModelContext();
		final Map<String, List<BlItemsBillingChargeModel>> billingCharges = itemModelCtx.getOriginalValue("billingCharges");
		final Map<String, List<BlItemsBillingChargeModel>> currentBillingCharges = consignmentEntryModel.getBillingCharges();
		currentBillingCharges.entrySet().forEach(billingCharge -> {
			if(billingCharge.getValue().size() > billingCharges.get(billingCharge.getKey()).size()) {
				final List<BlItemsBillingChargeModel> charges = billingCharges.get(billingCharge.getKey());
				final List<BlItemsBillingChargeModel> currentCharges = billingCharge.getValue();
				currentCharges.removeAll(charges);
				final CustomerModel customerModel = (CustomerModel) consignmentEntryModel.getConsignment().getOrder().getUser();
				final BigDecimal newlyAddedCharges = currentCharges.stream().map(BlItemsBillingChargeModel::getChargedAmount)
						.reduce(BigDecimal.ZERO, BigDecimal::add);
				BigDecimal totalAmountPastDue = Objects.isNull(customerModel.getTotalAmountPastDue()) ? BigDecimal.ZERO :
				customerModel.getTotalAmountPastDue();
				totalAmountPastDue = totalAmountPastDue.add(newlyAddedCharges);
				customerModel.setTotalAmountPastDue(totalAmountPastDue);
				interceptorContext.getModelService().save(customerModel);
			}
		});
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

}
