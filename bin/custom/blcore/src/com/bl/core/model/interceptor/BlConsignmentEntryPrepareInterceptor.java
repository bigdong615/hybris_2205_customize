package com.bl.core.model.interceptor;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;


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
						if (BooleanUtils.negate(ItemBillingChargeTypeEnum.valueOf("MISSING_CHARGE").equals(charge.getBillChargeType())))
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
		}
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
		if (BooleanUtils.negate(interceptorContext.isNew(consignmentEntryModel))
				&& interceptorContext.isModified(consignmentEntryModel, ConsignmentEntryModel.SERIALPRODUCTS))
		{
			final Map<String, List<BlItemsBillingChargeModel>> billingCharges = consignmentEntryModel.getSerialProducts().stream()
					.collect(Collectors.toMap(BlProductModel::getCode, serial -> new ArrayList<BlItemsBillingChargeModel>()));
			consignmentEntryModel.setBillingCharges(billingCharges);
		}
	}

}
