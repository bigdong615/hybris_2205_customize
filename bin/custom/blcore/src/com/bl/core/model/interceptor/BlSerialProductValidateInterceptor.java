package com.bl.core.model.interceptor;

import com.bl.core.enums.SerialStatusEnum;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;

import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.Objects;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ConditionRatingValueEnum;
import com.bl.core.model.BlSerialProductModel;
import java.util.stream.Collectors;


/**
 * Validator Interceptor for BlSerialProductModel to verify the values set on attribute before saving the model
 *
 * @author Ravikumar
 *
 */
public class BlSerialProductValidateInterceptor implements ValidateInterceptor<BlSerialProductModel>
{

	private BaseStoreService baseStoreService;
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void onValidate(final BlSerialProductModel blSerialProductModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		if (Objects.nonNull(blSerialProductModel) && !interceptorContext.isNew(blSerialProductModel))
		{
			validateConditionalRatings(blSerialProductModel, interceptorContext);
			if (blSerialProductModel.getIsBufferedInventory().booleanValue()) {
				final BaseStoreModel baseStore = getBaseStoreService().getBaseStoreForUid(
						BlCoreConstants.BASE_STORE_ID);
				if (null != baseStore && null != baseStore.getMinQtyForBufferInventory() &&
						baseStore.getMinQtyForBufferInventory() > 0) {
					final int minQtyForBufferInv = baseStore.getMinQtyForBufferInventory();
					final int totalSerialProducts = blSerialProductModel.getBlProduct().getSerialProducts()
							.stream().filter(serialProduct -> null != serialProduct.getSerialStatus()
									&& serialProduct.getSerialStatus().equals(SerialStatusEnum.ACTIVE))
							.collect(Collectors.toList()).size();
					if (minQtyForBufferInv > totalSerialProducts) {
						throw new InterceptorException(
								"Can't mark this serial product as buffer inventory");
					}
				}
			}
		}
	}

	/**
	 * Validate conditional ratings on serials.
	 *
	 * @param blSerialProductModel
	 *           the bl serial product model
	 * @param interceptorContext
	 *           the interceptor context
	 * @throws InterceptorException
	 *            the interceptor exception
	 */
	private void validateConditionalRatings(final BlSerialProductModel blSerialProductModel,
			final InterceptorContext interceptorContext) throws InterceptorException
	{
		if (isFunctionalAndConditionIsModified(blSerialProductModel, interceptorContext))
		{
			throw new InterceptorException("Cosmetic and Functional Condition Rating cannot be 0 (zero)");
		}
		else if (isModifiedAttributeAndZeroSelected(blSerialProductModel, BlSerialProductModel.COSMETICRATING,
				blSerialProductModel.getCosmeticRating(), interceptorContext))
		{
			throw new InterceptorException("Cosmetic Condition Rating cannot be 0 (zero)");
		}
		else if (isModifiedAttributeAndZeroSelected(blSerialProductModel, BlSerialProductModel.FUNCTIONALRATING,
				blSerialProductModel.getFunctionalRating(), interceptorContext))
		{
			throw new InterceptorException("Functional Condition Rating cannot be 0 (zero)");
		}
	}

	/**
	 * Checks if both functional and condition is modified with value zero.
	 *
	 * @param blSerialProductModel
	 *           the bl serial product model
	 * @param interceptorContext
	 *           the interceptor context
	 * @return true, if is functional and condition is modified
	 */
	private boolean isFunctionalAndConditionIsModified(final BlSerialProductModel blSerialProductModel,
			final InterceptorContext interceptorContext)
	{
		return isModifiedAttributeAndZeroSelected(blSerialProductModel, BlSerialProductModel.COSMETICRATING,
				blSerialProductModel.getCosmeticRating(), interceptorContext)
				&& isModifiedAttributeAndZeroSelected(blSerialProductModel, BlSerialProductModel.FUNCTIONALRATING,
						blSerialProductModel.getFunctionalRating(), interceptorContext);
	}

	/**
	 * Checks if attribute is modified and value is selected as zero.
	 *
	 * @param blSerialProductModel
	 *           the bl serial product model
	 * @param attributeName
	 *           the attribute name
	 * @param value
	 *           the value
	 * @param interceptorContext
	 *           the interceptor context
	 * @return true, if is modified attribute and zero selected
	 */
	private boolean isModifiedAttributeAndZeroSelected(final BlSerialProductModel blSerialProductModel, final String attributeName,
			final ConditionRatingValueEnum value, final InterceptorContext interceptorContext)
	{
		return interceptorContext.isModified(blSerialProductModel, attributeName) && value.getCode().equals(BlCoreConstants.ZERO);
	}

	public BaseStoreService getBaseStoreService() {
		return baseStoreService;
	}

	public void setBaseStoreService(BaseStoreService baseStoreService) {
		this.baseStoreService = baseStoreService;
	}
}
