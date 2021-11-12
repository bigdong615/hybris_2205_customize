package com.bl.core.model.interceptor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ConditionRatingValueEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;


/**
 * Validator Interceptor for BlSerialProductModel to verify the values set on attribute before saving the model
 *
 * @author Ravikumar
 *
 */
public class BlSerialProductValidateInterceptor implements ValidateInterceptor<BlSerialProductModel>
{

	private BlProductDao productDao;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void onValidate(final BlSerialProductModel blSerialProductModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		if (Objects.nonNull(blSerialProductModel))
		{
			if(!interceptorContext.isNew(blSerialProductModel)) {
				validateConditionalRatings(blSerialProductModel, interceptorContext);
			}

			if (StringUtils.isNotBlank(blSerialProductModel.getBarcode()) && interceptorContext.isModified(blSerialProductModel,
					BlSerialProductModel.BARCODE)) {
				final BlSerialProductModel serialProductModel = getProductDao().getSerialByBarcode(blSerialProductModel.getBarcode());
				if(null != serialProductModel && !serialProductModel.getProductId().equals(blSerialProductModel.getProductId())) {
					throw new InterceptorException("Barcode should be unique!");
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

	public BlProductDao getProductDao() {
		return productDao;
	}

	public void setProductDao(BlProductDao productDao) {
		this.productDao = productDao;
	}
}
