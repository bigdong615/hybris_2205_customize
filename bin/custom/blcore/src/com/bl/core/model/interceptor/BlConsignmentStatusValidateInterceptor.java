package com.bl.core.model.interceptor;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;
import de.hybris.platform.servicelayer.user.UserService;

import javax.annotation.Resource;

import com.bl.constants.BlInventoryScanLoggingConstants;


/**
 * This validator used to validate consignment status
 *
 * @author Aditi Sharma
 */
public class BlConsignmentStatusValidateInterceptor implements ValidateInterceptor<ConsignmentModel>
{

	/**
	 * This method is used to validate consignment by its status
	 */
	@Resource
	private UserService userService;

	/**
	 * method will validate consignment status based on user group
	 */
	@Override
	public void onValidate(final ConsignmentModel consignmentModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		boolean isCsAgent = false;
		final UserModel currentUser = userService.getCurrentUser();

		for (final PrincipalGroupModel userGroup : currentUser.getGroups())
		{
			if (BlInventoryScanLoggingConstants.CUSTOMER_SUPPORT_AGENT_GROUP.equals(currentUser.getUid()))
			{
				isCsAgent = true;
				break;
			}
		}

		if (isCsAgent && ConsignmentStatus.SHIPPED.equals(consignmentModel.getItemModelContext().getOriginalValue("status")))
		{
			throw new InterceptorException("You can not modify shipped consignment");
		}
	}
}
