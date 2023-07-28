package com.bl.core.coupon.impl;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.couponservices.CouponServiceException;
import de.hybris.platform.couponservices.dao.CouponDao;
import de.hybris.platform.couponservices.model.AbstractCouponModel;
import de.hybris.platform.couponservices.model.MultiCodeCouponModel;
import de.hybris.platform.couponservices.model.SingleCodeCouponModel;
import de.hybris.platform.couponservices.redemption.strategies.CouponRedemptionStrategy;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.couponservices.services.impl.DefaultCouponManagementService;

import java.util.Objects;

import org.apache.commons.lang.BooleanUtils;

import com.bl.core.coupon.BlCouponManagementService;


/**
 * This class is created to redeem the extend order
 *
 * @author Manikandan
 */
public class DefaultBlCouponManagementService extends DefaultCouponManagementService implements BlCouponManagementService
{

	private CouponDao couponDao;

	/**
	 * This method created to redeem the coupon for extend order
	 */
	@Override
	public CouponResponse redeemForExtendOrder(final String couponCode, final OrderModel order)
	{
		final CouponResponse response = this.verifyCouponCode(couponCode, order);
		if (BooleanUtils.isTrue(response.getSuccess()))
		{
			this.createCouponRedemption(couponCode, order);
		}
		else
		{
			throw new CouponServiceException(response.getMessage());
		}
		return response;
	}

	@Override
	public boolean isCouponAvailableForUse(final String couponCode, final UserModel user)
	{
		final AbstractCouponModel multicodeCoupon = getCouponDao().findCouponById(couponCode);
		if (Objects.nonNull(multicodeCoupon))
		{
			if (multicodeCoupon instanceof MultiCodeCouponModel)
			{
				return true;
			}
		}
		final SingleCodeCouponModel singleCodeCoupon = getCouponDao().findSingleCodeCouponById(couponCode);
		if (Objects.nonNull(singleCodeCoupon))
		{
			final CouponRedemptionStrategy<AbstractCouponModel> strategy = getRedemptionStrategyMap()
					.get(singleCodeCoupon.getItemtype());
			if (Objects.isNull(strategy))
			{
				throw new IllegalArgumentException("coupon " + singleCodeCoupon.getCouponId() + " of type:"
						+ singleCodeCoupon.getItemtype() + " has no redemption strategy defined.");
			}
			return strategy.isCouponRedeemable(singleCodeCoupon, user, couponCode);
		}
		return false;
	}

	public CouponDao getCouponDao()
	{
		return couponDao;
	}

	public void setCouponDao(final CouponDao couponDao)
	{
		this.couponDao = couponDao;
	}
}
