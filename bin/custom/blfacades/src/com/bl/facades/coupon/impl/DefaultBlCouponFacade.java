package com.bl.facades.coupon.impl;

import com.bl.core.coupon.impl.DefaultBlCouponService;
import com.bl.core.utils.BlExtendOrderUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.coupon.BlCouponFacade;
import com.bl.facades.populators.BlExtendRentalOrderDetailsPopulator;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.voucher.exceptions.VoucherOperationException;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import org.apache.commons.lang3.BooleanUtils;

/**
 * This class created to add custom logic for extend order promotion
 * @author Manikandan
 */
public class DefaultBlCouponFacade implements BlCouponFacade {

  private DefaultBlCouponService defaultBlCouponService;
  private CustomerAccountService customerAccountService;
  private BaseStoreService baseStoreService;
  private UserService userService;
  private BlExtendRentalOrderDetailsPopulator blExtendRentalOrderDetailsPopulator;


  private Converter<OrderModel, OrderData> blDefaultExtendOrderConverter;

  /**
   * This method created to add custom logic extend order promotion
   */
  @Override
  public OrderData applyVoucherForExtendOrder(final String voucherCode , final String referer , final List<String> errorList)
      throws VoucherOperationException {

    ServicesUtil.validateParameterNotNullStandardMessage("coupon code", voucherCode);
    final DefaultBlCouponService var10002 = getDefaultBlCouponService();
    var10002.getClass();
    final CouponResponse couponResponse = applyIfCartExists(voucherCode , var10002::redeemCouponForExtendOrder);
    
    if (BooleanUtils.isNotTrue(couponResponse.getSuccess())) {
      errorList.add(couponResponse.getMessage());
    }

    return getOrderDataForExtendedOrder(referer);
  }

  /**
   * This method created to add custom logic extend order promotion
   */
  @Override
  public <R> R applyIfCartExists(final String code, final BiFunction<String, OrderModel, R> orderConsumer) throws VoucherOperationException {

     OrderModel orderModel = null;
    if(null != BlExtendOrderUtils.getCurrentExtendOrderToSession()){
       orderModel = BlExtendOrderUtils.getCurrentExtendOrderToSession();
    }
    if (Objects.nonNull(orderModel)) {
      return orderConsumer.apply(code, orderModel);
    } else {
      throw new VoucherOperationException("No order was found in session");
    }
  }

  /**
   * This method created to remove the coupon from extend order secion
   */
  @Override
  public OrderData releaseVoucherForExtendOrder(final String voucherCode , final String referer) throws VoucherOperationException {

    ServicesUtil.validateParameterNotNullStandardMessage("coupon code", voucherCode);
    final DefaultBlCouponService var10002 = this.getDefaultBlCouponService();
    var10002.getClass();
    this.acceptIfCartExists(voucherCode, var10002::releaseCouponCodeForExtendOrder);
    OrderModel orderModel = null;
    if(null != BlExtendOrderUtils.getCurrentExtendOrderToSession() &&
        BlExtendOrderUtils.getCurrentExtendOrderToSession().getCode().equalsIgnoreCase(getOrderCodeFromRequest(referer))){
      orderModel = BlExtendOrderUtils.getCurrentExtendOrderToSession();
    }

    else {
      final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
      orderModel = getCustomerAccountService()
          .getOrderForCode((CustomerModel) getUserService().getCurrentUser(), getOrderCodeFromRequest(referer),
              baseStoreModel);
    }
    final OrderData orderData = new OrderData();
    getBlDefaultExtendOrderConverter().convert(orderModel , orderData);
    return orderData;

  }

  @Override
  public void acceptIfCartExists(final String code, final BiConsumer<String, AbstractOrderModel> orderConsumer) throws VoucherOperationException {
    OrderModel orderModel = null;
    if(null != BlExtendOrderUtils.getCurrentExtendOrderToSession()){
      orderModel = BlExtendOrderUtils.getCurrentExtendOrderToSession();
    }
    if (Objects.nonNull(orderModel)) {
      orderConsumer.accept(code, orderModel);
    } else {
      throw new VoucherOperationException("No cart was found in session");
    }
  }

  /**
   * This method created to get order code from referer
   */
  private String getOrderCodeFromRequest(final String referer) {
    final String[] split = referer.split(BlFacadesConstants.URL_SEPERATOR);
    final int size = split.length -1;
    return split[size];
  }
  
  @Override
  public OrderData getOrderDataForExtendedOrder(final String referer)
  {
	  OrderModel orderModel = null;
	  if (null != BlExtendOrderUtils.getCurrentExtendOrderToSession()
			  && BlExtendOrderUtils.getCurrentExtendOrderToSession().getCode().equalsIgnoreCase(getOrderCodeFromRequest(referer)))
	  {
		  orderModel = BlExtendOrderUtils.getCurrentExtendOrderToSession();
	  }

	  else
	  {
		  final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
		  orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(),
				  getOrderCodeFromRequest(referer), baseStoreModel);
	  }
	  final OrderData orderData = new OrderData();
	  getBlDefaultExtendOrderConverter().convert(orderModel, orderData);
	  return orderData;
  }

  public DefaultBlCouponService getDefaultBlCouponService() {
    return defaultBlCouponService;
  }

  public void setDefaultBlCouponService(
      DefaultBlCouponService defaultBlCouponService) {
    this.defaultBlCouponService = defaultBlCouponService;
  }

  public CustomerAccountService getCustomerAccountService() {
    return customerAccountService;
  }

  public void setCustomerAccountService(
      CustomerAccountService customerAccountService) {
    this.customerAccountService = customerAccountService;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

  public BlExtendRentalOrderDetailsPopulator getBlExtendRentalOrderDetailsPopulator() {
    return blExtendRentalOrderDetailsPopulator;
  }

  public void setBlExtendRentalOrderDetailsPopulator(
      BlExtendRentalOrderDetailsPopulator blExtendRentalOrderDetailsPopulator) {
    this.blExtendRentalOrderDetailsPopulator = blExtendRentalOrderDetailsPopulator;
  }


  public Converter<OrderModel, OrderData> getBlDefaultExtendOrderConverter() {
    return blDefaultExtendOrderConverter;
  }

  public void setBlDefaultExtendOrderConverter(
      Converter<OrderModel, OrderData> blDefaultExtendOrderConverter) {
    this.blDefaultExtendOrderConverter = blDefaultExtendOrderConverter;
  }

}
