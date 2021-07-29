package com.bl.storefront.interceptors.beforeview;

import com.bl.facades.product.data.RentalDateDto;
import com.tealium.context.TealiumContext;
import de.hybris.platform.acceleratorstorefrontcommons.interceptors.BeforeViewHandler;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.jalo.JaloSession;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.ModelAndView;


public class TealiumContextBeforeViewHandler implements BeforeViewHandler
{

	private static final Logger LOG = LoggerFactory.getLogger(TealiumContextBeforeViewHandler.class);
	private static final  int SCALE = 2;
	private static final String SUB_TOTAL = "subtotal";
	private static final String TOTAL_VALUE = "total_value";
	private static final String CART_SIZE = "cartSize";
	private static final String SHIPPING_COST = "shipping_cost";
	private static final String PAGE_TYPE ="pageType";
	private static final String QUANTITY = "quantity";
	private static final String UNIT_PRICE = "unit_price";
	private static final String DAMAGE_WAIVER_COST = "damage_waiver_cost";
	private static final String COUPON_CODE = "couponCode";
	private static final String RENTAL_DAYS = "rentalDays";
	private static final String IS_BUY = "isBuy";
	private static final String IS_VIDEO = "isVideo";

	@Override
	public void beforeView(
      HttpServletRequest request, HttpServletResponse response, ModelAndView modelAndView) throws Exception
	{
		TealiumContext context = (TealiumContext) JaloSession.getCurrentSession().getAttribute("tealiumContext");
		if (context == null){
			context = new TealiumContext() ;
		}

		try
		{
			context.set("pagetype", modelAndView.getModel().get(PAGE_TYPE));
			CustomerData customerData =(CustomerData) 	modelAndView.getModel().get("user");
			context.set("AccountID",customerData.getCustomerId());
			RentalDateDto rentalDate = (RentalDateDto) modelAndView.getModel().get("rentalDate");

			List<String> productId = new ArrayList<>();
			List<String>  unitPrice = new ArrayList<>();
			String productName ="";
			String productSKU ="";
			if("PRODUCT".equals(modelAndView.getModel().get(PAGE_TYPE))){
				boolean isRental = (Boolean)modelAndView.getModel().get("IsRentalPage");
				if(isRental && rentalDate!= null){
					context.set(IS_BUY, 0);
					context.set(RENTAL_DAYS, rentalDate.getNumberOfDays());
				}else{
					context.set(IS_BUY, 1);
				}
				ProductData productData = (ProductData) modelAndView.getModel().get("product");

				productName = productData.getDisplayName();
        productSKU = productData.getCode();
				productId.add(productData.getProductId());
			}

			List <String> quantity = new ArrayList<>();
			if("CART".equals(modelAndView.getModel().get(PAGE_TYPE))){
				CartData cartData = (CartData) modelAndView.getModel().get("cartData");
				context.set(CART_SIZE,cartData.getTotalUnitCount());

				boolean isRental = cartData.getIsRentalCart();
				if(isRental && rentalDate!= null){
					context.set(IS_BUY, 0);
					context.set(RENTAL_DAYS, rentalDate.getNumberOfDays());
				}else{
					context.set(IS_BUY, 1);
				}

				if(cartData.getDeliveryCost() != null && cartData.getDeliveryCost().getValue().floatValue() >= 0) {
						context.set(SHIPPING_COST, cartData.getDeliveryCost().getValue().toPlainString());
				}else {
					context.set(SHIPPING_COST, "");
				}

				List<String> voucherCode = new ArrayList<>();
				if(CollectionUtils.isNotEmpty(cartData.getAppliedVouchers())){
					cartData.getAppliedVouchers().forEach( voucher -> {
						voucherCode.add(voucher);
					});
					context.setArrayValue(COUPON_CODE,voucherCode.toArray(new String[voucherCode.size()]));
				}
        PriceData damageWaiverCost = cartData.getTotalDamageWaiverCost();
				if(damageWaiverCost!= null && damageWaiverCost.getValue() != null){
				context.set(DAMAGE_WAIVER_COST,damageWaiverCost.getValue().setScale(SCALE, RoundingMode.HALF_DOWN).toPlainString());
					double subtotal =cartData.getSubTotal().getValue().doubleValue()+damageWaiverCost.getValue().doubleValue();
					BigDecimal subTotal = new BigDecimal(subtotal); // NOSONAR
					subTotal=subTotal.setScale(SCALE, RoundingMode.HALF_DOWN);
					context.set(SUB_TOTAL, subTotal.toPlainString());
        }else{
					context.set(SUB_TOTAL,cartData.getSubTotal().getValue().toPlainString());
				}
        List<OrderEntryData> entryDataList = cartData.getEntries();
				StringBuilder pName = new StringBuilder();
				StringBuilder pSKU = new StringBuilder();
        if (CollectionUtils.isNotEmpty(entryDataList)){
            entryDataList.forEach( orderEntryData -> {
            quantity.add(orderEntryData.getQuantity().toString());
            ProductData productData = orderEntryData.getProduct();
            pName.append("|").append(productData.getName());
            pSKU.append(",").append(productData.getCode());
            productId.add(productData.getProductId());
						unitPrice.add(orderEntryData.getBasePrice().getValue().toPlainString());
          });
					productName = pName.toString();
					productSKU  = pSKU.toString();
					context.setArrayValue(QUANTITY, quantity.toArray(new String[quantity.size()]));
					context.setArrayValue(UNIT_PRICE,unitPrice.toArray(new String[unitPrice.size()]));
					context.set(TOTAL_VALUE,cartData.getTotalPrice().getValue());
        }
			}

			if("shippingPage".equals((modelAndView.getModel().get(PAGE_TYPE))) || "paymentPage".equals(modelAndView.getModel().get(PAGE_TYPE))) {
				AbstractOrderData orderData = (AbstractOrderData) modelAndView.getModel().get("cartData");
				orderData.getEntries();
				orderData.getDeliveryCost();
				if(orderData.getDeliveryCost() != null && orderData.getDeliveryCost().getValue().floatValue() >= 0) {
						context.set(SHIPPING_COST, orderData.getDeliveryCost().getValue().toPlainString());
				}
				context.set(CART_SIZE,orderData.getTotalUnitCount());

				boolean isRental = orderData.getIsRentalCart();
				if(isRental && rentalDate!= null){
					 context.set(IS_BUY, 0);
					 context.set(RENTAL_DAYS, rentalDate.getNumberOfDays());
				}else{
					context.set(IS_BUY, 1);
				}

				List<String> voucherCode = new ArrayList<>();
				if(CollectionUtils.isNotEmpty(orderData.getAppliedVouchers())){
					orderData.getAppliedVouchers().forEach( voucher -> {
						voucherCode.add(voucher);
					});
					context.set(COUPON_CODE,voucherCode.toString());
				}
			 PriceData damageWaiverCost =	orderData.getTotalDamageWaiverCost();
				if(damageWaiverCost!= null && damageWaiverCost.getValue() != null) {
					context.set(DAMAGE_WAIVER_COST,
							damageWaiverCost.getValue().setScale(SCALE, RoundingMode.HALF_DOWN));
					double subtotal =orderData.getSubTotal().getValue().doubleValue()+damageWaiverCost.getValue().doubleValue();
					BigDecimal subTotal = new BigDecimal(subtotal); //// NOSONAR
					subTotal=subTotal.setScale(SCALE, RoundingMode.HALF_DOWN);
					context.set(SUB_TOTAL, subTotal.toPlainString());
				}else{
					context.set(SUB_TOTAL, orderData.getSubTotal().getValue().toPlainString());
				}
				List<OrderEntryData> entryDataList = orderData.getEntries();
				StringBuilder pName = new StringBuilder();
				StringBuilder pSKU = new StringBuilder();
				if (CollectionUtils.isNotEmpty(entryDataList)){
					entryDataList.forEach( orderEntryData -> {
						quantity.add(orderEntryData.getQuantity().toString());
						ProductData productData = orderEntryData.getProduct();
						pName.append("|").append(productData.getName());
						pSKU.append(",").append(productData.getCode());
						productId.add(productData.getProductId());
						unitPrice.add(orderEntryData.getBasePrice().getValue().toPlainString());
					});
					productName = pName.toString();
					productSKU  = pSKU.toString();

					context.setArrayValue(QUANTITY, quantity.toArray(new String[quantity.size()]));
					context.setArrayValue(UNIT_PRICE,unitPrice.toArray(new String[unitPrice.size()]));
					context.set(TOTAL_VALUE,orderData.getTotalPrice().getValue());
				}

			}

			if("ORDERCONFIRMATION".equals(modelAndView.getModel().get(PAGE_TYPE)) ) {
				AbstractOrderData orderData = (AbstractOrderData) modelAndView.getModel().get("orderData");
				context.set("orderID",orderData.getCode());
				if(orderData.getDeliveryCost() != null && orderData.getDeliveryCost().getValue().floatValue() >= 0) {
						context.set(SHIPPING_COST, orderData.getDeliveryCost().getValue().toPlainString());
				}

				context.set(CART_SIZE,orderData.getTotalUnitCount());

				boolean isRental = orderData.getIsRentalCart();
				if(isRental && orderData.getRentalDates()!= null){
					context.set(IS_BUY, 0);
					context.set(RENTAL_DAYS,orderData.getRentalDates().getNumberOfDays());
				}else{
					context.set(IS_BUY, 1);
				}
				List<String> voucherCode = new ArrayList<>();
				if(CollectionUtils.isNotEmpty(orderData.getAppliedVouchers())){
					orderData.getAppliedVouchers().forEach( voucher -> {
						voucherCode.add(voucher);
					});
					context.set(COUPON_CODE,voucherCode.toString());
				}
				PriceData damageWaiverCost =	orderData.getTotalDamageWaiverCost();
				if(damageWaiverCost!= null && damageWaiverCost.getValue() != null) {
					context.set(DAMAGE_WAIVER_COST,
							damageWaiverCost.getValue().setScale(SCALE, RoundingMode.HALF_DOWN).toPlainString());
					double subtotal =orderData.getSubTotal().getValue().doubleValue()+damageWaiverCost.getValue().doubleValue();
					BigDecimal subTotal = new BigDecimal(subtotal); // NOSONAR
					subTotal=subTotal.setScale(SCALE, RoundingMode.HALF_DOWN);
					context.set(SUB_TOTAL, subTotal.toPlainString());
				}else{
					context.set(SUB_TOTAL, orderData.getSubTotal().getValue().toPlainString());
				}
				List<OrderEntryData> entryDataList = orderData.getEntries();
				StringBuilder pName = new StringBuilder();
				StringBuilder pSKU = new StringBuilder();
				List<String> videoList = new ArrayList<>();
				if (CollectionUtils.isNotEmpty(entryDataList)){
					entryDataList.forEach( orderEntryData -> {
						quantity.add(orderEntryData.getQuantity().toString());
						ProductData productData = orderEntryData.getProduct();
						pName.append("|").append(productData.getName());
						pSKU.append(",").append(productData.getCode());
						productId.add(productData.getProductId());
						unitPrice.add(orderEntryData.getBasePrice().getValue().toPlainString());
						videoList.add(productData.isIsVideo() ? "1" :"0");
					});
					productName = pName.toString();
					productSKU  = pSKU.toString();

					context.setArrayValue(QUANTITY, quantity.toArray(new String[quantity.size()]));
					context.setArrayValue(UNIT_PRICE,unitPrice.toArray(new String[unitPrice.size()]));
					context.set(TOTAL_VALUE,orderData.getTotalPrice().getValue().toPlainString());
					context.set("order_tax",orderData.getTotalTax().getValue().toPlainString());
					context.set("userEmail",customerData.getUid());
					context.set("userFirstName",customerData.getFirstName());
					context.set("userLastName",customerData.getLastName());
					context.setArrayValue(IS_VIDEO,videoList.toArray(new String[videoList.size()]));
				}

			}

			if(productSKU != null && productSKU.contains(",")){
			productSKU = productSKU.substring(1);
		   }
			if(productName.contains("|")){
			productName = productName.substring(1);
	   	}
			context.set("ProductName",productName);
			context.set("productSKU",productSKU);
			context.setArrayValue("prodid",productId.toArray(new String[productId.size()]));
		}
		catch (Exception e)
		{
			LOG.error("TealiumContextBeforeViewHandler failed with message:" + e.getMessage()); // NOSONAR
		}
		finally
		{
			JaloSession session = JaloSession.getCurrentSession();
			session.setAttribute("tealiumContext", context);
		}

	}

}
