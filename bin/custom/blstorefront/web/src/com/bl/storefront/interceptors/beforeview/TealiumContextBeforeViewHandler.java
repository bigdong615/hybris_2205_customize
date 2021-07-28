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
	private final String FLAG_VALUE = "true";

	@Override
	public void beforeView(
      HttpServletRequest request, HttpServletResponse response, ModelAndView modelAndView) throws Exception
	{
		TealiumContext prevContext = (TealiumContext) JaloSession.getCurrentSession().getAttribute("tealiumContext");
		TealiumContext context;
		context = prevContext == null ? new TealiumContext() : prevContext;
		try
		{
			context.set("pagetype", modelAndView.getModel().get("pageType"));
			CustomerData customerData =(CustomerData) 	modelAndView.getModel().get("user");
			context.set("AccountID",customerData.getCustomerId());
			RentalDateDto rentalDate = (RentalDateDto) modelAndView.getModel().get("rentalDate");

			List<String> productId = new ArrayList<>();
			List<Float>  unitPrice = new ArrayList<>();
			String productName ="";
			String productSKU ="";
			if("PRODUCT".equals(modelAndView.getModel().get("pageType"))){
				Boolean isRental = (Boolean)modelAndView.getModel().get("IsRentalPage");
				if(isRental){
					context.set("isBuy", 0);
					if(rentalDate!= null) {
						context.set("rentalDays", rentalDate.getNumberOfDays());
					}
				}else{
					context.set("isBuy", 1);
				}
				ProductData productData = (ProductData) modelAndView.getModel().get("product");

				productName = new  String(productData.getDisplayName());
        productSKU = new String(productData.getCode());
				productId.add(productData.getProductId());
			}

			List <Long> quantity = new ArrayList<>();
			if("CART".equals(modelAndView.getModel().get("pageType"))){
				CartData cartData = (CartData) modelAndView.getModel().get("cartData");
				context.set("cartSize",cartData.getTotalUnitCount());

				Boolean isRental = (Boolean)cartData.getIsRentalCart();
				if(isRental){
					context.set("isBuy", 0);
					if(rentalDate!= null) {
						context.set("rentalDays", rentalDate.getNumberOfDays());
					}
				}else{
					context.set("isBuy", 1);
				}

				if(cartData.getDeliveryCost() != null) {
					if (cartData.getDeliveryCost().getValue().floatValue() >= 0) {
						context.set("shipping_cost", cartData.getDeliveryCost().getValue());
					}
				}
				List<String> coupanCode= new ArrayList<>();
				coupanCode.add("test");
				cartData.setAppliedVouchers(coupanCode);
				List<String> voucherCode = new ArrayList<>();
				if(CollectionUtils.isNotEmpty(cartData.getAppliedVouchers())){
					cartData.getAppliedVouchers().forEach( voucher -> {
						voucherCode.add(voucher);
					});
					context.set("couponCode",voucherCode.toString());
				}
        PriceData damageWaiverCost = cartData.getTotalDamageWaiverCost();
				if(damageWaiverCost!= null){
				context.set("damage_waiver_cost",damageWaiverCost.getValue());
        context.set("subtotal",cartData.getSubTotal().getValue().doubleValue()+damageWaiverCost.getValue().doubleValue());
        }else{
					context.set("subtotal",cartData.getSubTotal().getValue().doubleValue());
				}
        List<OrderEntryData> entryDataList = cartData.getEntries();
				StringBuilder pName = new StringBuilder();
				StringBuilder pSKU = new StringBuilder();
        if (CollectionUtils.isNotEmpty(entryDataList)){
            entryDataList.forEach( orderEntryData -> {
            quantity.add(orderEntryData.getQuantity());
            ProductData productData = orderEntryData.getProduct();
            pName.append("|").append(productData.getName());
            pSKU.append(",").append(productData.getCode());
            productId.add(productData.getProductId());
						unitPrice.add(orderEntryData.getTotalPrice().getValue().floatValue());
          });
					productName = new String(pName.toString());
					productSKU  = new String(pSKU.toString());
					context.set("quantity", quantity.toString());
					context.set("unit_price",unitPrice.toString());
					context.set("total_value",cartData.getTotalPrice().getValue());
        }
			}

			if("shippingPage".equals((modelAndView.getModel().get("pageType"))) || "paymentPage".equals(modelAndView.getModel().get("pageType"))) {
				AbstractOrderData orderData = (AbstractOrderData) modelAndView.getModel().get("cartData");
				orderData.getEntries();
				orderData.getDeliveryCost();
				if(orderData.getDeliveryCost() != null) {
					if (orderData.getDeliveryCost().getValue().floatValue() >= 0) {
						context.set("shipping_cost", orderData.getDeliveryCost().getValue());
					}
				}

				context.set("cartSize",orderData.getTotalUnitCount());

				Boolean isRental = (Boolean)orderData.getIsRentalCart();
				if(isRental){
					context.set("isBuy", 0);
					if(rentalDate!= null) {
						context.set("rentalDays", rentalDate.getNumberOfDays());
					}
				}else{
					context.set("isBuy", 1);
				}

				List<String> voucherCode = new ArrayList<>();
				if(CollectionUtils.isNotEmpty(orderData.getAppliedVouchers())){
					orderData.getAppliedVouchers().forEach( voucher -> {
						voucherCode.add(voucher);
					});
					context.set("couponCode",voucherCode.toString());
				}

				context.set("damage_waiver_cost",orderData.getTotalDamageWaiverCost().getValue());
				context.set("subtotal",orderData.getSubTotal().getValue().doubleValue()+orderData.getTotalDamageWaiverCost().getValue().doubleValue());
				List<OrderEntryData> entryDataList = orderData.getEntries();
				StringBuilder pName = new StringBuilder();
				StringBuilder pSKU = new StringBuilder();
				if (CollectionUtils.isNotEmpty(entryDataList)){
					entryDataList.forEach( orderEntryData -> {
						quantity.add(orderEntryData.getQuantity());
						ProductData productData = orderEntryData.getProduct();
						pName.append("|").append(productData.getName());
						pSKU.append(",").append(productData.getCode());
						productId.add(productData.getProductId());
						unitPrice.add(orderEntryData.getTotalPrice().getValue().floatValue());
					});
					productName = new String(pName.toString());
					productSKU  = new String(pSKU.toString());

					context.set("quantity", quantity.toString());
					context.set("unit_price",unitPrice.toString());
					context.set("total_value",orderData.getTotalPrice().getValue());
				}

			}

			if("ORDERCONFIRMATION".equals(modelAndView.getModel().get("pageType")) ) {
				AbstractOrderData orderData = (AbstractOrderData) modelAndView.getModel().get("orderData");
				context.set("orderID",orderData.getCode());


				orderData.getDeliveryCost();
				if(orderData.getDeliveryCost() != null) {
					if (orderData.getDeliveryCost().getValue().floatValue() >= 0) {
						context.set("shipping_cost", orderData.getDeliveryCost().getValue());
					}
				}

				context.set("cartSize",orderData.getTotalUnitCount());

				Boolean isRental = (Boolean)orderData.getIsRentalCart();
				if(isRental){
					context.set("isBuy", 0);
					if(orderData.getRentalDates()!= null) {
						context.set("rentalDays",orderData.getRentalDates().getNumberOfDays());
					}
				}else{
					context.set("isBuy", 1);
				}
				List<String> coupanCode= new ArrayList<>();
				coupanCode.add("test");
				orderData.setAppliedVouchers(coupanCode);
				List<String> voucherCode = new ArrayList<>();
				if(CollectionUtils.isNotEmpty(orderData.getAppliedVouchers())){
					orderData.getAppliedVouchers().forEach( voucher -> {
						voucherCode.add(voucher);
					});
					context.set("couponCode",voucherCode.toString());
				}

				context.set("damage_waiver_cost",orderData.getTotalDamageWaiverCost().getValue());
				context.set("subtotal",orderData.getSubTotal().getValue().doubleValue()+orderData.getTotalDamageWaiverCost().getValue().doubleValue());
				List<OrderEntryData> entryDataList = orderData.getEntries();
				StringBuilder pName = new StringBuilder();
				StringBuilder pSKU = new StringBuilder();
				if (CollectionUtils.isNotEmpty(entryDataList)){
					entryDataList.forEach( orderEntryData -> {
						quantity.add(orderEntryData.getQuantity());
						ProductData productData = orderEntryData.getProduct();
						pName.append("|").append(productData.getName());
						pSKU.append(",").append(productData.getCode());
						productId.add(productData.getProductId());
						unitPrice.add(orderEntryData.getTotalPrice().getValue().floatValue());
					});
					productName = new String(pName.toString());
					productSKU  = new String(pSKU.toString());

					context.set("quantity", quantity.toString());
					context.set("unit_price",unitPrice.toString());
					context.set("total_value",orderData.getTotalPrice().getValue());

					context.set("userEmail",customerData.getUid());
					context.set("userFirstName",customerData.getFirstName());
					context.set("userLastName",customerData.getLastName());
					context.set("isVideo",orderData.getCode());

				}

			}

			if(productSKU != null && productSKU.contains(",")){
			productSKU = productSKU.substring(1);
		   }
			if(productName.toString().contains("|")){
			productName = productName.substring(1);
	   	}
			context.set("ProductName",productName);
			context.set("productSKU",productSKU);
			if (productId.size() == 1) {
				context.set("prodid", productId.get(0));
			}
    else{
				context.set("prodid", productId.toString());
			}
		}
		catch (Throwable e)
		{
			LOG.error("TealiumContextBeforeViewHandler failed with message:" + e.getMessage());
		}
		finally
		{
			JaloSession session = JaloSession.getCurrentSession();
			session.setAttribute("tealiumContext", context);
		}

	}

}
