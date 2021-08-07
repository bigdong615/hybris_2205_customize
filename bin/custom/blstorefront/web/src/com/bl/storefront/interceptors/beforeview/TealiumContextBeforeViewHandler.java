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
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.ModelAndView;

/**
 * @author  Vijay Vishwakarma
 * This handler is created for provided required data for Tealium UDO.
 */
public class TealiumContextBeforeViewHandler implements BeforeViewHandler
{

	private static final Logger LOG = LoggerFactory.getLogger(TealiumContextBeforeViewHandler.class);
	public static final String PAGETYPE = "pagetype";
	public static final String CART_DATA = "cartData";
	public static final String PAYMENT_PAGE = "paymentPage";
	private static final int SCALE = 2;
	private static final String SUB_TOTAL = "subtotal";
	private static final String TOTAL_VALUE = "total_value";
	private static final String CART_SIZE = "cartSize";
	private static final String SHIPPING_COST = "shipping_cost";
	private static final String PAGE_TYPE = "pageType";
	private static final String QUANTITY = "quantity";
	private static final String UNIT_PRICE = "unit_price";
	private static final String DAMAGE_WAIVER_COST = "damage_waiver_cost";
	private static final String COUPON_CODE = "couponCode";
	private static final String RENTAL_DAYS = "rentalDays";
	private static final String IS_BUY = "isBuy";
	private static final String IS_VIDEO = "isVideo";
	private static final String TEALIUM_CONTEXT = "tealiumContext";
	private static final String ACCOUNT_ID = "AccountID";
	private static final String TEALIUM_ACCOUNT = "tealium_account";
	private static final String UTAG_MAIN_MYCOOKIE = "utag_main_mycookie";
	private static final String USER = "user";
	private static final String RENTAL_DATE = "rentalDate";
	private static final String PRODUCT = "PRODUCT";
	private static final String PRODUCT_NAME = "ProductName";
	private static final String PRODUCT_SKU = "productSKU";
	private static final String PRODID = "prodid";
	private static final String ORDER_TAX = "order_tax";
	private static final String USER_EMAIL = "userEmail";
	private static final String USER_FIRST_NAME = "userFirstName";
	private static final String USER_LAST_NAME = "userLastName";
	private static final String IS_RENTAL_PAGE = "IsRentalPage";
	private static final String CART = "CART";
	private static final String SHIPPING_PAGE = "shippingPage";
	private static final String ORDERCONFIRMATION = "ORDERCONFIRMATION";
	private static final String ORDER_DATA = "orderData";
	private static final String ORDER_ID = "orderID";
	public static final String FALSE_VALUE = "0";
	public static final String TRUE_VALUE = "1";
	public static final String OR_STR = "|";
	public static final String CAMAS_STR = ",";
	public static final String PROD_OUT_OF_STOCK = "prodOutOfStock";


	@Override
	public void beforeView(
			HttpServletRequest request, HttpServletResponse response, ModelAndView modelAndView)
			throws Exception {
		TealiumContext context = (TealiumContext) JaloSession.getCurrentSession().getAttribute(
				TEALIUM_CONTEXT);
		if (context == null) {
			context = new TealiumContext();
		}
		try {
			context.set(PAGETYPE, modelAndView.getModel().get(PAGE_TYPE));
			CustomerData customerData = (CustomerData) modelAndView.getModel().get(USER);
			context.set(ACCOUNT_ID, customerData.getCustomerId());
			context.set(TEALIUM_ACCOUNT, customerData.getUid());
			context.set(UTAG_MAIN_MYCOOKIE, customerData.getUid());
			RentalDateDto rentalDate = (RentalDateDto) modelAndView.getModel().get(RENTAL_DATE);
			List<String> productId = new ArrayList<>();
			List<String> unitPrice = new ArrayList<>();
			List<String> videoList = new ArrayList<>();
			List<String> quantity = new ArrayList<>();
			if (PRODUCT.equals(modelAndView.getModel().get(PAGE_TYPE))) {
				setProductTag(context, modelAndView, rentalDate, productId);
			}

			if (CART.equals(modelAndView.getModel().get(PAGE_TYPE))) {
				setCartTag(context, modelAndView, rentalDate, productId, quantity,
						unitPrice, videoList);
			}

			if (SHIPPING_PAGE.equals((modelAndView.getModel().get(PAGE_TYPE))) || PAYMENT_PAGE
					.equals(modelAndView.getModel().get(PAGE_TYPE))) {
				setShippingTag(context, modelAndView, rentalDate, productId,
						quantity, unitPrice, videoList);
			}

			if (ORDERCONFIRMATION.equals(modelAndView.getModel().get(PAGE_TYPE))) {
				setOrderConfirmationTag(context, modelAndView, rentalDate,
						productId, quantity, unitPrice, videoList, customerData);
			}

		} catch (Exception e) {
			LOG.error("TealiumContextBeforeViewHandler failed with message:" + e.getMessage()); // NOSONAR
		} finally {
			JaloSession session = JaloSession.getCurrentSession();
			session.setAttribute(TEALIUM_CONTEXT, context);
		}

	}

	/**
	 * This method used for setting PDP related context data.
	 * @param context
	 * @param modelAndView
	 * @param rentalDate
	 * @param productId
	 */
	private void setProductTag(TealiumContext context, ModelAndView modelAndView,
			RentalDateDto rentalDate,List<String> productId) {
		setRentalTag(context, (Boolean) modelAndView.getModel().get(IS_RENTAL_PAGE), rentalDate);
		ProductData productData = (ProductData) modelAndView.getModel().get("product");
		productId.add(productData.getProductId());
		context.set(PRODUCT_NAME, productData.getDisplayName());
		context.set(PRODUCT_SKU, productData.getCode());
		context.setArrayValue(PRODID, productId.toArray(new String[productId.size()]));
		context.set(PROD_OUT_OF_STOCK, StringUtils.isNotEmpty(productData.getStock().getStockLevelStatus().getCode()) ? "0": "1");
	}

	/**
	 * This method used for setting Cart page related context data.
	 * @param context
	 * @param modelAndView
	 * @param rentalDate
	 * @param productId
	 * @param quantity
	 * @param unitPrice
	 * @param videoList
	 */
	private void setCartTag(TealiumContext context, ModelAndView modelAndView,
			RentalDateDto rentalDate,List<String> productId, List<String> quantity,
			List<String> unitPrice, List<String> videoList) {
		CartData cartData = (CartData) modelAndView.getModel().get(CART_DATA);
		setRentalTag(context, cartData.getIsRentalCart(), rentalDate);
		setVoucherCode(context, cartData.getAppliedVouchers());
		setSubtotalTag(context, cartData.getTotalDamageWaiverCost(), cartData.getSubTotal());
		setProductEntryData(context,cartData.getEntries(), quantity, productId, unitPrice,videoList);
		setCommonTag(context, quantity, unitPrice, cartData.getTotalUnitCount(),
				cartData.getDeliveryCost(), cartData.getTotalPrice());
	}

	/**
	 * This method used for setting shiping and payment page related context data.
	 * @param context
	 * @param modelAndView
	 * @param rentalDate
	 * @param productId
	 * @param quantity
	 * @param unitPrice
	 * @param videoList
	 */
	private void setShippingTag(TealiumContext context, ModelAndView modelAndView,
			RentalDateDto rentalDate, List<String> productId,
			List<String> quantity, List<String> unitPrice, List<String> videoList) {
		AbstractOrderData orderData = (AbstractOrderData) modelAndView.getModel().get(CART_DATA);
		setRentalTag(context, orderData.getIsRentalCart(), rentalDate);
		setVoucherCode(context, orderData.getAppliedVouchers());
		setSubtotalTag(context, orderData.getTotalDamageWaiverCost(), orderData.getSubTotal());
		setProductEntryData(context,orderData.getEntries(), quantity, productId, unitPrice,videoList);
		setCommonTag(context, quantity, unitPrice, orderData.getTotalUnitCount(),
				orderData.getDeliveryCost(), orderData.getTotalPrice());

	}

	/**
	 * This method used for setting order confirmation page related context data.
	 * @param context
	 * @param modelAndView
	 * @param rentalDate
	 * @param productId
	 * @param quantity
	 * @param unitPrice
	 * @param videoList
	 * @param customerData
	 */
	private void setOrderConfirmationTag(TealiumContext context, ModelAndView modelAndView, // NOSONAR
			RentalDateDto rentalDate, List<String> productId,
			List<String> quantity, List<String> unitPrice, List<String> videoList,
			CustomerData customerData) { // NOSONAR
		AbstractOrderData orderData = (AbstractOrderData) modelAndView.getModel().get(ORDER_DATA);
		context.set(ORDER_ID, orderData.getCode());
		setRentalTag(context, orderData.getIsRentalCart(), orderData.getRentalDates());
		setVoucherCode(context, orderData.getAppliedVouchers());
		setSubtotalTag(context, orderData.getTotalDamageWaiverCost(), orderData.getSubTotal());
		setProductEntryData(context,orderData.getEntries(), quantity, productId, unitPrice, videoList);
		setCommonTag(context, quantity, unitPrice, orderData.getTotalUnitCount(),
				orderData.getDeliveryCost(), orderData.getTotalPrice());
		context.setArrayValue(IS_VIDEO, videoList.toArray(new String[videoList.size()]));
		context.set(ORDER_TAX, orderData.getTotalTax().getValue().toPlainString());
		context.set(USER_EMAIL, customerData.getUid());
		context.set(USER_FIRST_NAME, customerData.getFirstName());
		context.set(USER_LAST_NAME, customerData.getLastName());
	}

	/**
	 *  This method used for populating entry data.
	 * @param context
	 * @param entryDataList
	 * @param quantity
	 * @param productId
	 * @param unitPrice
	 * @param videoList
	 */
	public void setProductEntryData(TealiumContext context,List<OrderEntryData> entryDataList, List<String> quantity,
			List<String> productId, List<String> unitPrice,
			List<String> videoList) {
		StringBuilder pName = new StringBuilder();
		StringBuilder pSKU = new StringBuilder();
		if (CollectionUtils.isNotEmpty(entryDataList)) {
			entryDataList.forEach(orderEntryData -> {
				quantity.add(orderEntryData.getQuantity().toString());
				ProductData productData = orderEntryData.getProduct();
				pName.append(OR_STR).append(productData.getName());
				pSKU.append(CAMAS_STR).append(productData.getCode());
				productId.add(productData.getProductId());
				unitPrice.add(orderEntryData.getBasePrice().getValue().toPlainString());
				videoList.add(productData.isIsVideo() ? TRUE_VALUE : FALSE_VALUE);
			});
			String	productName = pName.toString();
			String	productSKU = pSKU.toString();
			if (productSKU != null && productSKU.contains(CAMAS_STR)) {
				productSKU = productSKU.substring(1);
			}
			if (productName.contains(OR_STR)) {
				productName = productName.substring(1);
			}
			context.set(PRODUCT_NAME, productName);
			context.set(PRODUCT_SKU, productSKU);
			context.setArrayValue(PRODID, productId.toArray(new String[productId.size()]));
		}
	}

	/**
	 *  This method used for setting rental related information.
	 * @param context
	 * @param isRental
	 * @param rentalDate
	 */
	private void setRentalTag(TealiumContext context, boolean isRental, RentalDateDto rentalDate) {
		if (isRental && rentalDate != null) {
			context.set(IS_BUY, FALSE_VALUE);
			context.set(RENTAL_DAYS, rentalDate.getNumberOfDays());
		} else {
			context.set(IS_BUY, TRUE_VALUE);
		}
	}

	/**
	 * This method used for setting coupon code.
	 * @param context
	 * @param appliedVoucher
	 */
	private void setVoucherCode(TealiumContext context, List<String> appliedVoucher) {
		List<String> voucherCode = new ArrayList<>();
		if (CollectionUtils.isNotEmpty(appliedVoucher)) {
			appliedVoucher.forEach(voucher -> {
				voucherCode.add(voucher);
			});
			context.set(COUPON_CODE, voucherCode.toString());
		}
	}

	/**
	 *  This method used for setting total damage waiver and sub total cost.
	 * @param context
	 * @param damageWaiverCost
	 * @param subTotal
	 */
	private void setSubtotalTag(TealiumContext context, PriceData damageWaiverCost,
			PriceData subTotal) {
		if (damageWaiverCost != null && damageWaiverCost.getValue() != null) {
			context.set(DAMAGE_WAIVER_COST,
					damageWaiverCost.getValue().setScale(SCALE, RoundingMode.HALF_DOWN).toPlainString());
			double subtotalWithDamageWaiver =
					subTotal.getValue().doubleValue() + damageWaiverCost.getValue().doubleValue();
			BigDecimal subtotal =  BigDecimal.valueOf(subtotalWithDamageWaiver);
			subtotal = subtotal.setScale(SCALE, RoundingMode.HALF_DOWN);
			context.set(SUB_TOTAL, subtotal.toPlainString());
		} else {
			context.set(SUB_TOTAL, subTotal.getValue().toPlainString());
		}
	}

	/**
	 * This method used for setting some common  field in context.
	 * @param context
	 * @param quantity
	 * @param unitPrice
	 * @param cartSize
	 * @param deliveryCost
	 * @param totalPrice
	 */
	private void setCommonTag(TealiumContext context, List<String> quantity, List<String> unitPrice,
			Integer cartSize, PriceData deliveryCost, PriceData totalPrice) {
		context.setArrayValue(QUANTITY, quantity.toArray(new String[quantity.size()]));
		context.setArrayValue(UNIT_PRICE, unitPrice.toArray(new String[unitPrice.size()]));
		context.set(CART_SIZE, cartSize);
		if (deliveryCost != null && deliveryCost.getValue().floatValue() >= 0) {
			context.set(SHIPPING_COST, deliveryCost.getValue().toPlainString());
		}
		context.set(TOTAL_VALUE, totalPrice.getValue());
	}

}
