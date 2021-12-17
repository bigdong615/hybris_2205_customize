<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="multi-checkout"
	tagdir="/WEB-INF/tags/responsive/checkout/multi"%>
<%@ taglib prefix="multi-checkout-paypal"
	tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/checkout/multi"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="custom-fields"
	tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/custom/fields"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement"
	tagdir="/WEB-INF/tags/responsive/formElement"%>
<%@ taglib prefix="checkout"
	tagdir="/WEB-INF/tags/responsive/checkout/multi"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="agreementInfo"
	tagdir="/WEB-INF/tags/responsive/agreementInfo"%>

<c:url value="/checkout/orderConfirmation/"
	var="orderConfirmationPageUrl" />
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="description" content="">
<title>BorrowLenses - Checkout - Step 4</title>

<!-- Required CSS -->
<link rel="stylesheet" type="text/css" media="all"
	href="${fn:escapeXml(themeResourcePath)}/css/bootstrap.min.css" />
<link rel="stylesheet" type="text/css" media="all"
	href="${fn:escapeXml(themeResourcePath)}/css/mmenu.css" />
<link rel="stylesheet" type="text/css" media="all"
	href="${fn:escapeXml(themeResourcePath)}/css/blstyle.css" />
<link rel="stylesheet" type="text/css" media="all"
	href="${fn:escapeXml(themeResourcePath)}/css/cart.css" />
<link rel="stylesheet" type="text/css" media="all"
	href="${fn:escapeXml(themeResourcePath)}/css/blcustom.css" />
</head>
<body class="cart print-quote">
	<section id="cartProcess" class="pt-5">
		<div class="container">
			<div class="row justify-content-center">
				<div class="col-xl-10">
					<div class="row">
						<div id="order" class="col-md-7">
							<h1>
								<spring:theme code="text.review.used.gear.page.title" />
							</h1>
							<hr>
							<form>
                <div class="reviewCart">
                	<div class="row">
                		<div class="col-4">
                			<p class="overline">
                				<spring:theme code="text.review.used.gear.page.arrives" />
                			</p>
                			<c:if
                				test="${fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == false}">
                				<p class="lightteal mb-0">
                					<b><spring:theme code="text.review.used.gear.page" /></b>
                				</p>
                			</c:if>
                			<p class="body14">
                				<c:choose>
                					<c:when
                						test="${fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'SHIP_UPS_OFFICE') == true or fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == true}">
                						<spring:theme code="text.review.page.date.start.delivery.pickup" />
                					</c:when>
                					<c:otherwise>
                						<spring:theme code="text.review.page.date.start.delivery"
                							arguments="${orderData.deliveryMode.carrier }" />
                					</c:otherwise>
                				</c:choose>
                			</p>
                		</div>
                	</div>
                </div>
								<div class="reviewCart pb-0">
									<h5 class="mb-4">
										<spring:theme code="text.checkout.multi.order.UsedGear" />
									</h5>
									<c:forEach items="${orderData.entries}" var="cartEntry">
										<div class="row mb-4">
											<div class="col-md-3 text-center">
												<product:productPrimaryImage product="${cartEntry.product}"
													format="thumbnail" />
											</div>
											<div class="col-md-9 mt-3">
												<p class="gray80 body14">
													<b class="gray100">${cartEntry.product.name}</b>
													<spring:theme code="text.review.page.your.rental.qty" />
													${cartEntry.quantity }<br>
													<spring:theme code="text.review.page.your.rental.total" />
													<format:price priceData="${cartEntry.totalPrice}"
														displayFreeForZero="true" />
												</p>
											</div>
										</div>
									</c:forEach>
								</div>
								<div class="reviewCart pb-0">
									<c:choose>
										<c:when
											test="${fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'SHIP_UPS_OFFICE') == true or fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == true}">
											<h5 class="mb-4">
												<spring:theme code="text.review.page.delivery.pickup.title" />
											</h5>
											<div class="row mb-4">
												<div class="col-6">
													<p class="gray80 body14">
														<b class="gray100"><spring:theme
																code="text.review.page.delivery.mode.pickup" /></b>
														${orderData.pickUpPersonFirstName}&nbsp;${orderData.pickUpPersonLastName}
														<br /> ${orderData.pickUpPersonEmail} <br />
														${orderData.pickUpPersonPhone} <br />
													</p>
												</div>
												<c:if test="${not empty orderData.deliveryAddress}">
													<div class="col-6">
														<p class="gray80 body14">
															<b class="gray100"><spring:theme
																	code="text.review.page.delivery.pickup.from" /></b>
															<order:addressItem address="${orderData.deliveryAddress}" />
														</p>
													</div>
												</c:if>
											</div>
										</c:when>
										<c:otherwise>
											<h5 class="mb-4">
												<spring:theme code="text.review.page.delivery.title" />
											</h5>
											<div class="row mb-4">
												<div class="col-6">
													<p class="gray80 body14">
														<b class="gray100"><spring:theme
																code="text.review.page.delivery.mode" /></b>
														${orderData.deliveryMode.name}
													</p>
												</div>
												<c:if test="${not empty orderData.deliveryAddress}">
													<div class="col-6">
														<p class="gray80 body14">
															<b class="gray100"><spring:theme
																	code="text.review.page.delivery.shipping.to" /></b>
															<order:addressItem address="${orderData.deliveryAddress}" />
														</p>
													</div>
												</c:if>
											</div>
										</c:otherwise>
									</c:choose>
								</div>
								<div class="reviewCart pb-0">
									<h5 class="mb-4">
										<spring:theme code="text.review.page.payment.title" />
									</h5>
									<multi-checkout-paypal:orderPrintPagePaymentInfo
										orderData="${orderData}"
										paymentInfo="${orderData.paymentInfo}"
										brainTreePaymentInfo="${brainTreePaymentInfoData}" />
								</div>
								<c:if test="${not empty orderData.giftCardData}">
									<multi-checkout-paypal:orderPrintPagePaymentInfoGiftCard
										orderData="${orderData}" />
								</c:if>
							</form>
						</div>
						<div class="col-md-5">
							<div id="orderSummary" class="card">
								<h5>
									<spring:theme code="checkout.multi.order.summary" />
								</h5>
								<hr>
								<table id="costSummary">
									<tbody>
										<tr>
											<td class="gray80"><spring:theme
													code="text.checkout.multi.order.summary.cost.usedGear" /></td>
											<td class="text-end"><format:blPrice
													priceData="${orderData.subTotal}" /></td>
										</tr>
										<tr>
											<td class="gray80"><spring:theme
													code="text.checkout.multi.order.summary.shipping" /></td>
											<td class="text-end"><spring:theme
													code="text.review.print.page.your.rental.order.summery.tbd" /></td>
										</tr>
										<tr>
											<td class="gray80"><spring:theme
													code="text.checkout.multi.order.summary.tax" /></td>
											<td class="text-end"><spring:theme
													code="text.review.print.page.your.rental.order.summery.tbd" /></td>
										</tr>
										<tr class="discount">
											<c:if test="${orderData.totalDiscounts.value > 0}">
												<td><spring:theme code="text.discount" /></td>
												<td class="text-end" id="cart-shipping-tax">- <format:blPrice
														priceData="${orderData.totalDiscounts}" />
												</td>
											</c:if>
										</tr>
										<tr class="total">
											<td><spring:theme code="basket.page.total" /></td>
											<td class="text-end"><format:price
													priceData="${orderData.totalPrice}" /></td>
										</tr>
									</tbody>
								</table>
							</div>
            </div>
            <div class="text-start mt-3">
							<a href="${orderConfirmationPageUrl}${orderData.code}"
								class="btn btn-primary"><spring:theme
									code="text.order.confirmation.print.page" /></a>
						</div>
          </div>
				</div>
			</div>
		</div>
	</section>
	<!-- Required for ALL pages - JQuery and Bootstrap framework -->
	<script
		src="https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>
	<script
		src="${fn:escapeXml(commonResourcePath)}/js/bootstrap.bundle.min.js"></script>

	<script>
        // Initialize Tooltips
        var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="tooltip"]'))
        var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
          return new bootstrap.Tooltip(tooltipTriggerEl)
        })
        // Mobile Menu styles - #my-menu is required for ALL pages
        document.addEventListener(
            "DOMContentLoaded", () => {
                /* new Mmenu( "#my-menu", {
                    extensions: ["fullscreen","position-front"],
                    navbars		: [{
                        position: "top",
                        content : [ "close", "logo" ]
                    }],
                }  );*/
            }
        );
    </script>

</body>
</html>
