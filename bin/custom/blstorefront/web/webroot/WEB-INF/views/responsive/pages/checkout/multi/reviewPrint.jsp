<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="multi-checkout" tagdir="/WEB-INF/tags/responsive/checkout/multi"%>
<%@ taglib prefix="multi-checkout-paypal" tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/checkout/multi" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="custom-fields" tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/custom/fields" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement"%>
<%@ taglib prefix="checkout" tagdir="/WEB-INF/tags/responsive/checkout/multi"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>	
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="agreementInfo" tagdir="/WEB-INF/tags/responsive/agreementInfo" %>

<c:url value="/checkout/multi/summary/braintree/view" var="reviewPageUrl"/>
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="">
    <title>BorrowLenses - Checkout - Step 4</title>
    
    <!-- Required CSS -->
    <link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/bootstrap.min.css"/>
	<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/mmenu.css"/>
	<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/blstyle.css"/>
	<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/cart.css"/> 
	<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/blcustom.css"/> 
  </head>
  <body class="cart print-quote">    
    <section id="cartProcess" class="pt-5">
        <div class="container">
            <div class="row justify-content-center">
                <div class="col-xl-10">
                    <div class="row">
                        <div id="order" class="col-md-7">
                            <h1><spring:theme code="text.review.page.title"/></h1>
                            <div class="notification notification-warning">
								<spring:theme code="text.review.print.page.your.rental.warning"/>
							</div>
                            <hr>
                            <form>
								<div class="reviewCart pb-0">
									<h5 class="mb-4"><spring:theme code="text.review.page.date.title"/></h5>
									<div class="row">
										<div class="col-5">
											<p class="overline"><spring:theme code="text.review.page.date.start"/></p>
											<p class="printQuoteDate mb-0"><b>${formattedRentalStartDate}</b></p>
											<p class="body14">
	                                        	<c:choose>
	                                        		<c:when test="${fn:containsIgnoreCase(cartData.deliveryMode.shippingGroup, 'SHIP_UPS_OFFICE') == true or fn:containsIgnoreCase(cartData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == true}">
	                                        			<spring:theme code="text.review.page.date.start.delivery.pickup"/>
	                                        		</c:when>
	                                        		<c:otherwise>
	                                        			<spring:theme code="text.review.page.date.start.delivery" arguments="${deliveryMode.carrier }"/>
	                                        		</c:otherwise>
	                                        	</c:choose>    
											</p>
										</div>
										<div class="col-2 text-center">
											<img class="rental-arrow" src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/icon-arrow.svg">
										</div>
										<div class="col-5">
											<p class="overline"><spring:theme code="text.review.page.date.end"/></p>
											<p class="printQuoteDate mb-0"><b>${formattedRentalEndDate}</b></p>
											<p class="body14">
												<c:choose>
													<c:when test="${fn:containsIgnoreCase(cartData.deliveryMode.shippingGroup, 'SHIP_UPS_OFFICE') == true or fn:containsIgnoreCase(cartData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == true}">
														<spring:theme code="text.review.page.date.end.delivery.pickup"/>
													</c:when>
													<c:otherwise>
														<spring:theme code="text.review.page.date.end.delivery" arguments="${deliveryMode.carrier }"/>
													</c:otherwise>
												</c:choose> 
											</p>
										</div>
									</div>
								</div>
								<div class="reviewCart pb-0">
									<h5 class="mb-4"><spring:theme code="text.review.page.your.rental.title"/></h5>
									<c:forEach items="${cartData.entries}" var="cartEntry" >
										<div class="row mb-4">
											<div class="col-md-3 text-center">
												<product:productPrimaryImage product="${cartEntry.product}" format="thumbnail"/>
											</div>
											<div class="col-md-9 mt-3">
												<p class="gray80 body14">
													<b class="gray100">${cartEntry.product.name}</b>
													<spring:theme code="text.review.page.your.rental.qty"/>  ${cartEntry.quantity }<br>
													<c:choose>
															<c:when test="${cartEntry.gearGuardProFullWaiverSelected }">
																+ <spring:theme code="text.review.page.damage.waiver.gear.plus"/><br>
															</c:when>
															<c:when test="${cartEntry.gearGuardWaiverSelected }">
																+ <spring:theme code="text.review.page.damage.waiver.gear"/><br>
															</c:when>
															<c:otherwise>
																+ <spring:theme code="text.review.page.damage.waiver.gear.no"/><br>
															</c:otherwise>
														</c:choose>
														<c:if test="${not empty cartEntry.selectedOptions}">
															+ ${cartEntry.selectedOptions} <br>
														</c:if>
													<spring:theme code="text.review.page.your.rental.total"/>  <format:price priceData="${cartEntry.totalPrice}" displayFreeForZero="true" />
												 <c:if test="${not empty cartEntry.product.bundleProductReference}">
												 <ul class="checklist mt-4">
                           <c:forEach items="${cartEntry.product.bundleProductReference}" var="bundleItems">
                                <li>  ${bundleItems.productReferenceName}</li>
                           </c:forEach>
                         </ul>
                         </c:if>
												</p>
											</div>
										</div>
									</c:forEach>
								</div>
								<div class="reviewCart pb-0">
									<c:choose>
										<c:when test="${fn:containsIgnoreCase(cartData.deliveryMode.shippingGroup, 'SHIP_UPS_OFFICE') == true or fn:containsIgnoreCase(cartData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == true}">
											<h5 class="mb-4"><spring:theme code="text.review.page.delivery.pickup.title"/></h5>
											<div class="row mb-4">
												<div class="col-6">
													<p class="gray80 body14">
														<b class="gray100"><spring:theme code="text.review.page.delivery.mode.pickup"/></b>
														${cartData.pickUpPersonFirstName}&nbsp;${cartData.pickUpPersonLastName} <br/>
														${cartData.pickUpPersonEmail} <br/>
														${cartData.pickUpPersonPhone} <br/>
													</p>
												</div>
												<c:if test="${not empty deliveryAddress}">
												<div class="col-6">
													<p class="gray80 body14">
														<b class="gray100"><spring:theme code="text.review.page.delivery.pickup.from"/></b>
														<order:addressItem address="${deliveryAddress}"/>
													</p>
												</div>
												</c:if>
											</div>
										</c:when>
										<c:otherwise>
											<h5 class="mb-4"><spring:theme code="text.review.page.delivery.title"/></h5>
											<div class="row mb-4">
												<div class="col-6">
													<p class="gray80 body14">
														<b class="gray100"><spring:theme code="text.review.page.delivery.mode"/></b>
														${deliveryMode.name}
													</p>
												</div>
												<c:if test="${not empty deliveryAddress}">
												<div class="col-6">
													<p class="gray80 body14">
														<b class="gray100"><spring:theme code="text.review.page.delivery.shipping.to"/></b>
														<order:addressItem address="${deliveryAddress}"/>
													</p>
												</div>
												</c:if>
											</div>
										</c:otherwise>
									</c:choose>
								</div>
								<div class="reviewCart pb-0">
									<h5 class="mb-4"><spring:theme code="text.review.page.payment.title"/> </h5>
									<multi-checkout-paypal:paymentInfo cartData="${cartData}" paymentInfo="${cartData.paymentInfo}" brainTreePaymentInfo="${brainTreePaymentInfoData}" />
								</div>
								<c:if test="${not empty cartData.giftCardData}">
									<multi-checkout-paypal:paymentInfoGiftCard cartData="${cartData}"/>
								</c:if>
                            </form>
                        </div>
                        <div class="col-md-5">
                            <div id="orderSummary" class="card">
                                <h5> <spring:theme code="checkout.multi.order.summary"/></h5>
                                <c:if test="${cartData.isRentalCart}">
                                <hr>
                                <p><b><spring:theme code="text.rental.cart.date"/></b>&emsp;${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}</p>
                                </c:if>
                                <hr>
                                <table id="costSummary">
                                    <tbody>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.cost"/></td>
                                            <td class="text-end"><format:blPrice priceData="${cartData.subTotal}"/></td>
                                        </tr>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.cart.damage.waiver"/> <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></td>
                                            <td class="text-end"><format:blPrice priceData="${cartData.totalDamageWaiverCost}"/></td>
                                        </tr>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.shipping"/></td>
                                            <td class="text-end"><spring:theme code="text.review.print.page.your.rental.order.summery.tbd"/></td>
                                        </tr>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.tax"/></td>
                                            <td class="text-end"><spring:theme code="text.review.print.page.your.rental.order.summery.tbd"/></td>
                                        </tr>
                                        <tr class="discount">
                                            <c:if test ="${cartData.totalDiscounts.value > 0}">
				                                 <td ><spring:theme code="text.discount"/></td>
						                         <td class="text-end" id="cart-shipping-tax">
						                             - <format:blPrice priceData="${cartData.totalDiscounts}"/>
				                              	</td>
			                             </c:if>
                                        </tr>
                                        <tr class="total">
                                            <td><spring:theme code="basket.page.total"/></td>
                                            <td class="text-end"><format:price priceData="${cartData.totalPrice}"/></td>
                                        </tr>
                                    </tbody>
                                </table>
                            </div>
                            
                            <div class="notification notification-warning"><spring:theme code="text.review.print.page.your.rental.order.summery.warning"/></div>
                            
                        </div>
                        
                        <div class="text-start mt-3">
                        	<a href="${reviewPageUrl}" class="btn btn-primary"><spring:theme code="text.review.print.page.your.rental.return.to.review"/></a>
                        </div> 
                        
                    </div>
                </div>
            </div>
        </div>    
    </section>   
    <cart:damageWaiverInfo/>
    <!-- Required for ALL pages - JQuery and Bootstrap framework -->  
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>  
    <script src="${fn:escapeXml(commonResourcePath)}/js/bootstrap.bundle.min.js"></script>
    
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
