<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>

<%-- ToDo -  need to update used gear html, as of now added the same rental cart html --%>
<c:set value="cart/emptyCart" var="emptyCart" />
<c:url value="/checkout/multi/payment-method/add" var="paymentPageUrl" />

<c:set value="buy/category/usedgear" var="usedGearPlpUrl" />
<c:set value="${cartData.isRentalCart}" var="usedGearflag" />
<c:url value="/" var="homePageUrl" />
<div class="screen"></div>
<section id="cartProcess" class="cart cart-rental">
	<div class="container">
		<div id="cartSteps" class="row justify-content-center">
			<div class="col-xl-10">
			    <a href="#" onClick="window.location.reload(true)" class="text-decoration-none">
				    <span class="step1 active"><i class="number">1</i>
				        <spring:theme code="text.checkout.multi.order.UsedGear" />
				    </span>
				</a>
				<span class="step2"><i class="number">2</i> <spring:theme
						code="text.checkout.multi.order.Delivery" /></span> <span class="step3"><i
					class="number">3</i> <spring:theme
						code="text.checkout.multi.order.payment" /></span> <span class="step4"><i
					class="number">4</i> <spring:theme
						code="text.checkout.multi.order.review" /></span>
			</div>
		</div>
		<div class="row justify-content-center">
			<div class="col-xl-10">
				<div class="row">
					<div id="order" class="col-lg-7">
						<h1>
							<spring:theme code="text.gift.cart.purchase.title" />
						</h1>
						<hr>
						<c:forEach items="${cartData.entries}" var="entry">
							<cart:blGiftCardPurchaseCartItem entry="${entry}"
								cartData="${cartData}" />
						</c:forEach>

					</div>
					<div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">

						<cart:blGiftCartPurchaseOrderSummery cartData="${cartData}" emptyCart="${emptyCart}" />
						<c:if test="${not empty fn:escapeXml(errorMsg)}">
							<div class="notification notification-error">
								${fn:escapeXml(errorMsg)}</div>
						</c:if>
						<%-- <div class="notification notification-warning">This is a cart warning.</div>
                               <c:if test="${not empty cartData.potentialOrderPromotions}">
                                    <c:forEach items="${cartData.potentialOrderPromotions}" var="promotion">
                                    <c:if test="${fn:containsIgnoreCase(promotion.promotionData.code, 'free_shipping')}">
                                       <div class="notification notification-tip truck"><spring:theme code="text.free.shipping.promo.applied.message"/></div>
                                    </c:if>
                                    </c:forEach>
                                </c:if>

                                <div class="notification notification-tip check"><spring:theme code="text.shipping.change.or.cancellation.message"/></div>
                                <div class="notification notification-tip check">Free changes or cancellation until Jan 28.</div> --%>

					</div>
				</div>
			</div>
		</div>
	</div>
	<div class="modal fade" id="clearCartWarning" tabindex="-1"
		aria-hidden="true">
		<div class="modal-dialog modal-dialog-centered modal-sm">
			<div class="modal-content">
				<div class="modal-header">
					<h5 class="modal-title">
						<spring:theme
							code="shipping.interception.change.date.warning.wait" />
					</h5>
					<button type="button" class="btn-close" data-bs-dismiss="modal"
						aria-label="Close"></button>
				</div>
				<div class="modal-body">
					<p class="body14">
						<spring:theme code="text.clear.cart.message" />
					</p>
					<a href="${emptyCart}"
						class="btn btn-primary btn-block my-4 clear-cart-continue"><spring:theme
							code="general.continue.button" /></a>
					<p class="text-center mb-0">
						<a href="#" class="lightteal" data-bs-dismiss="modal"
							aria-label="Close"><spring:theme code="text.button.cancel" /></a>
					</p>
				</div>
			</div>
		</div>
	</div>
</section>

