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

<spring:url value="/checkout/multi/summary/braintree/placeOrder"
	var="placeOrderUrl" />
<spring:url value="/checkout/multi/termsAndConditions"
	var="getTermsAndConditionsUrl" />

<c:url value="/cart" var="cartPageUrl" />
<c:url value="/checkout/multi/delivery-method/chooseShipping"
	var="shippingPageUrl" />
<c:url value="/checkout/multi/payment-method/add" var="paymentPageUrl" />
<c:url value="/checkout/multi/summary/braintree/reviewPrint"
	var="printReviewUrl" />

<section id="cartProcess">
	<div class="container">
		<div id="cartSteps" class="row justify-content-center">
			<div class="col-xl-10">
			    <a href="${cartPageUrl}" class="text-decoration-none">
				    <span class="step1 complete"><i class="icon-check"></i> <spring:theme
						code="text.checkout.multi.order.UsedGear" /></span>
				</a>
				<span class="step2 complete"><i class="icon-check"></i> <spring:theme
						code="text.review.page.delivery.or.pickup" /></span>
				<a href="${paymentPageUrl}" class="text-decoration-none">
					<span class="step3 complete"><i class="icon-check"></i> <spring:theme
						code="text.review.page.payment" /></span>
				</a>
				<a href="#" onClick="window.location.reload(true)" class="text-decoration-none">
					<span class="step4 active"><i class="number">4</i>
					    <spring:theme code="text.review.page.review" />
					</span>
			    </a>
			</div>
		</div>
		<div class="row justify-content-center">
			<div class="col-xl-10">
				<div class="row">
					<div id="order" class="col-lg-7">
						<h1>
							<spring:theme code="text.gift.card.purchase.review.page.title" />
						</h1>
						<hr>

						<div class="cartProduct">
							<div class="row">
								<c:forEach items="${allItems}" var="cartEntry">
									<div class="col-md-2 text-center mt-3">
											<product:productPrimaryImage product="${cartEntry.product}"
											format="thumbnail" />
									</div>

									<div class="col-md-6 mt-3">

										<b>${cartEntry.product.name}</b>
									</div>

									<div class="col-md-4 mt-3">
										<p class="text-md-end">
											<spring:theme
												code="text.gift.cart.purchase.review.page.amount" />
											<b><format:price priceData="${cartEntry.totalPrice}"
													displayFreeForZero="true" /></b>
										</p>

									</div>

								</c:forEach>
							</div>
							<c:forEach items="${cartData.entries}" var="entry">
								<div class="row mt-3">
									<div class="col-md-10 offset-md-2">
										<form:form method="POST" modelAttribute="giftCardPurchaseForm"
											id="giftCardPurchaseForm">
											<div class="gc-pdp-form">
												<input type="text" class="form-control" id="first-name"
													placeholder="<spring:theme code='giftcard.PurchaseForm.name.placeholder' />"
													name="name" value="${entry.recipientName}"> <input
													type="text" class="form-control" id="email"
													value="${entry.recipientEmail}"
													placeholder="<spring:theme code='giftcard.PurchaseForm.email.placeholder' />"
													name="email">
												<textarea class="form-control mt-2 mb-4"
													placeholder="<spring:theme code='giftcard.PurchaseForm.message.placeholder' />"
													name="message">${entry.recipientMessage}</textarea>
											</div>
											<div class="notification notification-warning mb-2 gc-error-message"
                      								style="display: none;"></div>
										</form:form>
									</div>
								</div>
							</c:forEach>
						</div>

						<div class="reviewCart">
							<h5 class="mb-4">
								<spring:theme code="text.review.page.payment.title" />
								<a href="#" class="edit-cart lightteal float-end reviewEdit"
									data-bs-toggle="modal" data-bs-target="#editWarning"
									data-section="payment" data-redirect-url="${paymentPageUrl}">
									<spring:theme code="text.review.page.edit.title" />
								</a>
							</h5>
							<multi-checkout-paypal:paymentInfo cartData="${cartData}"
								paymentInfo="${cartData.paymentInfo}"
								brainTreePaymentInfo="${brainTreePaymentInfoData}" />
						</div>
						<c:if test="${not empty cartData.giftCardData}">
							<multi-checkout-paypal:paymentInfoGiftCard cartData="${cartData}" />
						</c:if>
						<form:form action="${placeOrderUrl}" id="placeOrderForm1"
							modelAttribute="placeOrderForm">
							
						<b><spring:theme code="text.review.page.order.notes" /></b>
							 <input type="text" class="form-control order-notes"
								name="orderNotes" id="notes"
								placeholder="<spring:theme code="text.review.page.order.notes.placeholder"/>"
								min="1" max="1000" maxlength="1000"
								value="${cartData.orderNotes}" />
							<input type="checkbox" name="newsLetterSubscriptionOpted"
								value="true" checked="checked" id="newsletter" />
							<label for="newsletter"><span class="gray80"><spring:theme
										code="text.review.page.newsletter.checkbox.label" /></span></label>
							<hr class="mt-5">
							<div class="reviewCart">
								<h5 class="mb-4">
									<spring:theme
										code="text.review.page.rental.terms.agreement.title" />
								</h5>
								<div class="row mb-4">
									<agreementInfo:rentalTermsAgreementInfo />
									<div class="notification notification-warning">
										<spring:theme
											code="text.review.page.your.rental.heads.up.message" />
									</div>
								</div>
							</div>
							<div class="rental-terms">
								<b><spring:theme code="text.review.page.terms.check" /></b>
							</div>

							<div class="cart-actions">
								<input type="hidden" id="shipsFromPostalCode"
									name="shipsFromPostalCode" value="${shipsFromPostalCode}">
								<button id="placeOrder" type="button"
									class="btn btn-sm btn-primary float-end">
									<spring:theme code="checkout.summary.placeOrder"
										text="Place Your Order" />
								</button>
							</div>
						</form:form>
					</div>
					<div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">
						<cart:blGiftCardPurchaseOrderSummaryForReviewPage
							cartData="${cartData}" emptyCart="${emptyCart}" />
						<c:if test="${not empty cartData.potentialOrderPromotions}">
							<c:forEach items="${cartData.potentialOrderPromotions}"
								var="promotion">
								<c:if
									test="${fn:containsIgnoreCase(promotion.promotionData.code, 'free_shipping')}">
									<div class="notification notification-tip truck">
										<spring:theme code="text.free.shipping.promo.applied.message" />
									</div>
								</c:if>
							</c:forEach>
						</c:if>
						
					</div>
				</div>
			</div>
		</div>
	</div>
</section>
<cart:damageWaiverInfo />
<div class="modal fade" id="editWarning" tabindex="-1"
	aria-hidden="true">
	<div class="modal-dialog modal-dialog-centered modal-sm">
		<div class="modal-content">
			<div class="modal-header">
				<h5 class="modal-title">
					<spring:theme code="shipping.interception.change.date.warning.wait" />
				</h5>
				<button type="button" class="btn-close" aria-label="Close"
					id="shippingCloseIconModal"></button>
			</div>
			<div class="modal-body">
				<input type="hidden" value="" id="rentalStartDate"> <input
					type="hidden" value="" id="rentalEndDate"> <input
					type="hidden" value="" id="urlToRedirect"> <input
					type="hidden" value="" id="clickedSection">
				<p class="body14">
					<spring:theme
						code="shipping.interception.change.date.warning.message" />
				</p>
				<a href="#" class="btn btn-primary btn-block my-4"
					id="continueChanges"> <spring:theme
						code="shipping.interception.change.date.warning.continue" />
				</a>
				<p class="text-center mb-0">
					<a href="#" class="lightteal" aria-label="Close"
						id="shippingCloseModal"> <spring:theme
							code="shipping.interception.change.date.warning.cancel" />
					</a>
				</p>
			</div>
		</div>
	</div>
</div>

<div class="modal fade" id="easyReturns" tabindex="-1"
	aria-hidden="true">
	<div class="modal-dialog modal-dialog-centered modal-md">
		<div class="modal-content">
			<div class="modal-header">
				<h5 class="modal-title">
					<spring:theme code="text.review.page.renting.made.easy.title" />
				</h5>
				<button type="button" class="btn-close" data-bs-dismiss="modal"
					aria-label="Close"></button>
			</div>
			<div class="modal-body">
				<p class="body14">
					<spring:theme
						code="text.review.page.renting.made.easy.message.line1" />
				</p>
				<hr>
				<p class="body14">
					<spring:theme
						code="text.review.page.renting.made.easy.message.line2" />
				</p>
				<p class="body14">
					<spring:theme
						code="text.review.page.renting.made.easy.message.line3" />
				</p>
				<p class="body14">
					<spring:theme
						code="text.review.page.renting.made.easy.message.line4" />
				</p>
			</div>
		</div>
	</div>
</div>
