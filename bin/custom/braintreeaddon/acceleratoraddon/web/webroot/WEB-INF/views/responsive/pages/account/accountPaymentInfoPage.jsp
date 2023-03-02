<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>


 <c:choose>
<c:when test="${empty braintreePaymentInfos}">
		<div id="accountContent" class="col-lg-8 offset-lg-1">
			<h3>Credit Cards</h3>
			<hr>
			<div class="notification no-orders">
				<p>
					<strong>You don't have any saved credit cards.</strong>
				</p>
				<p>
					Go ahead and <a href="add-payment-method">add a credit card</a> to speed things up
					for your next order.
				</p>
			</div>
		</div>

	</c:when>

<c:otherwise> 


<div id="accountContent" class="col-lg-8 offset-lg-1">
	<h3>Credit Cards</h3>
	<hr>
	<div class="notification notification-tip cc">
		<p>
			Active and upcoming orders will charge the card used when you placed
			your rental. To change the card used, please�<a href="#">contact
				us</a>.
		</p>
	</div>
	<div id="cardList" class="row mt-5">
		<c:choose>
			<c:when test="${not empty braintreePaymentInfos}">
				<c:forEach items="${braintreePaymentInfos}" var="paymentInfo">


					<c:set var="cardName" value="${fn:escapeXml(paymentInfo.cardType)}" />
					<div class="col-md-6 saved-card">
						<div class="card mb-4">
							<c:choose>
								<c:when test="${paymentInfo.defaultPaymentInfo eq true}">
									<div class="badges">
										<span class="badge badge-default float-start">
										<spring:theme code="text.default.card" /></span>
									</div>
								</c:when>
								<c:otherwise>
									<button
										class="badge badge-outline float-md-start js-set-default-card"
										data-card-default="true"
										data-payment-default-id="${paymentInfo.id}"
										data-payment-default-token="${paymentInfo.paymentMethodToken}">
										<spring:theme code="text.setDefault.card" /> 
									</button>
								</c:otherwise>
							</c:choose>

							<div class="row">
								<div class="col-9">

									<span class="gray60">${fn:escapeXml(paymentInfo.cardType)}<br>
										${fn:escapeXml(paymentInfo.cardNumber)}<br> Exp:
										${fn:escapeXml(paymentInfo.expiryMonth)} /
										${fn:escapeXml(paymentInfo.expiryYear)}
									</span>

									<p class="card-actions mb-0">
									    <!-- Edit link is hidden as per BL-2096 -->
										<%-- <a href="#" class="edit-cc-form" data-id="${paymentInfo.id}">Edit</a> --%>
										<a href="#" class="delete-link"
											data-payment-id="${paymentInfo.id}"
											data-tokan="${paymentInfo.paymentMethodToken}"
											data-bs-toggle="modal" data-bs-target="#clearCartWarning">Remove</a>
									</p>



								</div>
								<div class="col-3 text-md-right">
									<img
										src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-${fn:replace(fn:toLowerCase(cardName),' ', '_')}.png"
										class="cc-image">
								</div>
							</div>
						</div>
					</div>
				</c:forEach>
			</c:when>
			<c:otherwise>
			</c:otherwise>
		</c:choose>
		<%--<div class="col-md-6 saved-card" >
			<div class="card mb-4">
				<div class="row">
					<div class="col-9">
						<b>PayPal makes it easy!</b> <span class="gray60">Select
							the PayPal option during checkout for a fast, secure checkout.</span>
					</div>
					<div class="col-3 text-md-right">
						<img
							src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/img-paypal.png"
							class="cc-image">
					</div>
				</div>
			</div>
		</div> --%>
		<div class="col-12">
			<!-- <button class="btn btn-primary">Add Credit Card</button> -->
			<a class="btn btn-primary" href="add-payment-method"> Add Credit
				Card </a>
		</div>
	</div>
</div>


<form id="braintree-payment-edit-form"
	action="${request.contextPath}/my-account/edit-payment-method"
	method="GET">
	<input type="hidden" id="paymentInfoId" name="paymentInfoId" value="" />
</form>

<c:url value="/my-account/remove-payment-method-bt"
	var="removePaymentActionUrl" />
<form:form id="removePaymentDetails${paymentInfo.id}"
	action="${removePaymentActionUrl}" method="post">
	<input type="hidden" id="paymentInfoIdRemove"
		name="paymentInfoIdRemove" value="" />
	<input type="hidden" id="paymentMethodTokenRomove"
		name="paymentMethodTokenRomove" value="" />

	<div class="modal fade" id="clearCartWarning" tabindex="-1"
		aria-hidden="true">
		<div class="modal-dialog modal-dialog-centered modal-sm">
			<div class="modal-content">
				<div class="modal-header">
					<h5 class="modal-title">
						<spring:theme code="text.account.paymentDetails.deleteLink.title" />
					</h5>
					<button type="button" class="btn-close" data-bs-dismiss="modal"
						aria-label="Close"></button>
				</div>
				<div class="modal-body">
					<p class="body14">
						<spring:theme
							code="text.account.paymentDetails.deleteLink.message" />
					</p>
					<ycommerce:testId code="paymentDetailsDelete_delete_button">
						<button type="submit" data-payment-id="${paymentInfo.id}"
							class="btn btn-primary btn-block my-4 paymentsDeleteBtn">
							<spring:theme code="shipping.interception.change.date.warning.continue" />
						</button>
					</ycommerce:testId>
					<p class="text-center mb-0">
						<a href="#" class="lightteal" data-bs-dismiss="modal"
							aria-label="Close"><spring:theme code="text.button.cancel" /></a>
					</p>
				</div>
			</div>
		</div>
	</div>

</form:form>
 </c:otherwise> 
</c:choose>

<script>
	var accountPaymentInfoPage = "accountPaymentInfoPage";
</script>

