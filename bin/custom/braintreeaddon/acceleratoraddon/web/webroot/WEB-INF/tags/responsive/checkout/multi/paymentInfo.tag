<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.CartData" %>
<%@ attribute name="paymentInfo" required="true" type="de.hybris.platform.commercefacades.order.data.CCPaymentInfoData" %>
<%@ attribute name="showPaymentInfo" required="false" type="java.lang.Boolean" %>
<%@ attribute name="brainTreePaymentInfo" required="false"
	type="com.braintree.hybris.data.BrainTreePaymentInfoData"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<c:if test="${not empty paymentInfo}">
    <%-- <ul class="checkout-order-summary-list">
        <li class="checkout-order-summary-list-heading">
            <div class="title"><spring:theme code="checkout.multi.payment" text="Payment:"></spring:theme></div>
            <div class="address">
				<c:choose>
					<c:when test="${brainTreePaymentInfo.paymentType eq 'BrainTreePayPalExpress' or brainTreePaymentInfo.paymentType eq 'PayPalAccount'}">
						<img
								src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-small.png"
								alt="PayPal icon" />
						<spring:theme code="paymentMethod.type.PayPal" />
						<div class="pp-account-email">${fn:escapeXml(brainTreePaymentInfo.email)}</div>
                     </c:when>
                    <c:when test="${brainTreePaymentInfo.paymentType eq 'ApplePayCard'}">
                        <img height="28" width="56" src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/logo_apple.png"
                            alt="ApplePay icon" />
						<spring:theme code="paymentMethod.type.ApplePay" />
                        <div class="applepay-account-email">${fn:escapeXml(brainTreePaymentInfo.email)}</div>
                     </c:when>
					<c:when test="${brainTreePaymentInfo.paymentType eq 'VenmoAccount'}">
						<img height="28" width="56" src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/venmo_acceptance_mark.svg"
							 alt="Venmo icon" />
						<spring:theme code="paymentMethod.type.Venmo" />
						<div class="venmo-account-email">${fn:escapeXml(brainTreePaymentInfo.email)}</div>
					</c:when>
					<c:when test="${brainTreePaymentInfo.paymentType eq 'AndroidPayCard'}">
						<img height="28" width="46" src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/googlePay_mark.png"
							 alt="GooglePay icon" />
						<spring:theme code="paymentMethod.type.GooglePay" />
						<div class="googlepay-account-email">${fn:escapeXml(brainTreePaymentInfo.email)}</div>
					</c:when>
					<c:otherwise>
							${fn:escapeXml(brainTreePaymentInfo.cardType)},
							${fn:escapeXml(brainTreePaymentInfo.cardNumber)}
					</c:otherwise>
				</c:choose>
				<c:if test="${not empty paymentInfo.billingAddress}">
					<c:if test="${not empty paymentInfo.billingAddress.line1}">${fn:escapeXml(paymentInfo.billingAddress.line1)},
					</c:if>
					<c:if test="${not empty paymentInfo.billingAddress.line2}">${fn:escapeXml(paymentInfo.billingAddress.line2)},
					</c:if>
					<c:if test="${not empty paymentInfo.billingAddress.town}">${fn:escapeXml(paymentInfo.billingAddress.town)},
					</c:if>
					<c:if test="${not empty paymentInfo.billingAddress.region.name}">${fn:escapeXml(paymentInfo.billingAddress.region.name)}&nbsp;
					</c:if>
					<c:if test="${not empty paymentInfo.billingAddress.postalCode}">${fn:escapeXml(paymentInfo.billingAddress.postalCode)},
					</c:if>
					<c:if test="${not empty paymentInfo.billingAddress.country.name}">${fn:escapeXml(paymentInfo.billingAddress.country.name)}
					</c:if>
				</c:if>
            </div>
        </li>
    </ul> --%>
    
    <div class="row mb-4">
		<div class="col-2 text-center">
			<img src="assets/cc-mastercard.png" style="width: 49px;">
		</div>
		<div class="col-5">
			<p class="gray80">
				<b class="gray100">${fn:escapeXml(brainTreePaymentInfo.cardType)}</b>
				${fn:escapeXml(brainTreePaymentInfo.cardNumber)} exp
				${fn:escapeXml(paymentInfo.expiryMonth)},${fn:escapeXml(paymentInfo.expiryYear)}
			</p>
		</div>
		<div class="col-5">
			<p class="gray80">
				<b class="gray100">Order Notes</b> JayZ Superbowl Shoot
			</p>
		</div>
	</div>
</c:if>

