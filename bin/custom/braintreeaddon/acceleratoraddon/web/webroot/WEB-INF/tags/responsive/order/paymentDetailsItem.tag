<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="order" required="true" type="de.hybris.platform.commercefacades.order.data.AbstractOrderData" %>
<%@ attribute name="brainTreePaymentInfo" required="false"
	type="com.braintree.hybris.data.BrainTreePaymentInfoData"%>

<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<spring:htmlEscape defaultHtmlEscape="true"/>
<div class="label-order">
    <spring:theme code="text.account.paymentType"/>
</div>
<div class="value-order">
	<c:choose>
		<c:when test="${brainTreePaymentInfo.paymentType eq 'BrainTreePayPalExpress' or brainTreePaymentInfo.paymentType eq 'PayPalAccount'}">
			<img
					src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-small.png"
					alt="PayPal icon" />
			<spring:theme code="paymentMethod.type.PayPal" />
			<div class="applepay-account-email">${fn:escapeXml(brainTreePaymentInfo.email)}</div>
		</c:when>
        <c:when test="${brainTreePaymentInfo.paymentType eq 'ApplePayCard'}">
            <img height="28" width="56"
                src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/logo_apple.png"
                alt="ApplePay icon" />
			<spring:theme code="paymentMethod.type.ApplePay" />
			<div class="applepay-account-email">${fn:escapeXml(brainTreePaymentInfo.email)}</div>
         </c:when>
		<c:when test="${brainTreePaymentInfo.paymentType eq 'VenmoAccount'}">
			<img height="28" width="56"
				 src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/venmo_acceptance_mark.svg"
				 alt="Venmo icon" />
			<spring:theme code="paymentMethod.type.Venmo" />
			${fn:escapeXml(brainTreePaymentInfo.email)}
		</c:when>
		<c:when test="${brainTreePaymentInfo.paymentType eq 'AndroidPayCard'}">
			<img height="28" width="56"
				 src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/googlePay_mark.png"
				 alt="GooglePay icon" />
			<spring:theme code="paymentMethod.type.GooglePay" />
			${fn:escapeXml(brainTreePaymentInfo.email)}
		</c:when>
		<c:otherwise>
				${fn:escapeXml(brainTreePaymentInfo.cardType)},
				${fn:escapeXml(brainTreePaymentInfo.cardNumber)}
    	</c:otherwise>
	</c:choose>
</div>

