<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<%-- JS configuration --%>
	<script>
		/*<![CDATA[*/
		<%-- Define a javascript variable to hold the content path --%>
		var ACC = { config: {},
				ccError: {},
				payPalError: {},
				blackoutError: {},
				giftCardError: {},
				deActivateAccount: {}};
			ACC.config.contextPath = '${ycommerce:encodeJavaScript(contextPath)}';
			ACC.config.encodedContextPath = '${ycommerce:encodeJavaScript(encodedContextPath)}';
			ACC.config.commonResourcePath = '${ycommerce:encodeJavaScript(commonResourcePath)}';
			ACC.config.themeResourcePath = '${ycommerce:encodeJavaScript(themeResourcePath)}';
			ACC.config.siteResourcePath = '${ycommerce:encodeJavaScript(siteResourcePath)}';
			ACC.config.rootPath = '${ycommerce:encodeJavaScript(siteRootUrl)}';
			ACC.config.CSRFToken = '${ycommerce:encodeJavaScript(CSRFToken.token)}';
			ACC.pwdStrengthVeryWeak = '<spring:theme code="password.strength.veryweak" htmlEscape="false" javaScriptEscape="true" />';
			ACC.pwdStrengthWeak = '<spring:theme code="password.strength.weak" htmlEscape="false" javaScriptEscape="true" />';
			ACC.pwdStrengthMedium = '<spring:theme code="password.strength.medium" htmlEscape="false" javaScriptEscape="true" />';
			ACC.pwdStrengthStrong = '<spring:theme code="password.strength.strong" htmlEscape="false" javaScriptEscape="true" />';
			ACC.pwdStrengthVeryStrong = '<spring:theme code="password.strength.verystrong" htmlEscape="false" javaScriptEscape="true" />';
			ACC.pwdStrengthUnsafePwd = '<spring:theme code="password.strength.unsafepwd" htmlEscape="false" javaScriptEscape="true" />';
			ACC.pwdStrengthTooShortPwd = '<spring:theme code="password.strength.tooshortpwd" htmlEscape="false" javaScriptEscape="true" />';
			ACC.pwdStrengthMinCharText = '<spring:theme code="password.strength.minchartext" htmlEscape="false" javaScriptEscape="true" />';
			ACC.accessibilityLoading = '<spring:theme code="aria.pickupinstore.loading" htmlEscape="false" javaScriptEscape="true" />';
			ACC.accessibilityStoresLoaded = '<spring:theme code="aria.pickupinstore.storesloaded" htmlEscape="false" javaScriptEscape="true" />';
			ACC.config.googleApiKey='${ycommerce:encodeJavaScript(googleApiKey)}';
			ACC.config.googleApiVersion='${ycommerce:encodeJavaScript(googleApiVersion)}';
			
			ACC.ccError.cardNumberInValid = '<spring:theme code="payment.cc.error.card.number.invalid" htmlEscape="false" javaScriptEscape="true" />';
			ACC.ccError.cardNumber = '<spring:theme code="payment.cc.error.card.number" htmlEscape="false" javaScriptEscape="true" />';
			ACC.ccError.cardYear = '<spring:theme code="payment.cc.error.card.year" htmlEscape="false" javaScriptEscape="true" />';
			ACC.ccError.cardMonth = '<spring:theme code="payment.cc.error.card.month" htmlEscape="false" javaScriptEscape="true" />';
			ACC.ccError.cardCVV = '<spring:theme code="payment.cc.error.card.cvv" htmlEscape="false" javaScriptEscape="true" />';
			ACC.ccError.cardCVVInValid = '<spring:theme code="payment.cc.error.card.cvv.invalid" htmlEscape="false" javaScriptEscape="true" />';
			ACC.ccError.allFieldsNotSelected ='<spring:theme code="payment.all.field.not.selected" htmlEscape="false" javaScriptEscape="true" />';
			ACC.ccError.onlyGCSelected = '<spring:theme code="payment.only.giftCard.selected" htmlEscape="false" javaScriptEscape="true" />'
			ACC.ccError.poNumber = '<spring:theme code="payment.error.po.number" htmlEscape="false" javaScriptEscape="true" />'
			
			ACC.payPalError.paypalPaymentFail = '<spring:theme code="text.paypal.payment.error.message" htmlEscape="false" javaScriptEscape="true" />'
			ACC.blackoutError.blockedShipping = '<spring:theme code="blackout.blocked.shipping.error" arguments="${rentalDate.selectedFromDate }" htmlEscape="false" javaScriptEscape="true" />'

			ACC.giftCardError.amount='<spring:theme code="giftcard.pdp.amount.limit" htmlEscape="false" javaScriptEscape="true" />'
			ACC.giftCardError.recipientMessage= '<spring:theme code="giftCard.message.invalid" htmlEscape="false" javaScriptEscape="true" />'
			ACC.giftCardError.emailValidation= '<spring:theme code="giftCard.email.invalid.format" htmlEscape="false" javaScriptEscape="true" />'

			ACC.deActivateAccount.login = '<spring:theme code="login.error.account.deactivate.title" htmlEscape="false" javaScriptEscape="true"/>'
			<c:if test="${request.secure}">
				<c:url var="autocompleteUrl" value="/search/autocompleteSecure" />
			</c:if>
			<c:if test="${not request.secure}">
				<c:url var="autocompleteUrl" value="/search/autocomplete" />
			</c:if>
			ACC.autocompleteUrl = '${ycommerce:encodeJavaScript(autocompleteUrl)}';

			<c:url value="/login" var="loginUrl"/>
			ACC.config.loginUrl = '${ycommerce:encodeJavaScript(loginUrl)}';

			<c:url var="authenticationStatusUrl" value="/authentication/status" />
			ACC.config.authenticationStatusUrl = '${ycommerce:encodeJavaScript(authenticationStatusUrl)}';

			<c:forEach var="jsVar" items="${jsVariables}">
				<c:if test="${not empty jsVar.qualifier}" >
				ACC['${ycommerce:encodeJavaScript(jsVar.qualifier)}'] = '${ycommerce:encodeJavaScript(jsVar.value)}';
				</c:if>
			</c:forEach>
		/*]]>*/
	</script>
	<template:javaScriptAddOnsVariables/>
	
	<%-- generated variables from commonVariables.properties --%>
	<script src="${fn:escapeXml(sharedResourcePath)}/js/generatedVariables.js"></script>