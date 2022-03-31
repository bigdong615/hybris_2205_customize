<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="checkout" tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/checkout/multi" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<template:page pageTitle="${pageTitle}" hideHeaderLinks="true">
<c:if test="${not empty accErrorMsgs}">
<c:forEach items="${accErrorMsgs}" var="msg">
 <spring:theme code="${msg.code}" arguments="${msg.attributes}" htmlEscape="false" var="errorMessages"/>
  <input type="hidden" value="${ycommerce:sanitizeHTML(errorMessages)}" class="js-reviewPage-error"/>
   </c:forEach>
</c:if>
<c:choose>
		<c:when test="${cartData.hasGiftCart}">
			<checkout:blGiftCardPurchaseCheckoutSummary />
		</c:when>
		 <c:when test="${cartData.isRetailGearOrder eq true}">
      <checkout:blNewGearCheckoutSummaryPage />
    </c:when>
		<c:when test="${cartData.isRentalCart}">
			<checkout:blRentalCheckoutSummaryPage />
		</c:when>
		<c:otherwise>
			<checkout:blUsedGearCheckoutSummary />
		</c:otherwise>
	</c:choose>
</template:page>
