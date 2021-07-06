<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="checkout" tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/checkout/multi" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<template:page pageTitle="${pageTitle}" hideHeaderLinks="true">
  <c:choose>
  		<c:when test="${cartData.isRentalCart}">
        <checkout:blRentalCheckoutSummaryPage/>
  		</c:when>
  		<c:otherwise>
        <checkout:blUsedGearCheckoutSummary/>
  		</c:otherwise>
  </c:choose>
</template:page>
