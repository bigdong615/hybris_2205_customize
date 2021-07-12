<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>

<template:page pageTitle="${pageTitle}">
  <c:choose>
    	<c:when test="${orderData.isRentalCart}">
         <order:blRentalOrderConfirmationPage/>
    	</c:when>
    	<c:otherwise>
         <order:blUsedGearOrderConfirmationPage/>
    	</c:otherwise>
  </c:choose>
</template:page>