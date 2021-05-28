<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<spring:url value="/my-account/update-profile" var="updateProfileUrl"/>
<spring:url value="/my-account/update-password" var="updatePasswordUrl"/>
<spring:url value="/my-account/update-email" var="updateEmailUrl"/>
<spring:url value="/my-account/address-book" var="addressBookUrl"/>
<spring:url value="/my-account/payment-details" var="paymentDetailsUrl"/>
<spring:url value="/my-account/orders" var="ordersUrl"/>

<template:page pageTitle="${pageTitle}">

<!-- Left Navigation Section -->
<c:url var = "baseUrl" value="/my-account"/>
<h6><spring:theme code="account.customer.name.prefix"/>&nbsp; ${user.name}! </h6>
<ul>
<li><a href="${baseUrl}/orders"><spring:theme code= "text.orders" /></a> </li>
<li><a href="${baseUrl}/address-book"><spring:theme code= "text.address" /></a> </li>
<li><a href="${baseUrl}/update-password"><spring:theme code= "text.update.password" /></a> </li>
<li><a href="${baseUrl}/saved-carts"><spring:theme code= "text.saved.cart" /></a> </li>
<li><a href="${baseUrl}/bookmarks"><spring:theme code= "text.bookmarks" /></a> </li>
<li><a href="${baseUrl}/verificationImages"><spring:theme code= "text.verification.images" /></a> </li>
<li><a href="${baseUrl}/creditCarts"><spring:theme code= "text.credit.cards" /></a> </li>
</ul>
<!-- Main Body Section -->
     <div class="account-section">
            <cms:pageSlot position="BodyContent" var="feature" element="div" class="">
               <cms:component component="${feature}" />
            </cms:pageSlot>
        </div>


</template:page>