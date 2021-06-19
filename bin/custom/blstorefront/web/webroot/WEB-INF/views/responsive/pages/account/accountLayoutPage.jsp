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
<c:url var = "baseUrl" value="/my-account"/>

 <section id="myAccount">
    <div class="container">
       <div class="row justify-content-center">
          <div id="accountMenu" class="col-lg-3 sticky-lg-top">
             <h6 class="mb-4">
                <spring:theme code="account.customer.name.prefix"/>
                &nbsp; ${user.name}!
             </h6>
             <div id="accountMobileNav" class="d-block d-lg-none dropdown my-4">
                <button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="accountMobile" data-bs-toggle="dropdown" aria-expanded="false">
                ${blPageType}
                </button>
                <ul class="dropdown-menu" aria-labelledby="accountMobile">
                   <li>
                      <a href="${baseUrl}/orders" class="dropdown-item">
                         <spring:theme code= "text.orders"/>
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/address-book" class="dropdown-item ${blPageType eq 'Addresses'? 'boldCustom' : ''}" >
                         <spring:theme code= "text.address" />
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/update-email" class="dropdown-item">
                         <spring:theme code="text.address.email"/>
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/update-password" class="dropdown-item">
                         <spring:theme code= "text.update.password" />
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/saved-carts" class="dropdown-item">
                         <spring:theme code= "text.saved.cart" />
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/bookmarks" class="dropdown-item">
                         <spring:theme code= "text.bookmarks" />
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/verificationImages" class="dropdown-item">
                         <spring:theme code= "text.verification.documents" />
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/creditCarts" class="dropdown-item">
                         <spring:theme code= "text.credit.cards" />
                      </a>
                   </li>
                </ul>
             </div>
             <div class="d-none d-lg-block">
                <p>
                   <a href="${baseUrl}/orders">
                      <spring:theme code= "text.orders" />
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/address-book"
                   <c:if test="${blPageType eq 'Addresses'}">
                      <c:out value="class=active"/>
                   </c:if> >
                   <spring:theme code= "text.address" />
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/update-email">
                      <spring:theme code="text.address.email"/>
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/update-password">
                      <spring:theme code= "text.update.password" />
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/saved-carts">
                      <spring:theme code= "text.saved.cart" />
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/bookmarks">
                      <spring:theme code= "text.bookmarks" />
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/verificationImages">
                      <spring:theme code= "text.verification.documents" />
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/creditCarts">
                      <spring:theme code= "text.credit.cards" />
                   </a>
                </p>
             </div>
          </div>
          <cms:pageSlot position="BodyContent" var="feature" >
             <cms:component component="${feature}" />
          </cms:pageSlot>
       </div>
    </div>
 </section>

</template:page>