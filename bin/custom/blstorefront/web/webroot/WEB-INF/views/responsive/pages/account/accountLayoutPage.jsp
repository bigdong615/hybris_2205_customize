<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:url value="/my-account/update-profile" var="updateProfileUrl"/>
<spring:url value="/my-account/update-password" var="updatePasswordUrl"/>
<spring:url value="/my-account/update-email" var="updateEmailUrl"/>
<spring:url value="/my-account/address-book" var="addressBookUrl"/>
<spring:url value="/my-account/payment-details" var="paymentDetailsUrl"/>
<spring:url value="/my-account/orders" var="ordersUrl"/>
<spring:url value="/my-account/bookmarks" var="bookmarksUrl"/>

<template:page pageTitle="${pageTitle}">
<c:url var = "baseUrl" value="/my-account"/>
 <section id="myAccount">
    <div class="container">
       <div class="row justify-content-center">
       <c:if test ="${pageType ne 'orderDetails'}">
          <div id="accountMenu" class="col-lg-3 sticky-lg-top">
             <h6 class="mb-4">
                <c:choose>
                  <c:when test="${not empty user.name}" >
                     <spring:theme code="account.customer.name.prefix"/>
                     &nbsp; ${user.name}!
                  </c:when>
                  <c:otherwise>
                     <spring:theme code="account.salutation.text.name"/>
                  </c:otherwise>
                </c:choose>
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
                      <a href="${baseUrl}/update-email" class="dropdown-item ${blPageType eq 'Change Email'? 'boldCustom' : ''}">
                         <spring:theme code="text.address.email"/>
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/update-password" class="dropdown-item ${blPageType eq 'Change Password'? 'boldCustom' : ''}"">
                         <spring:theme code= "text.update.password" />
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/saved-carts" class="dropdown-item">
                         <spring:theme code= "text.saved.cart" />
                      </a>
                   </li>
                   <li>
                      <a href="${baseUrl}/bookmarks" class="dropdown-item ${blPageType eq 'Bookmarks'? 'boldCustom' : ''}">
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
                   <a href="${baseUrl}/orders"
                   <c:if test="${fn:startsWith(pageTitle, 'Order History') || extendOrderData ne null || cmsPage.uid eq 'pay-bill-success'}">
                     <c:out value="class=active"/>
                 </c:if>>
                             <spring:theme code= "text.orders"/>
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
                   <a href="${baseUrl}/update-email"
                   <c:if test="${blPageType eq 'Change Email'}">
                      <c:out value="class=active"/>
                   </c:if>>
                      <spring:theme code="text.address.email"/>
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/update-password"
                   <c:if test="${blPageType eq 'Change Password'}">
                      <c:out value="class=active"/>
                   </c:if>>
                      <spring:theme code= "text.update.password" />
                   </a>
                </p>
                <hr>
                <p>
                <a href="${baseUrl}/saved-carts"
                                   <c:if test="${fn:startsWith(pageTitle, 'Saved Carts')}">
                                      <c:out value="class=active"/>
                                   </c:if>>
                                      <spring:theme code= "text.saved.cart" />
                                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/bookmarks"
                   <c:if test="${blPageType eq 'Bookmarks'}">
                                         <c:out value="class=active"/>
                   </c:if>>
                      <spring:theme code= "text.bookmarks" />
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/verificationImages"
                      <c:if test="${blPageType eq 'verificationImages'}">
                          <c:out value="class=active"/>
                      </c:if>>
                      <spring:theme code= "text.verification.documents" />
                   </a>
                </p>
                <hr>
                <p>
                   <a href="${baseUrl}/payment-details"
                   <c:if test="${cmsPage.uid eq 'payment-details'}">
                                         <c:out value="class=active"/>
                   </c:if>>
                      <spring:theme code= "text.credit.cards" />
                   </a>
                </p>
             </div>
          </div>
       </c:if>

          <cms:pageSlot position="BodyContent" var="feature" >
             <cms:component component="${feature}" />
          </cms:pageSlot>

       </div>
    </div>
 </section>
 <div class="modal fade" id="gcAlreadyAddedWarning" tabindex="-1" aria-hidden="true">
                                                    <div class="modal-dialog modal-dialog-centered modal-sm" id="addToCartModalDialog"></div>
                                               </div>

</template:page>