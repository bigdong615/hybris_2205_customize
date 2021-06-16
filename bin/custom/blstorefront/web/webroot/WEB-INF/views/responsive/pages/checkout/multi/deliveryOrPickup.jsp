<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="checkout" tagdir="/WEB-INF/tags/responsive/checkout/multi" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:set value="cart/emptyCart" var="emptyCart" />
<c:url value="/cart/updateDamageWaiver" var="cartUpdateDamageWaiverFormAction" />
<c:url value="/cart" var="cart" />

<template:page pageTitle="${pageTitle}">
    <section id="cartProcess" class="cart cart-rental">
        <div class="container">
            <div id="cartSteps" class="row justify-content-center">
                <div class="col-xl-10">
                    <span class="step1 complete"><i class="icon-check"></i> <spring:theme code="text.checkout.multi.order.rental"/></span>
                    <span class="step2 active"><i class="number">2</i> <spring:theme code="text.checkout.multi.order.Delivery"/></span>
                    <span class="step3"><i class="number">3</i> <spring:theme code="text.checkout.multi.order.payment"/></span>
                    <span class="step4"><i class="number">4</i> <spring:theme code="text.checkout.multi.order.review"/></span>
                </div>
            </div>
            <div class="row justify-content-center">
                <div class="col-xl-10">
                    <div class="row">
                        <div id="order" class="col-lg-7">
                            <h1><spring:theme code="text.checkout.multi.order.Delivery"/></h1>
                            <hr>
                            <p>
                                <b><spring:theme code="text.rental.cart.date"/></b>&emsp;
                                <input type="text" class="form-control cart-picker" id="litepicker"
                                placeholder="<spring:theme code="text.rental.cart.select.date"/>">
                            </p>
                            <div class="accordion" id="shippingOptions">
                                <div class="accordion-item shipProduct">
                                    <checkout:fast/>
                                </div>
                                <div class="accordion-item shipProduct">
                                    <checkout:faster/>
                                </div>
                                <div class="accordion-item shipProduct">
                                    <checkout:fastest/>
                                </div>
                            </div><!-- End Accordion -->
                            <div id="showErrorForInputValidation">

                            </div>
                            <div class="cart-actions">
                                <a href="${cart}" class="gray80"><spring:theme code="text.rental.cart.back" /></a>
                                <button type="button" class="btn btn-sm btn-primary float-end" onClick="shippingMethodContinue()">
                                    <spring:theme code="text.checkout.multi.order.delivery.continue"/>
                                </button>
                            </div>
                            <div id="statusUpdateTestMessage">

                            </div>
                            <div class="page-loader-new-layout">
                            	<img src="${themeResourcePath}/assets/bl-loader.gif" alt="Loading.." title="Loading.." id="new_loading_Img">
                            </div>
                        </div>
                        <div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">
                            <cart:orderSummery cartData="${cartData}" emptyCart="${emptyCart}"/>
                            <c:if test ="${not empty fn:escapeXml(errorMsg)}">
                                                          <div class="notification notification-error">
                                                                  ${fn:escapeXml(errorMsg)}
                                                           </div>
                                                         </c:if>
                            <c:if test="${not empty giftCardCodeRemove}">
                                <div class="notification notification-warning">${giftCardCodeRemove}</div>
                            </c:if>
                            <c:if test="${isGiftCardRemoved eq 'true'}">
                                <div class="notification notification-warning"><spring:theme code="text.gift.card.remove"/></div>
                            </c:if>
                                                         <div class="notification notification-error d-none"id="errorMessages_voucher" />
                            <%-- <div class="notification notification-warning">This is a cart warning.</div>
                            <div class="notification notification-tip truck">Free 2-day shipping on orders over $150.</div>
                            <div class="notification notification-tip check">Free changes or cancellation until Jan 28.</div> --%>
                            <div class="order-actions my-4">
                                 <a href="#" alt="Print Order"><i class="icon-print"></i></a>
                                 <a href="#"><i class="icon-save" alt="Save Order"></i></a>
                                 <%--<a href="${emptyCart}" alt="Trash Order" class="clear-cart-page" disabled="disabled"><i class="icon-trash"></i></a>--%>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </section>
<div class="modal fade" id="editWarning" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-sm">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title"><spring:theme code="shipping.interception.change.date.warning.wait"/></h5>
            <button type="button" class="btn-close" aria-label="Close" id="shippingCloseIconModal"></button>
          </div>
          <div class="modal-body"> 
          <input type="hidden" value="" id="rentalStartDate">
          <input type="hidden" value="" id="rentalEndDate">
              <p class="body14"><spring:theme code="shipping.interception.change.date.warning.message"/></p>
              <a href="#" class="btn btn-primary btn-block my-4" id="shippingChangeRentalDate"><spring:theme code="shipping.interception.change.date.warning.continue"/></a>
              <p class="text-center mb-0"><a href="#" class="lightteal" aria-label="Close" id="shippingCloseModal"><spring:theme code="shipping.interception.change.date.warning.cancel"/></a></p>
          </div>
        </div>
      </div>
    </div>
    <div class="modal fade" id="avsCheck" tabindex="-1" aria-hidden="true">
        <div class="modal-dialog modal-dialog-centered">
            <div class="modal-content">
                <div class="modal-header">
                    <h5 class="modal-title"><spring:theme code="shipping.avs.integration.address.popup.header"/></h5>
                    <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                </div>
                <div class="modal-body">
                    <div id="whatYouEnteredBody">
                        <p class="body14"> <b> <spring:theme code="shipping.avs.integration.address.popup.enter"/></b>
                            <div id="whatYouEntered"> </div>
                        </p>
                    </div>
                    <div id="whatWeSuggestBody">
                        <p class="body14"><b> <spring:theme code="shipping.avs.integration.address.popup.suggest"/></b>
                            <div id="whatWeSuggest"> </div>
                        </p>
                    </div>
                    <a href="#" class="btn btn-primary btn-block my-4" onClick="onClickOfSaveSuggestedAddress()">
                        <spring:theme code="shipping.avs.integration.address.popup.suggested"/>
                    </a>
                    <p class="text-center mb-0">
                        <a href="#" class="lightteal" data-bs-dismiss="modal" aria-label="Close" onClick="onClickOfSaveEnteredAddress()">
                            <spring:theme code="shipping.avs.integration.address.popup.entered"/>
                        </a>
                    </p>
              </div>
            </div>
        </div>
    </div>
</template:page>