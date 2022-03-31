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
                    <a href="${cart}" class="text-decoration-none">
                    <span class="step1 complete">
                        <i class="icon-check"></i>
                            <c:choose>
                                <c:when test="${cartData.isRentalCart && cartData.isRetailGearOrder eq false}">
                                    <spring:theme code="text.checkout.multi.order.rental"/>
                                </c:when>
                                <c:otherwise>
                                    <spring:theme code="text.checkout.multi.order.UsedGear"/>
                                </c:otherwise>
                            </c:choose>
                    </span>
                    </a>
                    <a href="#" onClick="window.location.reload(true)" class="text-decoration-none">
                        <span class="step2 active"><i class="number">2</i> <spring:theme code="text.checkout.multi.order.Delivery"/></span>
                    </a>
                    <c:choose>
                     <c:when test="${isReplacementOrderCart eq true}">

                     </c:when>
                     <c:otherwise>
                    <span class="step3"><i class="number">3</i> <spring:theme code="text.checkout.multi.order.payment"/></span>
                    <span class="step4"><i class="number">4</i> <spring:theme code="text.checkout.multi.order.review"/></span>
                    </c:otherwise>
                    </c:choose>
                </div>
            </div>
            <div class="row justify-content-center">
                <div class="col-xl-10">
                    <div class="row">
                        <div id="order" class="col-lg-7">
                            <h1><spring:theme code="text.checkout.multi.order.Delivery"/></h1>
                            <hr>
                            <c:if test="${cartData.isRentalCart && cartData.isRetailGearOrder eq false}">
                            <p>
                                <b><spring:theme code="text.rental.cart.date"/></b>&emsp;
                                <input type="text" class="form-control cart-picker" id="litepicker"
                                placeholder="<spring:theme code="text.rental.cart.select.date"/>">
                            </p>
                            </c:if>
                            <input type="hidden" value="${shippingMethod}" id="shippingMethod">
                            <input type="hidden" value="${previousPage}" id="previousPage">
                            <div class="accordion" id="shippingOptions">
                            <c:if test="${cartData.isRetailGearOrder eq false}">
                                <div class="accordion-item shipProduct">
                                    <checkout:fast/>
                                </div>
                            </c:if>
                            <c:if test="${cartData.isRetailGearOrder eq true}">
                            <input type="hidden" class="js-new-gear-shipping-page" value="true"/>
                            </c:if>
                                <div class="accordion-item shipProduct">
                                    <checkout:faster/>
                                </div>
                            <c:if test="${cartData.isRetailGearOrder eq false && cartData.isRentalCart}">
                                <div class="accordion-item shipProduct">
                                    <checkout:fastest/>
                                </div>
                            </c:if>

                            </div><!-- End Accordion -->
                            <div id="showErrorForInputValidation">

                            </div>
                            <div id="showErrorForInvalidZipInputValidation">

                            </div>
                            <div id="showErrorForInvalidEmailInputValidation">

                            </div>
                            <div id="showErrorForInvalidPhoneInputValidation">

                            </div>
                             <div id="showErrorForUPSOrPickAddressError">

                            </div>

                            <c:if test="${isAvalaraException eq true}">
                            <div class="notification notification-error"> <spring:theme code="text.checkout.avalara.message"/></div>
                            </c:if>

                            <div id="validationMessage"></div>
                            <div class="cart-actions">
                          <a href="${cart}" class="gray80"><c:choose><c:when test="${cartData.isRetailGearOrder eq true}"><spring:theme code="text.newgear.cart.back" /></c:when><c:when test="${cartData.isRentalCart}"><spring:theme code="text.rental.cart.back" /></c:when><c:otherwise><spring:theme code="text.usedGear.cart.back.plp" /></c:otherwise></c:choose></a>
                                <c:choose>
                                  <c:when test="${isReplacementOrderCart eq true}">
                                   <checkout:blReplacementOrder/>
                              </c:when>
                              <c:otherwise>
                              <button type="button" class="btn btn-sm btn-primary float-end" onClick="shippingMethodContinue()">
                                                                  <spring:theme code="text.checkout.multi.order.delivery.continue"/>
                                                              </button>
                              </c:otherwise>
                             </c:choose>
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
                                                          <div class="notification notification-error js-promo-error">
                                                                  ${fn:escapeXml(errorMsg)}
                                                           </div>
                                                         </c:if>

                            <c:if test="${not empty giftCardCodeRemove}">
                                <div class="notification notification-warning">${giftCardCodeRemove}</div>
                            </c:if>
                            <c:if test="${isGiftCardRemoved eq 'true'}">
                                <div class="notification notification-warning"><spring:theme code="text.gift.card.remove"/></div>
                            </c:if>
                               <c:if test="${not empty cartData.potentialOrderPromotions}">
                                     <c:forEach items="${cartData.potentialOrderPromotions}" var="promotion">
                                     <c:if test="${fn:containsIgnoreCase(promotion.promotionData.code, 'free_shipping')}">
                                        <div class="notification notification-tip truck"><spring:theme code="text.free.shipping.promo.applied.message"/></div>
                                     </c:if>
                                     </c:forEach>
                                 </c:if>

                                    <div class="notification notification-error d-none"id="errorMessages_voucher" ></div>
                                  <c:choose>
                                  <c:when test="${isReplacementOrderCart eq true}">

                                  </c:when>
                                  <c:otherwise>
                                    <c:if test="${cartData.isRetailGearOrder eq false && cartData.isRentalCart eq true}">
                                      <div class="notification notification-tip check"><spring:theme code="text.shipping.change.or.cancellation.message"/></div>
                                    </c:if>
                                  </c:otherwise>
                                  </c:choose>


                            <%-- <div class="notification notification-warning">This is a cart warning.</div>--%>
<%--                             <div class="order-actions my-4">
                            <c:if test="${cartData.isRetailGearOrder eq false}">
                                 <a href="#" alt="Print Order"><i class="icon-print"></i></a>
                            </c:if>
                                 <a href="#"><i class="icon-save" alt="Save Order"></i></a>
                                 <a href="${emptyCart}" alt="Trash Order" class="clear-cart-page" disabled="disabled"><i class="icon-trash"></i></a>
                            </div> --%>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </section>
    <cart:damageWaiverInfo/>
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

    <div class="modal fade" id="avsCheckReplacementOrder" tabindex="-1" aria-hidden="true">
            <div class="modal-dialog modal-dialog-centered">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title"><spring:theme code="shipping.avs.integration.address.popup.header"/></h5>
                        <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                    </div>
                    <div class="modal-body">
                        <div id="whatYouEnteredBody">
                            <p class="body14"> <b> <spring:theme code="shipping.avs.integration.address.popup.enter"/></b>
                                <div id="whatYouEnteredForReplacementOrder"> </div>
                            </p>
                        </div>
                        <div id="whatWeSuggestBody">
                            <p class="body14"><b> <spring:theme code="shipping.avs.integration.address.popup.suggest"/></b>
                                <div id="whatWeSuggestForReplacementOrder"> </div>
                            </p>
                        </div>
                        <a href="#" class="btn btn-primary btn-block my-4" onClick="onClickOfSaveSuggestedAddressForReplacementOrder()">
                            <spring:theme code="shipping.avs.integration.address.popup.suggested"/>
                        </a>
                        <p class="text-center mb-0">
                            <a href="#" class="lightteal" data-bs-dismiss="modal" aria-label="Close" onClick="onClickOfSaveEnteredAddressForOrderReplacement()">
                                <spring:theme code="shipping.avs.integration.address.popup.entered"/>
                            </a>
                        </p>
                  </div>
                </div>
            </div>
        </div>

</template:page>