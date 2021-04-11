<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:set value="cart/emptyCart" var="emptyCart" />

<template:page pageTitle="${pageTitle}">

  <%--<cart:cartValidation/>
	<cart:cartPickupValidation/> --%>

   <div class="screen"></div>
     <section id="cartProcess" class="cart cart-rental">
          <div class="container">
              <div id="cartSteps" class="row justify-content-center">
                  <div class="col-xl-10">
                      <span class="step1 active"><i class="number">1</i> <spring:theme code="text.checkout.multi.order.rental"/></span>
                      <span class="step2"><i class="number">2</i> <spring:theme code="text.checkout.multi.order.Delivery"/></span>
                      <span class="step3"><i class="number">3</i> <spring:theme code="text.checkout.multi.order.payment"/></span>
                      <span class="step4"><i class="number">4</i> <spring:theme code="text.checkout.multi.order.review"/></span>
                  </div>
              </div>
              <div class="row justify-content-center">
                  <div class="col-xl-10">
                      <div class="row">
                          <div id="order" class="col-lg-7">
                              <h1><spring:theme code="text.rental.cart.title"/></h1>
                              <hr>
                              <p><b><spring:theme code="text.rental.cart.date"/></b>&emsp;<input type="text" class="form-control cart-picker" id="litepicker" placeholder="<spring:theme code="text.rental.cart.select.date"/>"></p>

                              <c:forEach items="${cartData.entries}" var="entry">
                                 <cart:rentalCartItem entry="${entry}" cartData="${cartData}" />
                              </c:forEach>

                              <div class="cart-actions">
                                  <a href="#" class="gray80"><spring:theme code="text.rental.cart.back" /></a>
                                  <a href="#" class="btn btn-sm btn-primary float-end"><spring:theme code="general.continue.button" /></a>
                                  <a href="#" class="btn btn-sm btn-outline float-end me-3"><spring:theme code="general.update.button" /></a>
                              </div>
                              <p class="mt-5 body14 gray60"><spring:theme code="text.rental.cart.msg" /></p>
                          </div>
                          <div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">
                              <div id="orderSummary" class="card">
                                  <h5><spring:theme code="checkout.multi.order.summary"/></h5>
                                  <hr>
                                  <p><b><spring:theme code="text.rental.cart.date"/></b>&emsp;<input type="text" class="form-control cart-picker" id="summary-litepicker" placeholder="<spring:theme code="text.rental.cart.select.date"/>"></p>
                                  <hr>
                                  <table id="costSummary">
                                      <tbody>
                                          <tr>
                                              <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.cost"/></td>
                                              <td class="text-end">$98.00</td>
                                          </tr>
                                          <tr>
                                              <td class="gray80"><spring:theme code="text.cart.damage.waiver"/> <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></td>
                                              <td class="text-end">$98.00</td>
                                          </tr>
                                          <tr>
                                              <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.shipping"/></td>
                                              <td class="text-end">–</td>
                                          </tr>
                                          <tr>
                                              <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.tax"/></td>
                                              <td class="text-end">–</td>
                                          </tr>
                                          <tr class="total">
                                              <td><spring:theme code="basket.page.total"/></td>
                                              <td class="text-end">$160.23</td>
                                          </tr>
                                      </tbody>
                                  </table>
                                  <div class="input-group my-3">
                                    <input type="text" class="form-control" placeholder="<spring:theme code="text.checkout.multi.order.summary.promocode.placeholder"/>">
                                    <div class="input-group-append">
                                      <button class="btn btn-secondary" type="button"><spring:theme code="text.voucher.apply.button.label"/></button>
                                    </div>
                                  </div>
                                  <small class="gray60"><spring:theme code="text.checkout.multi.order.summary.msg"/></small>
                              </div>
                              <div class="notification notification-warning">This is a cart warning.</div>
                              <div class="notification notification-tip truck">Free 2-day shipping on orders over $150.</div>
                              <div class="notification notification-tip check">Free changes or cancellation until Jan 28.</div>
                              <div class="order-actions my-4"><a href="#" alt="Print Order"><i class="icon-print"></i></a><a href="#"><i class="icon-save" alt="Save Order"></i></a><a href="${emptyCart}" alt="Trash Order" class="clear-cart-page"><i class="icon-trash"></i></a></div>
                          </div>
                      </div>
                  </div>
              </div>
          </div>
     </section>



    <div class="modal fade" id="damageWaivers" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-lg">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title">Damage Waivers Explained</h5>
                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
            <div class="modal-body">
                <div class="row mb-4">
                    <div class="text-center col-md-3 col-lg-2">
                        <img src="${themeResourcePath}/assets/gear-guard-plus.png">
                    </div>
                    <div class="col-md-9 col-lg-10">
                        <p><b>GearGuard Pro Full Value Waiver</b></p>
                        <p class="body14">Maximum protection from liability due to unintentional damage or loss, including theft and liquid damage, after the same 12% deductible.</p>
                        <hr>
                    </div>
                </div>
                <div class="row mb-4">
                    <div class="text-center col-md-3 col-lg-2">
                        <img src="${themeResourcePath}/assets/gear-guard.png">
                    </div>
                    <div class="col-md-9 col-lg-10">
                        <p><b>GearGuard Value Waiver</b></p>
                        <p class="body14">Drop something? You’re protected. Just kick in a 12% deductible for any unintended damage.</p>
                        <hr>
                    </div>
                </div>

                <div class="row">
                    <div class="text-center col-md-3 col-lg-2">
                        <img src="${themeResourcePath}/assets/gear-guard-none.png">
                    </div>
                    <div class="col-md-9 col-lg-10">
                        <p><b>No Damage Waiver</b></p>
                        <p class="body14">Any damaged or lost gear is completely on you to pay to repair or replace. Yikes!</p>
                    </div>
                </div>
            </div>
          </div>
      </div>
  </div>

</template:page>
