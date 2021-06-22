<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="sec"	uri="http://www.springframework.org/security/tags"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:set value="cart/emptyCart" var="emptyCart" />
<c:url value="/cart/updateDamageWaiver" var="cartUpdateDamageWaiverFormAction" />
<c:url value="/checkout/multi/delivery-method/chooseShipping" var="cartDeliveryOrPickupAction" />
<c:url value="/" var="homePageUrl" />
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
                              <p>
                                <b><spring:theme code="text.rental.cart.date"/></b>&emsp;
                                <input type="text" class="form-control cart-picker" id="litepicker" placeholder="<spring:theme code="text.rental.cart.select.date"/>">
                              </p>
                              <c:forEach items="${cartData.entries}" var="entry">
                                 <cart:blRentalCartItem entry="${entry}" cartData="${cartData}" />
                              </c:forEach>
                              <!-- Form to update the cart total on the selection of damage Waiver from the dropdown -->
								              <form:form id="updateDamageWaiverForm" action="${cartUpdateDamageWaiverFormAction}" method="post" >
					                      <input type="hidden" name="entryNumber" value="" />
					                      <input type="hidden" name="damageWaiverType" value="" />
           						        </form:form>
                              <div class="cart-actions">
                                  <a href="${homePageUrl}" class="gray80"><spring:theme code="text.rental.cart.back" /></a>
                                  <sec:authorize access="hasAnyRole('ROLE_ANONYMOUS')">
                                   <a class="btn btn-sm btn-primary float-end js-login-popup"  data-link="<c:url value='/login/loginpopup'/>" href="#"
                                   data-bs-toggle="modal" data-bs-target="#signIn">
                                    <spring:theme code="general.continue.button" />
                                   </a>
                                  </sec:authorize>
                                  <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
                                  <a href="" class="btn btn-sm btn-primary float-end" id="cart-continue">
                                    <spring:theme code="general.continue.button" />
                                   </a>
                                  </sec:authorize>

                              </div>
                              <!--BL-533 changes -->
                              <p class="mt-5 d-none body14 gray60"><spring:theme code="text.rental.cart.msg" /></p>
                          </div>
                          <div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">
                              <cart:orderSummery cartData="${cartData}" emptyCart="${emptyCart}"/>
                             <c:if test ="${not empty fn:escapeXml(errorMsg)}">
                              <div class="notification notification-error">
                                      ${fn:escapeXml(errorMsg)}
                               </div>
                             </c:if>
                             <div class="notification notification-error d-none"id="errorMessages_voucher"></div>

                              <div id="cart-warning" class="notification notification-warning" style="display:none"><spring:theme code="text.date.range.not.available" /></div>
                              <c:if test="${not empty giftCardCodeRemove}">
                                  <div id="cart-warning" class="notification notification-warning">${giftCardCodeRemove}</div>
                              </c:if>
                              <c:if test="${isGiftCardRemoved eq 'true'}">
                                 <div id="cart-warning" class="notification notification-warning"><spring:theme code="text.gift.card.remove"/></div>
                              </c:if>
                              <%--<div class="notification notification-tip truck">Free 2-day shipping on orders over $150.</div>
                              <div class="notification notification-tip check">Free changes or cancellation until Jan 28.</div> --%>
                              <div class="order-actions my-4">
                                  <a href="#" alt="Print Order"><i class="icon-print"></i></a>
                                  <a href="#"><i class="icon-save" alt="Save Order"></i></a>
                                  <a href="#" alt="Trash Order" class="clear-cart-page" data-bs-toggle="modal" data-bs-target="#clearCartWarning"><i class="icon-trash"></i></a>
                              </div>
                          </div>
                      </div>
                  </div>
              </div>
          </div>
     </section>
	<cart:damageWaiverInfo/>
    
  <div class="modal fade" id="clearCartWarning" tabindex="-1"
  	aria-hidden="true">
  	<div class="modal-dialog modal-dialog-centered modal-sm">
  		<div class="modal-content">
  			<div class="modal-header">
  				<h5 class="modal-title">
  					<spring:theme code="shipping.interception.change.date.warning.wait" />
  				</h5>
  				<button type="button" class="btn-close" data-bs-dismiss="modal"
  					aria-label="Close"></button>
  			</div>
  			<div class="modal-body">
  				<p class="body14">
  					<spring:theme code="text.clear.cart.message" />
  				</p>
  				<a href="${emptyCart}" class="btn btn-primary btn-block my-4 clear-cart-continue"><spring:theme
  						code="general.continue.button" /></a>
  				<p class="text-center mb-0">
  					<a href="#" class="lightteal" data-bs-dismiss="modal"
  						aria-label="Close"><spring:theme
  							code="text.button.cancel" /></a>
  				</p>
  			</div>
  		</div>
  	</div>
  </div>