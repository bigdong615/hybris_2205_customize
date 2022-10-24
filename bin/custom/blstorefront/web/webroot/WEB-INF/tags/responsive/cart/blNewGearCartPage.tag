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
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"  %>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:set value="cart/emptyCart" var="emptyCart" />
<c:url value="/cart/updateDamageWaiver" var="cartUpdateDamageWaiverFormAction" />
<c:url value="/checkout/multi/delivery-method/chooseShipping" var="cartDeliveryOrPickupAction" />
<c:url value="/" var="homePageUrl" />
<c:url value="/cart/reviewPrint" var="printQuoteUrl" />
 <div class="screen"></div>
     <section id="cartProcess" class="cart cart-rental">
          <div class="container">
              <div id="cartSteps" class="row justify-content-center">
                  <div class="col-xl-10">
                      <a href="#" onClick="window.location.reload(true)" class="text-decoration-none">
                        <span class="step1 active"><i class="number">1</i> <spring:theme code="text.checkout.multi.order.UsedGear"/></span>
                      </a>
                      <span class="step2"><i class="number">2</i> <spring:theme code="text.checkout.multi.order.Delivery"/></span>
                      <span class="step3"><i class="number">3</i> <spring:theme code="text.checkout.multi.order.payment"/></span>
                      <span class="step4"><i class="number">4</i> <spring:theme code="text.checkout.multi.order.review"/></span>
                  </div>
              </div>
              <div class="row justify-content-center">
                  <div class="col-xl-10">
                      <div class="row">
                          <div id="order" class="col-lg-7">
                              <h1><spring:theme code="text.new.gear.cart.title"/></h1>
                              <hr>
                              <c:forEach items="${cartData.entries}" var="entry">
                                 <cart:blNewGearCartItem entry="${entry}" cartData="${cartData}" />
                              </c:forEach>
                              <div class="cart-actions">
                                  <a href="${homePageUrl}" class="gray80"><spring:theme code="text.newgear.cart.back" /></a>
                              </div>

                              <!--BL-533 changes -->
                              <p class="mt-5 d-none body14 gray60"><spring:theme code="text.rental.cart.msg" /></p>
                          </div>
                          <div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">
                              <cart:orderSummery cartData="${cartData}" emptyCart="${emptyCart}"/>
                          <div class="cart-actions continue-button">
                              <sec:authorize access="hasAnyRole('ROLE_ANONYMOUS')">
                                  <a class="btn btn-sm btn-primary js-login-popup"  data-link="<c:url value='/login/loginpopup'/>" href="#"
                                     data-bs-toggle="modal" data-bs-target="#signIn">
                                     <spring:theme code="general.continue.button" />
                                     <input type="hidden" value="${pageType}" class="js-page-type"/>
                                  </a>
                              </sec:authorize>
                              <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
                                  <a href="${cartDeliveryOrPickupAction}" class="btn btn-sm btn-primary">
                                       <spring:theme code="general.continue.button" />
                                  </a>
                              </sec:authorize>
                           </div>
                              <div id="cart-warning" class="notification notification-warning" style="display:none"><spring:theme code="text.date.range.not.available" /></div>
                              <c:if test="${not empty giftCardCodeRemove}">
                                  <div id="cart-warning" class="notification notification-warning">${giftCardCodeRemove}</div>
                              </c:if>
                              <c:if test="${isGiftCardRemoved eq 'true'}">
                                 <div id="cart-warning" class="notification notification-warning"><spring:theme code="text.gift.card.remove"/></div>
                              </c:if>
                              <div class="order-actions my-4">

                              </div>
                          </div>
                      </div>
                  </div>
              </div>
          </div>
     </section>



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


