<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.CartData" %>
<%@ attribute name="emptyCart" required="true" type="String" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<spring:url value="/checkout/multi/summary/braintree/placeOrder" var="placeOrderUrl"/>
<spring:htmlEscape defaultHtmlEscape="true" />
<div id="orderSummary" class="card">
      <h5>
        <spring:theme code="checkout.multi.order.summary"/>
      </h5>
      <hr>
      <c:if test="${cartData.isRentalCart}">
      <p>
        <b><spring:theme code="text.rental.cart.date"/></b>&emsp;
        <input type="text" class="form-control cart-picker" id="summary-litepicker"
            placeholder="<spring:theme code="text.rental.cart.select.date"/>">
        </p>
      </c:if>
      <hr>
      <table id="costSummary">
          <tbody>
              <tr>
                  <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.cost"/></td>
                  <td class="text-end" id="cart-shipping-subTotal"><format:blPrice priceData="${cartData.subTotal}"/></td>
              </tr>
              <tr>
                  <td class="gray80"><spring:theme code="text.cart.damage.waiver"/>
                    <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers">
                        <i class="icon-support"></i>
                    </a>
                  </td>
                  <td class="text-end" id="cart-shipping-waiver"><format:blPrice priceData="${cartData.totalDamageWaiverCost}"/></td>
              </tr>
              <tr>
                  <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.shipping"/></td>
                  <td class="text-end" id="cart-shipping-cost"><format:blPrice priceData="${cartData.deliveryCost}"/></td>
                  <input type="hidden" class="cart-cost" id="${cartData.deliveryCost.formattedValue}">
              </tr>
              <tr>
                  <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.tax"/></td>
                   <td class="text-end" id="cart-shipping-tax">
                  <c:choose>
                    <c:when test="${pageType =='CART' || cartData.avalaraCalculated ne 'true'}">
                           <format:blPrice priceData="${cartData.taxAvalaraCalculated}"/>
                     </c:when>
                     <c:otherwise><format:price priceData="${cartData.taxAvalaraCalculated}"/></c:otherwise>
                  </c:choose>
                </td>
              </tr>
              <c:if test="${not empty cartData.appliedVouchers}">
              <tr class="discount">
              		  <td><spring:theme code="text.discount" /></td>
              			<td class="text-end">-<format:blPrice priceData="${cartData.giftCardDiscount}" /></td>
              </tr>
              </c:if>
              <tr class="total">
                  <td><spring:theme code="basket.page.total"/></td>
                  <td class="text-end" id="cart-shipping-total"><format:blPrice priceData="${cartData.totalPriceWithTax}"/></td>
              </tr>
          </tbody>
      </table>
      <div class="input-group my-3">
        <input type="text" class="form-control" placeholder="<spring:theme code="text.checkout.multi.order.summary.promocode.placeholder"/>">
        <div class="input-group-append">
          <button class="btn btn-secondary" type="button"><spring:theme code="text.voucher.apply.button.label"/></button>
        </div>
      </div>
      <%-- <small class="gray60"><spring:theme code="text.checkout.multi.order.summary.msg"/></small>
      <c:forEach items="${cartData.giftCardData}" var="gift" varStatus="loop">
      		<form:form id="removeVoucherForm${loop.index}"
      			action="${removeVoucherAction}" method="post"
      			modelAttribute="voucherForm">
      			<p class="body14">
      				<span class="gray60">${gift.code}</span> <a href="#"
      					class="js-release-voucher-remove-btn" id="${gift.code}"><spring:theme
      						code="text.remove"/></a><span class="float-end">${gift.redeemamount}</span>
      			</p>
      			<form:input id="${gift.code}" value="${gift.code}" path="voucherCode" />
      		</form:form>
      	</c:forEach> --%>
      	<div class="cart-actions">
                                <form:form action="${placeOrderUrl}" id="placeOrderForm1" modelAttribute="placeOrderForm">
                                        
                               <button id="placeOrder" type="submit" class="btn btn-block btn-primary mt-4">
                                   <spring:theme code="checkout.summary.placeOrder" text="Place Your Order"/>
                               </button>
                              </form:form>
       </div> 
</div>      	
