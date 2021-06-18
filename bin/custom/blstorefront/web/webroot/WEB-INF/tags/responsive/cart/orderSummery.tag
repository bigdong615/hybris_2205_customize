<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.CartData" %>
<%@ attribute name="emptyCart" required="true" type="String" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>

<spring:url value="/cart/voucher/remove" var="removeVoucherAction" htmlEscape="false"/>

<c:url value="/checkout/removeGiftCard" var="removeGiftCardAction" />
<spring:htmlEscape defaultHtmlEscape="true" />
<spring:url value="/cart/voucher/apply" var="applyVoucher" htmlEscape="false"/>

<div id="orderSummary" class="card">
      <h5>
        <spring:theme code="checkout.multi.order.summary"/>
      </h5>
      <hr>
      <c:choose>
      <c:when test="${cartData.isRentalCart}">
        <p>
        <b><spring:theme code="text.rental.cart.date"/></b>&emsp;
        <input type="text" class="form-control cart-picker" id="summary-litepicker"
            placeholder="<spring:theme code="text.rental.cart.select.date"/>">
        </p>
      </c:when>
      <c:otherwise>
      	<b><spring:theme code="text.used.Gear.cart.timer"/> <span id="usedTimer"></span></b>
      </c:otherwise>
      </c:choose>
      <hr>
      <table id="costSummary">
          <tbody>
              <tr>
                  <td class="gray80"><c:choose><c:when test="${cartData.isRentalCart}"><spring:theme code="text.checkout.multi.order.summary.cost"/></c:when><c:otherwise><spring:theme code="text.checkout.multi.order.summary.cost.usedGear"/></c:otherwise></c:choose></td>
                  <td class="text-end" id="cart-shipping-subTotal"><format:blPrice priceData="${cartData.subTotal}"/></td>
              </tr>
              <c:if test="${cartData.isRentalCart}">
              <tr>
                  <td class="gray80"><spring:theme code="text.cart.damage.waiver"/>
                    <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers">
                        <i class="icon-support"></i>
                    </a>
                  </td>
                  <td class="text-end" id="cart-shipping-waiver"><format:blPrice priceData="${cartData.totalDamageWaiverCost}"/></td>
              </tr>
              </c:if>
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

            <tr class="discount">
              <c:if test ="${cartData.totalDiscounts.value > 0}">
                  <td ><spring:theme code="text.discount"/></td>
               <td class="text-end" id="cart-shipping-discount">
               - <format:blPrice priceData="${cartData.totalDiscounts}"/>
               </td>
              </c:if>
            </tr>
              <tr class="total">
                  <td><spring:theme code="basket.page.total"/></td>
                  <td class="text-end" id="cart-shipping-total"> <format:price priceData="${cartData.totalPriceWithTax}" /> </td>
              </tr>
          </tbody>
      </table>

 <c:if test ="${not empty fn:escapeXml(errorMsg)}">
    <c:set var="errormsgvalid" value="error"/>
 </c:if>
 <c:url value="/cart/voucher/apply" var="voucherUrl"/>
 <form:form action="${voucherUrl}" modelAttribute="voucherForm" method="POST" id="applyVoucherForm">
    <spring:theme code="text.checkout.multi.order.summary.promocode.placeholder" var="voucherplaceholder"/>
    <div class="input-group my-3">
       <form:input type="text" class="form-control ${errormsgvalid} js-voucher-code-text" path="voucherCode" placeholder="${voucherplaceholder}" name="voucherCode"/>
       <div class="input-group-append">
          <button type="submit" class="btn btn-secondary js-voucher-apply-btn">
             <spring:theme code="text.voucher.apply.button.label"/>
          </button>
       </div>
    </div>
 </form:form>
 <small class="gray60"><spring:theme code="text.checkout.multi.order.summary.msg"/></small>
 <c:url value="/cart/voucher/remove" var="voucherRemoveUrl"/>
 <c:forEach items="${cartData.appliedVouchers}" var="voucher" varStatus="loop">
 <form:form action="${voucherRemoveUrl}" modelAttribute="voucherForm" method="POST" id="removeVoucherForm${loop.index}">
    <p class="body14">
    <c:if test="${cartData.totalDiscounts.value > 0}">
       <span class="gray60">${fn:escapeXml(voucher)}</span>

       <form:input hidden="hidden" value="${fn:escapeXml(voucher)}" path="voucherCode" name="voucherCode"/>
       <a href="#" class="js-cart-release-voucher-remove-btn" id="removeVoucherForm${loop.index}"><small>Remove Item</small></a>
       <c:forEach items ="${cartData.promotionAmountMap}" var="amountMap">
          <c:if test ="${amountMap.key eq voucher}">
             <span class="float-end">-${amountMap.value}</span>
          </c:if>
       </c:forEach>
    </c:if>
    </p>
 </form:form>
    </c:forEach>
    <c:forEach items="${cartData.giftCardData}" var="gift" varStatus="loop">
    	<form:form id="removeGiftCardForm${loop.index}"
    		action="${removeGiftCardAction}" method="POST"
    		modelAttribute="giftCardForm">
    		<p class="body14">
    			<c:if test="${cartData.totalDiscounts.value > 0}">
    				<span class="gray60">${fn:escapeXml(gift.code)}</span>
    				<form:input hidden="hidden" value="${fn:escapeXml(gift.code)}"
    					path="giftCardCode" name="giftCardCode" />
    				<a href="#" class="remove-gift-card"
    					id="removeGiftCardForm${loop.index}" data-index="${loop.index}"><spring:theme
    						code="text.remove" /></a>
    				<span class="float-end">${gift.redeemamount}</span>
    			</c:if>
    		</p>
    	</form:form>
    </c:forEach>
</div>
