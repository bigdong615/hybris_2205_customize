<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="cartData" required="true"
	type="de.hybris.platform.commercefacades.order.data.CartData"%>
<%@ attribute name="emptyCart" required="true" type="String"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>

<spring:url value="/cart/voucher/remove" var="removeVoucherAction"
	htmlEscape="false" />

<c:url value="/checkout/removeGiftCard" var="removeGiftCardAction" />
<spring:htmlEscape defaultHtmlEscape="true" />
<spring:url value="/cart/voucher/apply" var="applyVoucher"
	htmlEscape="false" />

<div id="orderSummary" class="card">
	<h5>
		<spring:theme code="text.giftcard.checkout.multi.order.summary" />
	</h5>
	<hr>
	<table id="costSummary">
		<tbody>
			<tr>
				<td class="gray80 pb-3"><spring:theme
						code="text.giftcard.checkout.multi.order.summary.cost" /></td>
				<td class="text-end" id="cart-shipping-subTotal"><format:blPrice
						priceData="${cartData.subTotal}" /></td>
			</tr>

			<tr class="total">
				<td><spring:theme code="basket.page.total" /></td>
				<td class="text-end" id="cart-shipping-total"><format:price
						priceData="${cartData.totalPriceWithTax}" /></td>
			</tr>
		</tbody>
	</table>
	<div class="cart-actions">
     <c:choose>
        <c:when test="${cmsPage.uid eq 'multiStepCheckoutSummaryPage'}">
           <a href="javascript:void(0)" class="btn btn-block btn-primary mt-4" id="submit_silentOrderPostForm">Continue</a>
           <a href="#" class="btn btn-block btn-primary mt-4" id="submit_silentOrderSavedForm">Continue</a>
        </c:when>
        <c:otherwise>
           <a href="#" class="btn btn-block btn-primary mt-4" id="submitCard">
              <spring:theme code="general.continue.button" />
           </a>
        </c:otherwise>
     </c:choose>
  </div>
	</div>
