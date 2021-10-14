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

<spring:url value="/checkout/multi/summary/braintree/placeOrder"
	var="placeOrderUrl" />
<spring:htmlEscape defaultHtmlEscape="true" />
<div id="orderSummary" class="card">
	<h5>
		<spring:theme code="checkout.multi.order.summary" />
	</h5>
	<hr>
	  <p>
			<b><spring:theme code="text.rental.cart.date" /></b>&emsp; <input
					type="text" class="form-control cart-picker"
					id="summary-litepicker"
					placeholder="<spring:theme code="text.rental.cart.select.date"/>">
		</p>
	<hr>
	<table id="costSummary">
		<tbody>
			<tr>
				<td class="gray80">
						<spring:theme code="text.checkout.multi.order.summary.cost" />
				</td>
				<td class="text-end" id="cart-shipping-subTotal"><format:blPrice
						priceData="${cartData.subTotal}" /></td>
			</tr>
			<tr>
				<td class="gray80"><spring:theme
						code="text.cart.damage.waiver" /> <a href="#"
						data-bs-toggle="modal" data-bs-target="#damageWaivers"> <i
						class="icon-support"></i>
				    </a>
				</td>
				<td class="text-end" id="cart-shipping-waiver"><format:blPrice
							priceData="${cartData.totalDamageWaiverCost}" />
				</td>
			</tr>
			<c:if test="${cartData.totalOptionsCost.value gt 0}">
				<tr>
					<td class="gray80"><spring:theme
							code="text.cart.rental.options" /> </td>
					<td class="text-end" id="cart-shipping-options"><format:blPrice
							priceData="${cartData.totalOptionsCost}" /></td>
				</tr>
				</c:if>
			<tr>
				<td class="gray80">
				    <c:choose>
                        <c:when test="${pageType =='reviewSummaryPage'}">
                            <spring:theme code="text.checkout.multi.order.summary.shipping.calculated" />
                        </c:when>
                        <c:otherwise>
                            <spring:theme code="text.checkout.multi.order.summary.shipping" />
                        </c:otherwise>
                    </c:choose>
				</td>
				<td class="text-end" id="cart-shipping-cost">
				    <c:choose>
                        <c:when test="${pageType =='reviewSummaryPage'}">
                            <format:price priceData="${cartData.deliveryCost}" />
                        </c:when>
                        <c:otherwise>
                            <format:blPrice priceData="${cartData.deliveryCost}" />
                        </c:otherwise>
                    </c:choose>
				</td>
				<input type="hidden" class="cart-cost" id="${cartData.deliveryCost.formattedValue}">
			</tr>
			<tr>
				<td class="gray80">
				    <c:choose>
                        <c:when test="${pageType =='reviewSummaryPage'}">
                            <spring:theme code="text.checkout.multi.order.summary.tax.calculated" />
                        </c:when>
                        <c:otherwise>
                            <spring:theme code="text.checkout.multi.order.summary.tax" />
                        </c:otherwise>
                    </c:choose>
                </td>
				<td class="text-end" id="cart-shipping-tax">
				    <c:choose>
						<c:when
							test="${pageType =='CART' || cartData.avalaraCalculated ne 'true'}">
							<format:blPrice priceData="${cartData.taxAvalaraCalculated}" />
						</c:when>
						<c:otherwise>
							<format:price priceData="${cartData.taxAvalaraCalculated}" />
						</c:otherwise>
					</c:choose>
				</td>
			</tr>
			<tr class="discount">
				<c:if test="${cartData.totalDiscounts.value > 0}">
					<td><spring:theme code="text.discount" /></td>
					<td class="text-end" id="cart-shipping-tax">- <format:blPrice
							priceData="${cartData.totalDiscounts}" />
					</td>
				</c:if>
			</tr>
			<tr class="total">
				<td><spring:theme code="basket.page.total" /></td>
				<td class="text-end" id="cart-shipping-total"><format:price
						priceData="${cartData.totalPriceWithTax}" /></td>
			</tr>
		</tbody>
	</table>
  <small class="gray60"><spring:theme
			code="text.checkout.multi.order.summary.msg" /></small>
	<div class="cart-actions">
		<form:form action="${placeOrderUrl}" id="placeOrderForm1"
			modelAttribute="placeOrderForm">

			<c:choose>
				<c:when test="${isCustomerHasUnPaidBillOrders}">
					<a href="#" data-bs-toggle="modal" data-bs-toggle="modal"
						data-bs-target="#unpaidBill" 
						class="btn btn-block btn-primary mt-4"> <spring:theme
							code="checkout.summary.placeOrder" text="Place Your Order" />
					</a>
				</c:when>
				<c:otherwise>
					<button id="placeOrderSummary" type="button"
						class="btn btn-block btn-primary mt-4">
						<spring:theme code="checkout.summary.placeOrder"
							text="Place Your Order" />
					</button>
				</c:otherwise>
			</c:choose>



		</form:form>
	</div>
</div>


