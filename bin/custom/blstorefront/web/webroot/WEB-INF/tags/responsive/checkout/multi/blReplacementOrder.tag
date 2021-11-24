<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="checkout" tagdir="/WEB-INF/tags/responsive/checkout/multi" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<spring:url value="/checkout/multi/summary/braintree/placeReplacementOrder"
	var="placeOrderUrl" />

		   <form:form action="${placeOrderUrl}" id="replaceMentplaceOrderForm"
            	modelAttribute="placeOrderForm">
            	<hr class="mt-5">
            	<div class="cart-actions">
            		<input type="hidden" id="shipsFromPostalCode"
            			name="shipsFromPostalCode" value="${shipsFromPostalCode}">
            		<button id="placeOrderButton" type="button"
            			class="btn btn-sm btn-primary float-end js-replacement-order">
            			<spring:theme code="checkout.summary.placeOrder"
            				text="Place Your Order" />
            		</button>
              </div>
        </form:form>