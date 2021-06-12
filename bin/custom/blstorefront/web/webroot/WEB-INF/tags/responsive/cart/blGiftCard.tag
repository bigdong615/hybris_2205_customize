<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.CartData" %>

<c:url value="/checkout/applyGiftCard" var="giftCardUrl" />
<spring:htmlEscape defaultHtmlEscape="true" />
<div class="paymentOption">
	<div class="row">
		<div class="col-12">
        <div class="notification notification-warning">
					<spring:theme code="text.gift.card.disclaimer" />
				</div>
				<b class="mt-4"><spring:theme code="text.gift.title"/></b>
        <form:form method="post" id="giftCardForm" action="${giftCardUrl}">
					<div class="input-group mt-2">
						<input type="text" id="gift-card-apply-gift-card-number" 
							name="giftCardNumber" class="form-control"
							placeholder="Gift Card Number">
						<div class="input-group-append">
						
							<button id="applyGcCode" class="btn btn-secondary" type="submit"><spring:theme code="text.gift.apply"/></button>
						</div>
					</div>
					<input type="hidden" name="appliedGC" value="${cartData.giftCardData}"/>
				</form:form>
				<c:if test="${not empty coupon_applied_msg}">
					<div class="notification notification-warning">${coupon_applied_msg}</div>
				</c:if>
    </div>
	</div>
</div>
