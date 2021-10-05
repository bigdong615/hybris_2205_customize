<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.OrderData" %>

<div class="page-loader-new-layout">
    <img src="${themeResourcePath}/assets/bl-loader.gif" alt="Loading.." title="Loading.." id="new_loading_Img">
</div>
<c:url value="/my-account/modified-order-gc-payment" var="giftCardUrl" />
<spring:htmlEscape defaultHtmlEscape="true" />
<div class="paymentOption">
	<div class="row">
		<div class="col-12">
				<b class="mt-4"><spring:theme code="text.gift.title"/></b>
				<c:if test="${not empty cartData.giftCardData}">
				<div class="notification notification-warning" style="font-size: 13px;">
        		<spring:theme code="text.gift.card.disclaimer"/>
        </div>
        </c:if>
        <form method="post" id="giftCardForm" action="${giftCardUrl}">
        <input type="hidden" name="${CSRFToken.parameterName}" value="${CSRFToken.token}" />
					<div class="input-group mt-2">
						<input type="text" id="gcCode" name="gcCode" class="form-control gc-textbox" placeholder="Gift Card Number">
					</div>
					<input type="hidden" id="orderCode" name="orderCode" value="${orderData.code}" />
					<input type="hidden" id="paymentAmount" name="paymentAmount" value=""/>
	
				</form>
				<div class="gc-button-left">
							<button id="applyModifiedGcCode" class="btn btn-secondary" type="submit"><spring:theme code="text.gift.apply"/></button>
						</div>
				<c:if test="${not empty coupon_applied_msg}">
					<div class="notification notification-warning" style="font-size: 13px;">${coupon_applied_msg}</div>
				</c:if>
				<c:if test="${not empty giftCardCodeRemove}">
        		<c:forEach items="${giftCardCodeRemove}" var="giftCardRemoved">
                <div class="notification notification-warning" style="font-size: 13px;">${giftCardRemoved}</div>
            </c:forEach>
        </c:if>
    </div>
	</div>
</div>
