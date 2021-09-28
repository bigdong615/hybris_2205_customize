<%@ attribute name="orderData" required="true" type="de.hybris.platform.commercefacades.order.data.OrderData" %>
<%@ attribute name="paymentInfo" required="true" type="de.hybris.platform.commercefacades.order.data.CCPaymentInfoData" %>
<%@ attribute name="showPaymentInfo" required="false" type="java.lang.Boolean" %>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<c:if test="${not empty paymentInfo}">
<c:set var="cardName" value="${fn:escapeXml(paymentInfo.cardType)}"/>
<c:if test="${empty cardName }">
	<c:choose>
		<c:when test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'BrainTreePayPalExpress')}">
			<c:set var="cardName" value="PayPal"/>
		</c:when>
	</c:choose>
</c:if>
	<div class="row">
		<div class="col-2 text-center">
			<img src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-${fn:replace(fn:toLowerCase(cardName),' ', '_')}.png" style="width: 50px;">
		</div>
		<div class="col-10 col-md-5">
			<b class="body14 gray100">${cardName}</b>
			<div class="row">
				<div class="col-4">
					<p class="body14">
						<c:if test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'BrainTreePayPalExpress') == false }">
						<spring:theme code="text.review.page.payment.ending" /><br>
						<spring:theme code="text.review.page.payment.exp" /><br>
						</c:if>
					</p>
				</div>
				<div class="col-8">
					<p class="body14 gray80">
						<c:if test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'BrainTreePayPalExpress') == false }">
						${fn:escapeXml(paymentInfo.cardNumber)}<br>
						${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)}<br>
						</c:if>
					</p>
				</div>
			</div>
		</div>
		<c:if test="${not empty orderData.orderNotes}">
		 <div class="col-12 col-md-5">
			<p class="gray80 body14">
				<b class="gray100">Order Notes</b> ${orderData.orderNotes}
			</p>
		</div>
	 </c:if>
	</div>
</c:if>

