<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="account"
	tagdir="/WEB-INF/tags/addons/blassistedservicestorefront/order"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:url value="/" var="homePageUrl" />
<c:url value="/my-account/order/${orderData.code}" var="viewOrderAction" />


<div id="accountContent" class="col-lg-5 offset-lg-1">
	<h1>Payment Received</h1>
	<hr>

	<h5 class="mb-5">Thanks for completing this order.</h5>
	<p>
		<c:choose>
			<c:when test="${fn:containsIgnoreCase(modifiedOrderPaymentMethod, 'creditCard')}">
				<spring:theme code="text.extend.order.text" />
				<format:price priceData="${amount}" displayFreeForZero="false" />
		and sent an confirmation email to: <b>${orderData.user.uid}</b>
			</c:when>
			<c:when test="${fn:containsIgnoreCase(modifiedOrderPaymentMethod, 'payPal')}">
				<spring:theme code="text.extend.order.paypal" />
				<format:price priceData="${amount}" displayFreeForZero="false" />
		and sent an confirmation email to: <b>${orderData.user.uid}</b>
			</c:when>
			<c:when test="${fn:containsIgnoreCase(modifiedOrderPaymentMethod, 'poPayment')}">
				<spring:theme code="text.extend.order.po" />
				<format:price priceData="${amount}" displayFreeForZero="false" />
		and sent an confirmation email to: <b>${orderData.user.uid}</b>
			</c:when>
			<c:when test="${fn:containsIgnoreCase(modifiedOrderPaymentMethod, 'giftCard')}">
				<spring:theme code="text.extend.order.gift.card" />
				<format:price priceData="${amount}" displayFreeForZero="false" />
		and sent an confirmation email to: <b>${orderData.user.uid}</b>
			</c:when>
			<c:otherwise>
				<spring:theme code="text.extend.order.refund" arguments="<format:price priceData='${amount}' displayFreeForZero='false' />"/>
			</c:otherwise>
		</c:choose>
		<c:if test="${not empty appliedGcList}">
						<div
							class="notification notification-tip check d-inline-block mb-5">
							<c:forEach items="${appliedGcList}" var="gift"
								varStatus="loop">
								<b><spring:theme code="order.confirmation.page.gift.card"
										arguments="${fn:escapeXml(gift.code)}" /></b>
								<spring:theme code="order.confirmation.page.remaining.balance" />&nbsp;<format:price
									priceData="${gift.balanceamount}" />
								</br>
							</c:forEach>
						</div>
					</c:if>
	</p>
	<div class="confirmation-actions my-5">
		<a href="${viewOrderAction}" class="btn btn-primary mx-3 mb-4 mb-sm-0">View Order</a> 
		<a href="${homePageUrl}" class="btn btn-outline mx-3 mb-4 mb-sm-0">Start a New Rental</a>
	</div>
</div>
