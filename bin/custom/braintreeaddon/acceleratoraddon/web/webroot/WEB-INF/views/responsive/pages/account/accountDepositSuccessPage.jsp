<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="account" tagdir="/WEB-INF/tags/addons/blassistedservicestorefront/order"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:url value="/" var="homePageUrl" />
<c:url value="/my-account/order/${orderData.code}" var="viewOrderAction" />

<div id="accountContent" class="col-lg-5 offset-lg-1">

	<h1><spring:theme code="order.myaccount.deposit.payment.success.deposit.received" /></h1>
	<hr>
	<h5 class="mb-5"><spring:theme code="order.myaccount.deposit.payment.success.msg" arguments="${paymentType}"/> &nbsp <format:price priceData="${depositAmount}" displayFreeForZero="false"/></h5> 
	<div class="confirmation-actions my-5">
		<a href="${viewOrderAction}" class="btn btn-primary mx-3 mb-4 mb-sm-0"><spring:theme code="order.myaccount.deposit.payment.success.view.order" /></a> 
		<a href="${homePageUrl}" class="btn btn-outline mx-3 mb-4 mb-sm-0"><spring:theme code="order.myaccount.deposit.payment.success.home" /></a>
	</div>
</div>
