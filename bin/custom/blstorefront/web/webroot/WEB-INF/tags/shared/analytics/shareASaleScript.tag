<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<!-- Begin shareasale integration code -->

<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.merchantID.value')"	var="merchantID" scope="page" />
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.token.value')"	var="token" scope="page" />
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.action.value')"	var="action" scope="page" />
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.version.value')"	var="version" scope="page" />
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.pixel.value')"	var="pixel" scope="page" />


<c:if test = "${pageType == 'ORDERCONFIRMATION'}">
<img id='_SHRSL_img_1'
src='https://www.shareasale.com/sale.cfm?tracking=${orderData.code}&amount=${orderData.subTotal.value}&merchantID=${merchantID}&transtype=${pixel}<c:if test="${order.appliedVouchers}">&couponcode=<c:forEach items="${order.appliedVouchers}" var="voucher" varStatus="status"><c:out value="${voucher}"/><c:if test="${!status.last}">,</c:if></c:forEach></c:if>&skuList=<c:forEach items="${orderData.entries}" var="entry" varStatus="status"><c:out value="${entry.product.name}"/><c:if test="${!status.last}">,</c:if></c:forEach>&pricelist=<c:forEach items="${orderData.entries}" var="entry" varStatus="status"><c:out value="${entry.totalPrice.value}"/><c:if test="${!status.last}">,</c:if></c:forEach>&quantitylist=<c:forEach items="${orderData.entries}" var="entry" varStatus="status"><c:out value="${entry.quantity}"/><c:if test="${!status.last}">,</c:if></c:forEach>' width='1' height='1'>
<script src='https://www.dwin1.com/19038.js' type='text/javascript' defer='defer'></script>
</c:if>
<!-- End sharasale integration code -->