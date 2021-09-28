<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<!-- Begin shareasale integration code -->

<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.merchantID.value')"	var="merchantID" scope="page" />
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.token.value')"	var="token" scope="page" />
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.action.value')"	var="action" scope="page" />
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.version.value')"	var="version" scope="page" />
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('shareasale.lead.pixel.value')"	var="pixel" scope="page" />

<c:if test = "${pageType == 'ORDERCONFIRMATION'}">
<img id='_SHRSL_img_1' src='https://www.shareasale.com/sale.cfm?tracking=${orderData.code}&amount=${orderData.subTotal.value}&merchantID=${merchantID}&transtype=${pixel}' width='1' height='1'>
<script src='https://www.dwin1.com/19038.js' type='text/javascript' defer='defer'></script>
</c:if>
<!-- End sharasale integration code -->