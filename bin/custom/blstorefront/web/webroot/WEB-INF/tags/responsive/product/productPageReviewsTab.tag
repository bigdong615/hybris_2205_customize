<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="product" required="true" type="de.hybris.platform.commercefacades.product.data.ProductData"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:set value="${ycommerce:productImage(product, 'product')}" var="primaryImage"/>

<c:if test="${not empty primaryImage.url}">
	<c:url value="${primaryImage.url}" var="primaryImageUrl" context="${originalContextPath}"/>
</c:if>

<c:set var="stockStatus" value="${product.stock.stockLevelStatus.code}"/>
<c:if test="${empty stockStatus}">
	<c:set var="stockStatus" value="inStock"/>
</c:if>
<c:set var="requestUrl" value="${pageContext.request.requestURL}" />
<c:set var="baseUrl" value="${fn:substringBefore(requestUrl,'/WEB-INF')}" />

<script src="//ui.powerreviews.com/stable/4.0/ui.js"></script>

<script>
	window.pwr = window.pwr || function() {
		(pwr.q = pwr.q || []).push(arguments);
	};
	pwr(
			"render",
			{
				api_key : '54a82048-9c20-4a13-a01b-dd5b0415a965',
				locale : 'en_US',
				merchant_group_id : '2120371445',
				merchant_id : '1415200746',
				page_id : '${product.code}',
				review_wrapper_url : '${baseUrl}/rent/product/${product.code}/writeReview/?pr_page_id= ${product.code}',
				REVIEW_DISPLAY_SNAPSHOT_TYPE : 'SIMPLE',
				REVIEW_DISPLAY_PAGINATION_TYPE : 'VERTICAL',

				product : {
					name : '${product.name}',
					url : '${baseUrl}${product.url}',
					image_url : '${primaryImageUrl}',
					description : '${(product.description)}',
					category_name : '${(product.categories[0].name)}',
					manufacturer_id : '${product.manufacturer}',
					upc : '${product.upc}',
					brand_name : '${product.brandName}',
					price : '${product.price.value}',
					in_stock : '${stockStatus}'
				},
				components : {
					ReviewSnippet : 'pr-reviewsnippet',
					ReviewImageSnippet : 'pr-imagesnippet',
					ReviewDisplay : 'pr-reviewdisplay',

				}
			});
</script>
