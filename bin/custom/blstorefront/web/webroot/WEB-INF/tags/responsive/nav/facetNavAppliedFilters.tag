<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="pageData" required="true" type="de.hybris.platform.commerceservices.search.facetdata.ProductSearchPageData" %>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:if test="${not empty pageData.breadcrumbs}">

	<div class="facet js-facet">

	<div class="js-facet-name">
		<span class="glyphicon facet__arrow"></span>
	</div>
		<div class="facet__values js-facet-values">
				<c:forEach items="${pageData.breadcrumbs}" var="breadcrumb">
						<c:url value="${breadcrumb.removeQuery.url}" var="removeQueryUrl"/>
						    <a href="${fn:escapeXml(removeQueryUrl)}" class="btn btn-filter">${fn:escapeXml(breadcrumb.facetValueName)}</a>
				</c:forEach>
			</ul>
		</div>
	</div>

</c:if>