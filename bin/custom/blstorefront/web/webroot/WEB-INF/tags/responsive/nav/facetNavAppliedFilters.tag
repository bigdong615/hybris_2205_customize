<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="pageData" required="true" type="de.hybris.platform.commerceservices.search.facetdata.ProductSearchPageData" %>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:if test="${not empty pageData.breadcrumbs && pageData.pagination.totalNumberOfResults > 0}">
	<div class="facet js-facet">
	<div class="js-facet-name">
		<span class="glyphicon facet__arrow"></span>
	</div>
		<div class="facet__values js-facet-values">
				<c:choose>
              <c:when test="${pageType == 'PRODUCTSEARCH'}">
              <c:forEach items="${pageData.breadcrumbs}" var="breadcrumb">
              						<c:url value="${breadcrumb.removeQuery.url}" var="removeQueryUrl"/>
              						    <a href="${fn:escapeXml(removeQueryUrl)}&blPageType=${blPageType}" class="btn btn-filter">${fn:escapeXml(breadcrumb.facetValueName)}</a>
              				</c:forEach>
              </c:when>
              <c:otherwise>
              <c:choose>
              <c:when test="${pageData.breadcrumbs.size()<2 && superCategory ne null}">
              <c:url var= "clearUrl" value ="${brandClear}"/>
                <c:forEach items="${pageData.breadcrumbs}" var="breadcrumb">
                             						    <a href="${fn:escapeXml(clearUrl)}" class="btn btn-filter">${fn:escapeXml(breadcrumb.facetValueName)}</a>
                 </c:forEach>
              </c:when>
              <c:when test="${pageData.breadcrumbs.size()<2 && usedsuperCategory ne null}">
                            <c:url var= "clearUrl" value ="${usedClear}"/>
                              <c:forEach items="${pageData.breadcrumbs}" var="breadcrumb">
                                           						    <a href="${fn:escapeXml(clearUrl)}" class="btn btn-filter">${fn:escapeXml(breadcrumb.facetValueName)}</a>
                               </c:forEach>
                            </c:when>
              <c:otherwise>
               <c:forEach items="${pageData.breadcrumbs}" var="breadcrumb">
               						<c:url value="${breadcrumb.removeQuery.url}" var="removeQueryUrl"/>
               						    <a href="${fn:escapeXml(removeQueryUrl)}" class="btn btn-filter">${fn:escapeXml(breadcrumb.facetValueName)}</a>
               				</c:forEach>
              </c:otherwise>
              </c:choose>
              </c:otherwise>
              </c:choose>
			</ul>
		</div>
	</div>

</c:if>