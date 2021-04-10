<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="searchUrl" required="true" %>
<%@ attribute name="searchPageData" required="true" type="de.hybris.platform.commerceservices.search.pagedata.SearchPageData" %>
<%@ taglib prefix="pagination" tagdir="/WEB-INF/tags/responsive/nav/pagination" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:if test="${searchPageData.pagination.totalNumberOfResults > 0}">
   <div id="productPagination" class="text-center">
      <c:set var="hasPreviousPage" value="${searchPageData.pagination.currentPage > 0}"/>
      <c:set var="hasNextPage" value="${(searchPageData.pagination.currentPage + 1) < searchPageData.pagination.numberOfPages}"/>
      <c:if test="${hasPreviousPage}">
         <spring:url value="${searchUrl}" var="previousPageUrl" htmlEscape="true">
            <spring:param name="page" value="${searchPageData.pagination.currentPage - 1}"/>
         </spring:url>
      </c:if>
      <c:if test="${hasNextPage}">
         <spring:url value="${searchUrl}" var="nextPageUrl" htmlEscape="true">
            <spring:param name="page" value="${searchPageData.pagination.currentPage + 1}"/>
         </spring:url>
      </c:if>
      <c:choose>
      <c:when test="${pageType == 'PRODUCTSEARCH'}">
       <a href="${previousPageUrl}&blPageType=${blPageType}" class="btn-arrow-left disabled"></a>
      <span class="currentPage">${searchPageData.pagination.currentPage + 1}</span> / <span class="totalPages">${searchPageData.pagination.numberOfPages}</span>
      <a href="${nextPageUrl}&blPageType=${blPageType}" class="btn-arrow-right"></a>
      </c:when>
      <c:otherwise>
       <a href="${previousPageUrl}" class="btn-arrow-left disabled"></a>
            <span class="currentPage">${searchPageData.pagination.currentPage + 1}</span> / <span class="totalPages">${searchPageData.pagination.numberOfPages}</span>
            <a href="${nextPageUrl}" class="btn-arrow-right"></a>
      </c:otherwise>
      </c:choose>
   </div>
</c:if>