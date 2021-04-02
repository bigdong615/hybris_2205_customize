<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="pageData" required="true" type="de.hybris.platform.commerceservices.search.facetdata.FacetSearchPageData" %>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>


<div id="productFilter" class="d-none d-lg-block col-lg-3 sticky-lg-top">
   <h6 class="mb-4">Filters</h6>
   <c:choose>
      <c:when test="${pageType == 'CATEGORY'}">
            <c:url var= "clearUrl" value ="${breadcrumbs[0].url}"/>
         <p>Refine by <a class="clear-filters" href="${clearUrl}"><spring:theme code="text.button.clear.all"/></a></p>
      </c:when>
      <c:otherwise>
         <p>Refine by <a class="clear-filters" href="?q=${searchPageData.freeTextSearch}"><spring:theme code="text.button.clear.all"/></a></p>
      </c:otherwise>
   </c:choose>
   <%-- Removed Store Facet Entry --%>
   <c:forEach items="${pageData.facets}" var="facet">
            <nav:facetNavRefinementFacet facetData="${facet}"/>
   </c:forEach>
</div>


