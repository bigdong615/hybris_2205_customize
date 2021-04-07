<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="pageData" required="true" type="de.hybris.platform.commerceservices.search.facetdata.FacetSearchPageData" %>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>


<div id="productFilter" class="d-none d-lg-block col-lg-3 sticky-lg-top">
   <h6 class="mb-4">Filters</h6>
   <c:choose>
      <c:when test="${pageType == 'CATEGORY'}">
           <c:if test="${clearAllQuery ne null}">
            <c:url var= "clearUrl" value ="${clearAllQuery}"/>
           </c:if>
           <c:if test="${clearAllQuery eq null && breadcrumbs.size()>1}">
            <c:url var= "clearUrl" value ="${breadcrumbs[1].url}"/>
           </c:if>
            <c:if test="${clearAllQuery eq null}">
                       <c:url var= "clearUrl" value ="${breadcrumbs[0].url}"/>
            </c:if>
         <p>Refine by <a class="clear-filters" href="${clearUrl}"><spring:theme code="text.button.clear.all"/></a></p>
      </c:when>
      <c:otherwise>
         <p>Refine by <a class="clear-filters" href="?q=${searchPageData.freeTextSearch}&blPageType=${blPageType}"><spring:theme code="text.button.clear.all"/></a></p>
      </c:otherwise>
   </c:choose>
   <c:forEach items="${pageData.facets}" var="facet">
      <c:choose>
         <c:when test="${facet.code eq 'availableInStores'}">
         </c:when>
         <c:otherwise>
            <nav:facetNavRefinementFacet facetData="${facet}"/>
         </c:otherwise>
      </c:choose>
   </c:forEach>
</div>


