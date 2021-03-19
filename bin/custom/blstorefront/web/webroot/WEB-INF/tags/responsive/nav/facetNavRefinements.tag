<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="pageData" required="true" type="de.hybris.platform.commerceservices.search.facetdata.FacetSearchPageData" %>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>

<div id="productFilter" class="d-none d-lg-block col-lg-3 sticky-lg-top productfilter-width">

            <h6 class="mb-4">Filters</h6>

            <p class="d-flex justify-content-between">Refine by <a class="clear-filters" href="#">Clear all</a></p>
            <hr>
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




