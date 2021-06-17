<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="pageData" required="true" type="de.hybris.platform.commerceservices.search.facetdata.FacetSearchPageData" %>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>


<div id="mobileFilter" class="col-12 d-block d-lg-none mb-4">

	<c:choose>
		<c:when test="${pageType == 'CATEGORY'}">
		<c:if test="${clearAllQuery ne null}">
              <c:url var= "clearUrl" value ="${clearAllQuery}"/>
        </c:if>
        <c:if test="${clearAllQuery eq null && breadcrumbs.size()>1}">
             <c:url var= "clearUrl" value ="${breadcrumbs[1].url}"/>
        </c:if>
        <c:if test="${clearAllQuery eq null && breadcrumbs.size()<2}">
            <c:url var= "clearUrl" value ="${breadcrumbs[0].url}"/>
        </c:if>
		<h6>
		<a href="#filter-menu" class="filter-button"><i
			class="icon-filter"></i> Filters</a><a class="clear-filters" href="${clearUrl}"><spring:theme
					code="text.button.clear.all" /></a></h6>
		</c:when>
		<c:otherwise>
			<h6>
		<a href="#filter-menu" class="filter-button"><i
			class="icon-filter"></i> Filters</a><a class="clear-filters" href="?q=${searchPageData.freeTextSearch}&blPageType=${blPageType}"><spring:theme
					code="text.button.clear.all" /></a></h6>
		</c:otherwise>
	</c:choose>
	</div>
	 <nav id="filter-menu">
	 <ul>
	 <li>
	 <span>
     	<c:forEach items="${pageData.facets}" var="facet">
         <nav:mobileFacetNavigationRefinements  facetData="${facet}"/>
      </c:forEach>
                </span>
                </li>
	 </ul>
      </nav>

               