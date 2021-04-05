<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>
<%@ attribute name="pageData" required="true" type="de.hybris.platform.commerceservices.search.facetdata.ProductSearchPageData" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<c:choose>
   <c:when test="${not empty searchPageData.freeTextSearch && searchPageData.freeTextSearch ne null}">
      <h6 class="search-term mb-4">&quot;${searchPageData.freeTextSearch} &quot;<span class="search-count"> &#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span></h6>
   </c:when>
   <c:otherwise>
   
    <!-- Added condition for BL-85  -->   
     <c:choose>
		<c:when
			test="${pageType == 'PRODUCTSEARCH' && blPageType == 'rentalGear'}">
			<h6 class="search-term mb-4">
				&quot;<spring:theme code="text.rental.gear.slp"/> &quot;<span class="search-count">
					&#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span>
			</h6>
		</c:when>		
		<c:when
			test="${pageType == 'PRODUCTSEARCH' && blPageType == 'usedGear'}">
			<h6 class="search-term mb-4">
				&quot;<spring:theme code="text.used.gear.slp"/> &quot;<span class="search-count">
					&#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span>
			</h6>
		</c:when>		
		<c:otherwise> 
		<h6 class="search-term mb-4">
			&quot;${searchPageData.categoryCode} &quot;<span class="search-count">
				&#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span>
		</h6>
		</c:otherwise>	
		</c:choose>			
		 <!-- condition ends here  -->
   </c:otherwise>
</c:choose>
<div id="filterList" class="col-12">
   <nav:facetNavAppliedFilters pageData="${searchPageData}"/>
   <nav:blSorting top="true"  supportShowPaged="${isShowPageAllowed}" supportShowAll="${isShowAllAllowed}"  searchPageData="${searchPageData}" searchUrl="${searchPageData.currentQuery.url}"  numberPagesShown="${numberPagesShown}"/>
</div>