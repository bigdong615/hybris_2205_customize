<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>
<%@ attribute name="pageData" required="true" type="de.hybris.platform.commerceservices.search.facetdata.ProductSearchPageData" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

  <!---BL-607:remove extra space from h6 tag -->
<c:choose>
   <c:when test="${not empty searchPageData.freeTextSearch && searchPageData.freeTextSearch ne null}">
      <h6 class="search-term mb-4">&quot;${searchPageData.freeTextSearch}&quot;<span class="search-count"> &#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span></h6>
   </c:when>
   <c:otherwise>
   
    <!-- Added condition for BL-85  -->
  
     <c:choose>
		<c:when
			test="${pageType == 'PRODUCTSEARCH' && blPageType == 'rentalGear'}">
			<h6 class="search-term mb-4">
				&quot;<spring:theme code="text.rental.gear.slp"/>&quot;<span class="search-count">
					&#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span>
			</h6>
		</c:when>		
		<c:when
			test="${pageType == 'PRODUCTSEARCH' && blPageType == 'usedGear'}">
			<h6 class="search-term mb-4">
				&quot;<spring:theme code="text.used.gear.slp"/>&quot;<span class="search-count">
					&#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span>
			</h6>
		</c:when>		
		<c:otherwise> 
		<h6 class="search-term mb-4">
		<c:set var="displayText" value="${searchPageData.categoryCode}"/>
		<c:if test ="${displayText eq 'rentalgear'}">
		   <spring:theme code="text.rental.gear.slp" var="rentalText"/>
			<c:set var="displayText" value="${rentalText}"/>
			&quot;${displayText}&quot;<span class="search-count">
      				&#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span>
		</c:if>
		<c:if test ="${displayText eq 'usedgear'}">
		      <spring:theme code="text.used.gear.slp" var="usedText"/>
    			<c:set var="displayText" value="${usedText}"/>
    			&quot;${displayText}&quot;<span class="search-count">
          				&#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span>
    		</c:if>
    	<c:if test="${searchPageData.categoryCode ne 'usedgear' && searchPageData.categoryCode ne 'rentalgear'}">
			&quot;${categoryName}&quot;<span class="search-count">
				&#040;${searchPageData.pagination.totalNumberOfResults}&#041;</span>
			</c:if>
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