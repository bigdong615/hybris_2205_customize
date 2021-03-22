<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>
<%@ attribute name="pageData" required="true" type="de.hybris.platform.commerceservices.search.facetdata.ProductSearchPageData" %>

<h6 class="search-term mb-4">&quot; ${searchPageData.categoryCode} &quot;<span class="search-count"> &#040;${searchPageData.pagination.totalNumberOfResults}&#041;
</span></h6>

<div id="filterList" class="col-12">
<nav:facetNavAppliedFilters pageData="${searchPageData}"/>
</div>