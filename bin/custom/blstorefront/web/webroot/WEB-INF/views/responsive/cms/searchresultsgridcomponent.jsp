<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<div id="productList" class="col-lg-9">
<div class="row mb-2">
        <nav:blFacetNavAppliedFilters pageData="${searchPageData}"/>
 </div>
    <%-- <div class="results">
        <h1><spring:theme code="search.page.searchText" arguments="${searchPageData.freeTextSearch}" htmlEscape="false"/></h1>
    </div> --%>

    <nav:searchSpellingSuggestion spellingSuggestion="${searchPageData.spellingSuggestion}" /> <br>

    <%-- <nav:pagination top="true"  supportShowPaged="${isShowPageAllowed}" supportShowAll="${isShowAllAllowed}"
    searchPageData="${searchPageData}" searchUrl="${searchPageData.currentQuery.url}"  numberPagesShown="${numberPagesShown}"/> --%>

   <div id="matchingProducts" class="row">
        <c:forEach items="${searchPageData.results}" var="product" varStatus="status">
            <product:productListerGridItem product="${product}"/>
        </c:forEach>
   </div>

 <%--   <div id="addToCartTitle" class="display-none">
        <div class="add-to-cart-header">
            <div class="headline">
                <span class="headline-text"><spring:theme code="basket.added.to.basket"/></span>
            </div>
        </div>
    </div> --%>
</div>