<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<div id="productList" class="col-lg-9">
   <div class="row mb-2">
      <nav:blFacetNavAppliedFilters pageData="${searchPageData}"/>
   </div>
   <nav:searchSpellingSuggestion spellingSuggestion="${searchPageData.spellingSuggestion}" />
   <br>
   <div id="matchingProducts" class="row">
      <c:forEach items="${searchPageData.results}" var="product" varStatus="status">
         <product:productListerGridItem product="${product}"/>
      </c:forEach>
   </div>
</div>