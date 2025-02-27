<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<spring:htmlEscape defaultHtmlEscape="true" />
 <div id="productList" class="col-lg-9">
         <div class="row mb-2">
         <c:if test="${isNewGearCategory ne true}">
        <nav:blFacetNavAppliedFilters pageData="${searchPageData}"/>
        </c:if>
         </div>
            <div id="matchingProducts" class="row">
               <c:forEach items="${searchPageData.results}" var="product" varStatus="status">
                  <c:if test="${product.productType ne 'GIFTCARD'}">
                  <product:productListerGridItem product="${product}" />
                  </c:if>
               </c:forEach>
            </div>
   <nav:pagination searchPageData="${searchPageData}" searchUrl="${searchPageData.currentQuery.url}"/>
</div>
         <%--  Commented For Later Use
         <div id="addToCartTitle" class="display-none">
               <div class="add-to-cart-header">
                  <div class="headline">
                     <span class="headline-text">
                        <spring:theme code="basket.added.to.basket"/>
                     </span>
                  </div>
               </div>
            </div>
         </div>
         --%>
