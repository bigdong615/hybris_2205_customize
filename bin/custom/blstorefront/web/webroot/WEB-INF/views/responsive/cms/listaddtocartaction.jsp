<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>

<%-- Added for rentalgear and usedgear pages --%>
<c:choose>
        <c:when test="${blPageType eq 'usedGear'}">
        <c:choose>
         <c:when test="${product.retailGear eq true}">
           <cart:blNewGearAddtoCart/>

         </c:when>
         <c:otherwise>
          <c:url var="usedGearUrl" value="/buy/product/${product.code}"/>
           <a href="${usedGearUrl}" class="btn btn-primary btnwidthusedgear">
            <spring:theme code="text.plp.see.pricing"/>
           </a>

         </c:otherwise>
         </c:choose>


        </c:when>
        <c:otherwise>
          <cart:blRentalGearAddToRental/>
        </c:otherwise>
</c:choose>
