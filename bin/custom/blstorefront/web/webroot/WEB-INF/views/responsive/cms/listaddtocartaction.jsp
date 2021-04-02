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
        <cart:blUsedGearAddToCart/>
        </c:when>
        <c:otherwise>
          <cart:blRentalGearAddToRental/>
        </c:otherwise>
</c:choose>
