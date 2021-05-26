<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>p
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="component" tagdir="/WEB-INF/tags/shared/component"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<c:url value="/removewishlist" var="removeProduct" />



This is bookmarks page
<c:forEach items="${wishlistData}" var="wishlistDatas">
<div>${wishlistDatas.displayName}</div>
<div>${wishlistDatas.code}</div>
 <form:form id="removewishlistForm" action="${removeProduct}" method="post">
 <input type="hidden" name="initialQuantity" value="1" />
     <a href="" class="wishlist_entry-remove bookmarkicons" data-productcode ="${wishlistDatas.code}">REMOVE</a>
 </form:form>
</c:forEach>
