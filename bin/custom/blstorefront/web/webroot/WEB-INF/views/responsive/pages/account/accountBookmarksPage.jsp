<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>p
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="component" tagdir="/WEB-INF/tags/shared/component"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<c:url value="/removewishlistentry" var="removeProduct" />


${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}pppppppp
${rentalDate.numberOfDays}ooooooo
This is bookmarks page
<c:forEach items="${wishlistData}" var="wishlistDatas" varStatus="loopindex">

<div>${wishlistDatas.displayName}</div>
<div>${wishlistDatas.code}</div>
 <form id="removewishlistForm_${loopindex.index}" action="${removeProduct}" method="post" >
 <input type="hidden" name="removeProductEntry" value="${wishlistDatas.code}" id="removeProductEntry{loopindex.index}" />
 <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
 <button type="submit" name="REMOVE">REMOVE</button>
 </form>
</c:forEach>

