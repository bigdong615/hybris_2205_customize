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
<format:price priceData="${wishlistDatas.price}"/>
<c:forEach items="${wishlistDatas.images}" var="productImagePdp">
   <c:if test="${productImagePdp.format eq 'product' and productImagePdp.imageType eq 'GALLERY'}">
                                                                <c:url value="${productImagePdp.url}" var="primaryImagePdpUrl" />
                                                                <c:set value="this is alternate" var="altTextHtml"/>
                                                                <!--BL-534: added <a> tag-->
                                                                    <li class="splide__slide">
                                                                       <a href ="${rentalPDPUrl}"><img src="${primaryImagePdpUrl}"></a</li>
   </c:if>
</c:forEach>
<div>${wishlistDatas.displayName}pppppp</div>
<div>${wishlistDatas.code}</div>
 <form id="removewishlistForm_${loopindex.index}" action="${removeProduct}" method="post" >
 <input type="hidden" name="removeProductEntry" value="${wishlistDatas.code}" id="removeProductEntry{loopindex.index}" />
 <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
 <button type="submit" name="REMOVE">REMOVE</button>
 </form>
</c:forEach>

