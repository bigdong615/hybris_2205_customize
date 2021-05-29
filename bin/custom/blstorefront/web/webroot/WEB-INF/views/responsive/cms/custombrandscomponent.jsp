<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<!-- BL-351 : Homepage Mobile Device Brands Section --> 
<c:if test="${positionAttribute == 'MobileHomePageBrandSectionSlot'}">
	<li class="splide__slide my-auto ${fn:toLowerCase(feature.brandImageTitle)}">
		<c:url value="${feature.urlLink}" var="brandUrl" />
		<a href="${brandUrl}"><img src="${feature.media.url}"></a>
	</li>
</c:if>
<!-- Homepage Desktop Brands Section --> 
<c:if test="${positionAttribute == 'HomePageBrandSectionSlot'}">
		<li class="my-auto ${fn:toLowerCase(feature.brandImageTitle)}">
	<c:url value="${feature.urlLink}" var="brandUrl" />
	<a href="${brandUrl}"><img src="${feature.media.url}"></a>
	</li>
</c:if>
<!-- BL-345 : Homepage Mobile They Borrow From Us Section --> 
<c:if test="${positionAttribute == 'MobileHomePageBorrowFromUsSectionSlot'}">
	<li class="splide__slide my-auto">
		<c:url value="${feature.urlLink}" var="brandUrl" />
		<a href="${brandUrl}"><img src="${feature.media.url}"></a>
	</li>
</c:if>
<!-- Homepage Desktop They Borrow From Us Section --> 
<c:if test="${positionAttribute == 'HomePageBorrowFromUsSectionSlot'}">
	<li class="my-auto">
	<c:url value="${feature.urlLink}" var="brandUrl" />
	<a href="${brandUrl}"><img src="${feature.media.url}"></a>
	</li>
</c:if>


