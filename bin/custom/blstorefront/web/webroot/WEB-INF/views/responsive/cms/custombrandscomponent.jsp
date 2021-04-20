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
	<c:if test="${isFirstElement}">
		<li class="me-auto my-auto ${fn:toLowerCase(feature.brandImageTitle)}">
	</c:if>
	<c:if test="${!isFirstElement and !isLastElement}">
		<li class="mx-auto my-auto ${fn:toLowerCase(feature.brandImageTitle)}">
	</c:if>
	<c:if test="${isLastElement}">
		<li class="ms-auto my-auto ${fn:toLowerCase(feature.brandImageTitle)}">
	</c:if>
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
	<c:if test="${isFirstElement}">
		<li class="me-auto my-auto">
	</c:if>
	<c:if test="${!isFirstElement and !isLastElement}">
		<li class="mx-auto my-auto">
	</c:if>
	<c:if test="${isLastElement}">
		<li class="ms-auto my-auto">
	</c:if>
	<c:url value="${feature.urlLink}" var="brandUrl" />
	<a href="${brandUrl}"><img src="${feature.media.url}"></a>
	</li>
</c:if>


