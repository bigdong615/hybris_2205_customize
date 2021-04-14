<%@ page trimDirectiveWhitespaces="true"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="search" tagdir="/WEB-INF/tags/responsive/search" %>

<!-- Using seprate tag file for search box due to difference in homepage html as compared to other pages -->
<c:choose>
	<c:when
		test="${positionAttribute == 'HomePageBannerSearchBoxSlot' or positionAttribute == 'MobileHomePageBannerSearchBoxSlot'}">
		<search:blHomePageCarouselSearchBox />
	</c:when>
	<c:otherwise>
		<%-- Added For Rentagear and Usedgear Page --%>
		<c:choose>
			<c:when test="${blPageType eq 'usedGear'}">
				<search:blUsedGearSearchBox />
			</c:when>
			<c:otherwise>
				<search:blRentalGearSearchBox />
			</c:otherwise>
		</c:choose>
	</c:otherwise>
</c:choose>
