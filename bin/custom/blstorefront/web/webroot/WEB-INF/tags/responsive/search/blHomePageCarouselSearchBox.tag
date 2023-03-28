<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:url value="/search/" var="searchUrl" />
<spring:url value="/search/autocomplete/{/componentuid}"
	var="autocompleteUrl" htmlEscape="false">
	<spring:param name="componentuid" value="${component.uid}" />
</spring:url>

<form name="search_form_${fn:escapeXml(component.uid)}" method="get" action="${fn:escapeXml(searchUrl)}">
	<div class="input-group">
		<ycommerce:testId code="header_search_input">
			<c:set var="optionsJson">
                     {
                     "autocompleteUrl" : "${ycommerce:encodeJSON(autocompleteUrl)}",
                     "minCharactersBeforeRequest" : "${ycommerce:encodeJSON(component.minCharactersBeforeRequest)}",
                     "waitTimeBeforeRequest" : "${ycommerce:encodeJSON(component.waitTimeBeforeRequest)}",
                     "displayProductImages" : "${ycommerce:encodeJSON(component.displayProductImages)}"
                     }
            </c:set>
			<c:if test="${positionAttribute == 'HomePageBannerSearchBoxSlot'}">
				<input type="text" id="js-site-search-input"
					class="form-control js-site-search-input d-desk"
					placeholder="<spring:theme code="text.hero.banner.searchbox.placeholder" />"
					name="text" value="" maxlength="100"
					data-options="${fn:escapeXml(optionsJson)}">

				<input type="text" id="litepicker" class="form-control"
					placeholder="<spring:theme code="text.hero.banner.searchbox.datepicker.placeholder" />" autocomplete="off">
			</c:if>
			<c:if test="${positionAttribute == 'MobileHomePageBannerSearchBoxSlot'}">
				<input type="text" id="js-site-search-input-mob"
					class="d-inline-block form-control js-site-search-input-mobile d-mob"
					placeholder="<spring:theme code="text.rental.search.placeholder" />"
					name="text" value="" maxlength="100"
					data-options="${fn:escapeXml(optionsJson)}">

				<input type="text" id="mobile-litepicker"
					class="form-control d-inline-block"
					placeholder="<spring:theme code="text.hero.banner.searchbox.mobile.datepicker.placeholder" />">
			</c:if>
			<input type = "hidden" value="rentalGear" name="blPageType" id="js-page-type"/>
		</ycommerce:testId>

		<div class="input-group-append d-md-block input-button-search">
			<button class="btn btn-search btn-link js_search_button js-search-track"
				type="submit">
				<spring:theme code="text.footer.subscription.button.search" />
			</button>
		</div>
	</div>
</form>