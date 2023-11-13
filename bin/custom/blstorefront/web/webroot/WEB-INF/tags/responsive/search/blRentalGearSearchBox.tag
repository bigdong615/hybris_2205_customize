<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:url value="/search/" var="searchUrl" />
<spring:url value="/search/autocomplete/{/componentuid}" var="autocompleteUrl" htmlEscape="false">
     <spring:param name="componentuid"  value="${component.uid}"/>
</spring:url>
<div class="wrapup-search">
<section id="globalSearch">
   <div class="container">
      <div  class="row justify-content-center">
         <div class="col-12">
            <form name="search_form_${fn:escapeXml(component.uid)}" method="get" action="${fn:escapeXml(searchUrl)}">
               <div class="input-group rental-search">
               <ycommerce:testId code="header_search_input">
                  <c:set var="optionsJson">
                     {
                     "autocompleteUrl" : "${ycommerce:encodeJSON(autocompleteUrl)}",
                     "minCharactersBeforeRequest" : "${ycommerce:encodeJSON(component.minCharactersBeforeRequest)}",
                     "waitTimeBeforeRequest" : "${ycommerce:encodeJSON(component.waitTimeBeforeRequest)}",
                     "displayProductImages" : "${ycommerce:encodeJSON(component.displayProductImages)}"
                     }
                  </c:set>
                  <input type="text" id="js-site-search-input"  class="d-none d-md-inline-block form-control js-site-search-input" placeholder="<spring:theme code="text.rental.desktop.search.placeholder"/>"
                     name="text" value="${searchedWord}" maxlength="100" data-options="${fn:escapeXml(optionsJson)}">
                    <input type = "hidden" value="rentalGear" name="blPageType" id="js-page-type"/>
                  <input type="text" id="js-site-search-input-mob" class="d-inline-block d-md-none form-control js-site-search-input-mobile" placeholder="<spring:theme code="text.rental.search.placeholder"/>"
                  	 name="text" value="${searchedWord}" maxlength="100" data-options="${fn:escapeXml(optionsJson)}">
                  <span id="litepicker" class="rental-dates d-none d-md-inline"><i class="icon-calendar"></i><spring:theme code="text.rental.dates.placeholder"/> :</span>
                  <input type="text" id="litepicker_search" class="form-control d-none d-md-inline-block" placeholder="<spring:theme code="text.rental.select.dates.placeholder"/>" autocomplete="off">
                <span id="resetCalendar" class="close resetCalendar">&times;</span>
                  <input type="text" id="mobile-litepicker" class="form-control d-inline-block d-md-none" placeholder="<spring:theme code="text.rental.date.placeholder"/>" autocomplete="off">
                  </ycommerce:testId>
                  <div class="input-group-append d-md-block input-button-search">
                     <button class="btn btn-search btn-link js_search_button js-search-track" type="submit"><spring:theme code="text.rental.search"/></button>
                  </div>
               </div>
            </form>
         </div>
      </div>
   </div>
</section>
</div>