<%@ page trimDirectiveWhitespaces="true"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:url value="/search/" var="searchUrl" />
<spring:url value="/search/autocomplete/{/componentuid}" var="autocompleteUrl" htmlEscape="false">
     <spring:param name="componentuid"  value="${component.uid}"/>
</spring:url>


<section id="globalSearch">
   <div class="container">
      <div  class="row justify-content-center">
         <div class="col-12">
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
                  <input type="text" id="js-site-search-input"  class="d-none d-md-inline-block form-control js-site-search-input" placeholder="Search photo & Videos rentals..."
                     name="text" value="" maxlength="100" data-options="${fn:escapeXml(optionsJson)}">
                     </ycommerce:testId>
                  <input type="text" class="d-inline-block d-md-none form-control" placeholder="Search…">
                  <span class="rental-dates d-none d-md-inline"><i class="icon-calendar"></i> Rental Dates</span>
                  <input type="text" id="litepicker" class="form-control d-none d-md-inline-block" placeholder="Select rental dates…">
                  <input type="text" id="mobile-litepicker" class="form-control d-inline-block d-md-none" placeholder="Dates…">
                  <div class="input-group-append d-none d-md-block">
                     <button class="btn btn-search btn-link js_search_button" type="submit">Search</button>
                  </div>
               </div>
            </form>
         </div>
      </div>
   </div>
</section>


<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/litepicker/dist/litepicker.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/litepicker/dist/plugins/mobilefriendly.js"></script>
    <script type="text/javascript">
        $('.menu-large').hover(
            function(){ $('.screen').addClass('show') },
            function(){ $('.screen').removeClass('show') }
        );
        $(".dropdown-menu li a").click(function(){
          $("#sortProducts").html($(this).text()+' <span class="caret"></span>');
        });
        if ($(window).width() < 400 ) {
            $("input#litepicker").attr("placeholder","Dates...");
        }
        else { $("input#litepicker").attr("placeholder","Select Rental Dates...");}
        document.querySelectorAll('.card-slider').forEach(carousel => new Splide( carousel, {
            type   : 'loop',
            perPage: 1,
            //drag   : false,
        } ).mount());
        const picker = new Litepicker({
            element: document.getElementById('litepicker'),
            //plugins: ['mobilefriendly'],
            singleMode: false,
            numberOfMonths: 2,
            numberOfColumns: 2,
            autoApply: false,
            format: "MMM D",
            resetButton: true,
            buttonText : {"reset":"Reset Dates"},
        });
        const mpicker = new Litepicker({
            element: document.getElementById('mobile-litepicker'),
            plugins: ['mobilefriendly'],
            singleMode: false,
            numberOfMonths: 1,
            numberOfColumns: 1,
            autoApply: false,
            format: "MMM D",
            resetButton: true,
            buttonText : {"reset":"Reset"},
        });
    </script>