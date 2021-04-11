<%@ page trimDirectiveWhitespaces="true"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="search" tagdir="/WEB-INF/tags/responsive/search" %>

<c:choose>
	<!-- Using seprate tag file for search box due to difference in homepage html as compared to other pages -->
	<c:when test="${positionAttribute == 'HomePageBannerSearchBoxSlot' or positionAttribute == 'MobileHomePageBannerSearchBoxSlot'}">
		<search:blHomePageCarouselSearchBox/>
	</c:when>
	<c:otherwise>
		<%-- Added For Rentagear and Usedgear Page --%>
		<c:choose>
        <c:when test="${blPageType eq 'usedGear'}">
              <search:blUsedGearSearchBox/>
        </c:when>
        <c:otherwise>
              <search:blRentalGearSearchBox/>
        </c:otherwise>
</c:choose>
	</c:otherwise>
</c:choose>
<c:if test="${positionAttribute != 'HomePageBannerSearchBoxSlot' and positionAttribute != 'MobileHomePageBannerSearchBoxSlot'}">
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
</c:if>