<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>

<%@ taglib prefix="cms" tagdir="/WEB-INF/tags/responsive/template/cms" %>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template" %>

<template:javaScriptVariables/>

<c:set var="commonResourcePathHtml" value="${fn:escapeXml(commonResourcePath)}"/>
<c:choose>
	<c:when test="${wro4jEnabled}">
	  	<script src="${fn:escapeXml(contextPath)}/wro/all_responsive.js"></script>
	  	<script src="${fn:escapeXml(contextPath)}/wro/addons_responsive.js"></script>
	</c:when>
	<c:otherwise>
		<%-- jquery --%>
		<script src="${commonResourcePathHtml}/js/jquery-3.5.1.min.js"></script>
		
		<%-- plugins --%>
		<script src="${commonResourcePathHtml}/js/enquire.min.js"></script>
		<script src="${commonResourcePathHtml}/js/Imager.min.js"></script>
		<script src="${commonResourcePathHtml}/js/purify.min.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.blockUI-2.66.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.colorbox-min.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.form.min.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.hoverIntent.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.pstrength.custom-1.2.0.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.syncheight.custom.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.tabs.custom.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery-ui-1.12.1.min.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.zoom.custom.js"></script>
		<script src="${commonResourcePathHtml}/js/owl.carousel.custom.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.tmpl-1.0.0pre.min.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.currencies.min.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.waitforimages.min.js"></script>
		<script src="${commonResourcePathHtml}/js/jquery.slideviewer.custom.1.2.js"></script>
		
		<%-- Custom ACC JS --%>
    <script src="${commonResourcePathHtml}/js/acc.address.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.autocomplete.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.carousel.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.cart.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.cartitem.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.checkout.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.checkoutsteps.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.cms.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.colorbox.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.common.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.forgottenpassword.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.global.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.hopdebug.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.imagegallery.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.langcurrencyselector.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.minicart.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.navigation.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.order.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.paginationsort.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.payment.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.paymentDetails.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.pickupinstore.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.product.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.productDetail.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.quickview.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.ratingstars.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.refinements.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.sanitizer.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.silentorderpost.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.tabs.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.termsandconditions.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.track.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.storefinder.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.futurelink.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.productorderform.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.savedcarts.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.multidgrid.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.quickorder.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.quote.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.consent.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.cookienotification.js"></script>
		<script src="${commonResourcePathHtml}/js/acc.closeaccount.js"></script>

		<script src="${commonResourcePathHtml}/js/acc.csv-import.js"></script>

		<script src="${commonResourcePathHtml}/js/_autoload.js"></script>

        <%-- custom js file --%>
        <c:if test="${cmsPage.uid eq 'cartpage'}">
        <script src="${commonResourcePathHtml}/js/blCustom.js"></script>
        </c:if>

		<%-- Cms Action JavaScript files --%>
		<c:forEach items="${cmsActionsJsFiles}" var="actionJsFile">
		    <script src="${commonResourcePathHtml}/js/cms/${fn:escapeXml(actionJsFile)}"></script>
		</c:forEach>
		
		<%-- AddOn JavaScript files --%>
		<c:forEach items="${addOnJavaScriptPaths}" var="addOnJavaScript">
		    <script src="${fn:escapeXml(addOnJavaScript)}"></script>
		</c:forEach>
		
		<script src="${commonResourcePathHtml}/js/bootstrap.bundle.min.js"></script>
		<script src="${commonResourcePathHtml}/js/mmenu.js"></script>
		<script src="https://cdn.jsdelivr.net/npm/litepicker/dist/litepicker.js"></script>
		<script src="https://cdn.jsdelivr.net/npm/litepicker/dist/plugins/mobilefriendly.js"></script>
		<script src="${commonResourcePathHtml}/js/splide.min.js"></script>
		<script src="${commonResourcePathHtml}/js/mmenu-light.js"></script>
    		<script src="${commonResourcePathHtml}/js/mburger.js"></script>

		<c:if test="${cmsPage.uid eq 'homepage'}">
		<script>
        document.addEventListener(
            "DOMContentLoaded", () => {
                 new Mmenu( "#my-menu", {
                    extensions: ["fullscreen","position-front"],
                    navbars		: [{
                        position: "top",
                        content : [ "close", "logo" ]
                    }],          
                } ); 
            }
        );
        $('.menu-large').hover(
               function(){ $('.screen').addClass('show') },
               function(){ $('.screen').removeClass('show') }
        );
         var splide = new Splide( '#hero-slider', {
            perPage: 1,
            type: 'fade',
            gap: 0,
        } ).mount();
        new Splide( '#cat-slider', {
            perPage: 4,
            breakpoints: {
                //'991': {
                //    perPage: 3,
                //},
                '640': {
                    perPage: 3,
                },
                '480': {
                    perPage: 2,  
                },
            },
            rewind : true,
            gap: 30,
        } ).mount();
         new Splide( '#gear-slider', {
            perPage: 3,
            breakpoints: {
                '991': {
                    perPage: 2,
                },
                '640': {
                    perPage: 1,
                },
            },
            rewind : true,
            gap: 20,
            padding: 10,
        } ).mount();  
        document.querySelectorAll('.card-slider').forEach(carousel => new Splide( carousel, {
            type   : 'loop',
            perPage: 1,
            drag   : false,
            breakpoints: {
                '991': {
                    pagination: false,
                },
            },
            //,
          } ).mount());
        document.querySelectorAll('.logo-slider').forEach(carousel => new Splide( carousel, {
            type   : 'loop',
            perPage: 3,
            gap: 20,
            //drag   : true,
        } ).mount());
         new Splide( '#testimonials-slider', {
            perPage: 1,
            type: 'fade',
            arrows: false,
        } ).mount(); 
        new Splide( '#blog-slider', {
            perPage: 3,
            breakpoints: {
                '991': {
                    perPage: 2,
                },
                '640': {
                    perPage: 1,
                },
            },
            rewind : true,
            gap: 20,
            padding: 10,
        } ).mount();
     // Initialize Calendar Litepicker - required for ANY page with the Calendar picker
         const picker = new Litepicker({ 
            element: document.getElementById('litepicker'),
            singleMode: false,
            numberOfMonths: 2,
            numberOfColumns: 2,
            autoApply: false,
            format: "MMM D, YYYY",
            resetButton: true,
            buttonText : {"reset":"Reset Dates"},
        }); 
         // Initialize MOBILE Calendar Litepicker - required for ANY page with the MOBILE Calendar picker
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
         // Added code to remove same name and id on search text box specific to device
         if ($(window).width() < 480 ) {
 			$("input.d-desk").attr("id","");
 		    $("input.d-desk").attr("name","");
 		}
 		else {
 			$("input.d-mob").attr("id","");
 			$("input.d-mob").attr("name","");
 		}
    </script>
	</c:if>


	<c:if test="${cmsPage.uid eq 'productGrid' || cmsPage.uid eq 'search' || cmsPage.uid eq 'searchEmpty'}">
  	  <script>
          // Mobile Menu styles - #my-menu is required for ALL pages
          // The second menu #filter-menu is required for ANY page with filtering - it is the mobile filter menu
          document.addEventListener(
              "DOMContentLoaded", () => {
                   new Mmenu( "#my-menu", {
                      extensions: ["fullscreen","position-front"],
                      navbars		: [{
                          position: "top",
                          content : [ "close", "logo" ]
                      }],
                  } );
                  new Mmenu( "#filter-menu", {
                      extensions: ["position-front","fullscreen"],
                      navbar: {
                        title: "Filters",
                      },
                      navbars		: [{
                          position: "top",
                          content : [ "close", "logo" ],
                      }],
                  } );
              }
          );
          // Initialize Mega menu rollover - required for ALL pages
          $('.menu-large').hover(
              function(){ $('.screen').addClass('show') },
              function(){ $('.screen').removeClass('show') }
          );
          // Initialize the "Sort By" dropdown menu above the product grid
          $(".dropdown-menu li a").click(function(){
            $("#sortProducts").html($(this).text()+' <span class="caret"></span>');
          });
          // Initialize Product Thumbnail Slider for Product Cards - required for ANY page with Thumbnail slider in Product card
          document.querySelectorAll('.card-slider').forEach(carousel => new Splide( carousel, {
              type   : 'loop',
              perPage: 1,
              drag   : false,
              breakpoints: {
                  '991': {
                      pagination: false,
                  },
              },
              //,
          } ).mount());

      </script>
  	</c:if>

  	<!-- This js is used for rental search box component-->
  	<c:if test="${fn:containsIgnoreCase(blPageType, 'rentalgear')}">
  	<script type="text/javascript">

            if ($(window).width() < 400 ) {
                $("input#litepicker").attr("placeholder","Dates...");
            }
            else { $("input#litepicker").attr("placeholder","Select Rental Dates...");}
            const picker = new Litepicker({
                element: document.getElementById('litepicker'),
                //plugins: ['mobilefriendly'],
                singleMode: false,
                numberOfMonths: 2,
                numberOfColumns: 2,
                autoApply: false,
                format: "MMM D, YYYY",
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

  <!-- This js will load on rental PDP  and it is required for all rental pdp component to make it work -->
	<c:if test="${cmsPage.uid eq 'productDetails' && IsRentalPage eq 'true' && product.forRent eq 'true'}">
                                		 <script>
                                        // Mobile Menu styles - #my-menu is required for ALL pages
                                             document.addEventListener(
                                                 "DOMContentLoaded", () => {
                                              new Mmenu( "#my-menu", {
                                                                 extensions: ["fullscreen","position-front"],
                                                                 navbars		: [{
                                                                     position: "top",
                                                                     content : [ "close", "logo" ]
                                                                 }],
                                                             } );
                                                 }
                                             );
                                             // Initialize Mega menu rollover - required for ALL pages
                                             $('.menu-large').hover(
                                                 function(){ $('.screen').addClass('show') },
                                                 function(){ $('.screen').removeClass('show') }
                                             );
                                             // Create and mount the product thumbnail slider - Required for Single Product Page
                                             var secondarySlider = new Splide( '#product-thumbnails', {
                                                 rewind      : true,
                                                 fixedWidth  : 115,
                                                 fixedHeight : 115,
                                                 isNavigation: true,
                                                 gap         : 10,
                                                 focus       : 'center',
                                                 pagination  : false,
                                                 cover       : true,
                                                 breakpoints : {
                                                     '600': {
                                                         fixedWidth  : 80,
                                                         fixedHeight : 80,
                                                         arrows: false,
                                                     }
                                                 },
                                                 keyboard: false,
                                             } ).mount();
                                             // Create the product slider - Required for Single Product Page
                                             var primarySlider = new Splide( '#product-slider', {
                                                 type       : 'fade',
                                                 pagination : false,
                                                 arrows     : false,
                                                 keyboard: false,
                                             } );
                                             // Set the thumbnails slider as a sync target and then call mount - Required for Single Product Page
                                             primarySlider.sync( secondarySlider ).mount();
                                             // Initialize Overview Video Slider - required for Single Product Page
                                             new Splide( '#overview-slider', {
                                                 perPage: 2,
                                                 breakpoints: {
                                                     '767': {
                                                         perPage: 1,
                                                         arrows: false,
                                                     },
                                                 },
                                                 gap: 30,
                                                 keyboard: false,
                                             } ).mount();
                                             // Initialize Additional Gear Slider - required for Single Product Page
                                             new Splide( '#gear-slider', {
                                                 perPage: 4,
                                                 breakpoints: {
                                                     '991': {
                                                         perPage: 3,
                                                     },
                                                     '767': {
                                                         perPage: 2,
                                                         arrows: false,
                                                     },
                                                     '640': {
                                                         perPage: 1,
                                                         arrows: false,
                                                     },
                                                 },
                                                 rewind : true,
                                                 gap: 20,
                                                 padding: 10,
                                                 keyboard: false,
                                             } ).mount();
                                             // Initialize Calendar Litepicker - required for ANY page with the Calendar picker

                                             // Initialize PRODUCT Calendar Litepicker - required for ANY page with the PRODUCT Calendar picker
                                             const prodpicker = new Litepicker({
                                                 element: document.getElementById('product-litepicker'),
                                                 singleMode: false,
                                                 numberOfMonths: 2,
                                                 numberOfColumns: 2,
                                                 autoApply: false,
                                                 format: "MMM D, YYYY",
                                                 resetButton: true,
                                                 buttonText : {"reset":"Reset Dates"},
                                             });

                                             // Initialize MOBILE PRODUCT Calendar Litepicker - required for ANY page with the PRODUCT Calendar picker
                                             const mprodpicker = new Litepicker({
                                                 element: document.getElementById('mobile-product-litepicker'),
                                                 plugins: ['mobilefriendly'],
                                                 singleMode: false,
                                                 numberOfMonths: 1,
                                                 numberOfColumns: 1,
                                                 autoApply: false,
                                                 format: "MMM D",
                                                 resetButton: true,
                                                 buttonText : {"reset":"Reset"},
                                             });
                                         // Initialize Product Thumbnail Slider for Product Cards - required for ANY page with Thumbnail slider in Product card
                                                 document.querySelectorAll('.card-slider').forEach(carousel => new Splide( carousel, {
                                                     type   : 'loop',
                                                     perPage: 1,
                                                     drag   : false,
                                                     breakpoints: {
                                                         '991': {
                                                             pagination: false,
                                                         },
                                                     },
                                                     keyboard: false,
                                                 } ).mount());

                                         </script>

                                		</c:if>

<!-- This js will load on usedGear PDP  and it is required for all usedGear pdp component to make it work -->
<c:if test="${cmsPage.uid eq 'productDetails' && IsRentalPage eq 'false' && product.forSale eq 'true'}">
		 <script>
		  // Expand Used Gear Items
          var swap = document.getElementById("showmore");
           $(swap).click(function (event) {
            event.preventDefault();
              $('.hide-product-row').toggle('slow');
                if(swap.innerHTML === "Show More") {
                   swap.innerHTML = "Show Less";
                    } else {
                  swap.innerHTML = "Show More";
                   }
                });
             // Mobile Menu styles - #my-menu is required for ALL pages
             document.addEventListener(
                 "DOMContentLoaded", () => {
                    new Mmenu( "#my-menu", {
                    extensions: ["fullscreen","position-front"],
                    navbars		: [{
                        position: "top",
                        content : [ "close", "logo" ]
                    }],
                } );
                 }
             );
             // Initialize Mega menu rollover - required for ALL pages
             $('.menu-large').hover(
                 function(){ $('.screen').addClass('show') },
                 function(){ $('.screen').removeClass('show') }
             );
             // Create and mount the product thumbnail slider - Required for Single Product Page
             var secondarySlider = new Splide( '#product-thumbnails', {
                 rewind      : true,
                 fixedWidth  : 115,
                 fixedHeight : 115,
                 isNavigation: true,
                 gap         : 10,
                 focus       : 'center',
                 pagination  : false,
                 cover       : true,
                 breakpoints : {
                     '600': {
                         fixedWidth  : 80,
                         fixedHeight : 80,
                         arrows: false,
                     }
                 },
                 keyboard: false,
             } ).mount();
             // Create the product slider - Required for Single Product Page
             var primarySlider = new Splide( '#product-slider', {
                 type       : 'fade',
                 pagination : false,
                 arrows     : false,
                 keyboard: false,
             } );
             // Set the thumbnails slider as a sync target and then call mount - Required for Single Product Page
             primarySlider.sync( secondarySlider ).mount();
             // Initialize Overview Video Slider - required for Single Product Page
             new Splide( '#overview-slider', {
                 perPage: 2,
                 breakpoints: {
                     '767': {
                         perPage: 1,
                         arrows: false,
                     },
                 },
                 gap: 30,
                 keyboard: false,
             } ).mount();
             // Initialize Additional Gear Slider - required for Single Product Page
             new Splide( '#gear-slider', {
                 perPage: 4,
                 breakpoints: {
                     '991': {
                         perPage: 3,
                     },
                     '767': {
                         perPage: 2,
                         arrows: false,
                     },
                     '640': {
                         perPage: 1,
                         arrows: false,
                     },
                 },
                 rewind : true,
                 gap: 20,
                 padding: 10,
                 keyboard: false,
             } ).mount();
             // Initialize Calendar Litepicker - required for ANY page with the Calendar picker

// Initialize Product Thumbnail Slider for Product Cards - required for ANY page with Thumbnail slider in Product card
        document.querySelectorAll('.card-slider').forEach(carousel => new Splide( carousel, {
            type   : 'loop',
            perPage: 1,
            drag   : false,
            breakpoints: {
                '991': {
                    pagination: false,
                },
            },
            keyboard: false,
        } ).mount());

         </script>

		</c:if>

		<%-- BL-457 added JS for rental cart page --%>
		<c:if test="${cmsPage.uid eq 'cartpage'}">
            <script>
                // Mobile Menu styles - #my-menu is required for ALL pages
                document.addEventListener(
                    "DOMContentLoaded", () => {
                        new Mmenu( "#my-menu", {
                            extensions: ["fullscreen","position-front"],
                            navbars		: [{
                                position: "top",
                                content : [ "close", "logo" ]
                            }],
                        } );
                    }
                );
                // Initialize Mega menu rollover - required for ALL pages
                $('.menu-large').hover(
                    function(){ $('.screen').addClass('show') },
                    function(){ $('.screen').removeClass('show') }
                );
                // Initialize Calendar Litepicker - required for ANY page with the Calendar picker
                const picker = new Litepicker({
                    element: document.getElementById('litepicker'),
                    plugins: ['mobilefriendly'],
                    singleMode: false,
                    numberOfMonths: 2,
                    numberOfColumns: 2,
                    autoApply: false,
                    format: "MMM D",
                    resetButton: true,
                    buttonText : {"reset":"Reset Dates"},
                });
                // Initialize Calendar Litepicker - required for ANY page with the Calendar picker
                const summarypicker = new Litepicker({
                    element: document.getElementById('summary-litepicker'),
                    plugins: ['mobilefriendly'],
                    singleMode: false,
                    numberOfMonths: 2,
                    numberOfColumns: 2,
                    autoApply: false,
                    format: "MMM D",
                    resetButton: true,
                    buttonText : {"reset":"Reset Dates"},
                });
            </script>
        </c:if>

        <%-- BL-466 added JS for empty cart page --%>
        <c:if test="${cmsPage.uid eq 'emptyCartPage'}">
              <script>
                      // Mobile Menu styles - #my-menu is required for ALL pages
                      document.addEventListener(
                          "DOMContentLoaded", () => {
                              new Mmenu( "#my-menu", {
                                  extensions: ["fullscreen","position-front"],
                                  navbars		: [{
                                      position: "top",
                                      content : [ "close", "logo" ]
                                  }],
                              } );
                          }
                      );
                      // Initialize Mega menu rollover - required for ALL pages
                      $('.menu-large').hover(
                          function(){ $('.screen').addClass('show') },
                          function(){ $('.screen').removeClass('show') }
                      );

                      // Initialize Featured Slider - required for ANY page with Featured Gear slider
                      new Splide( '#gear-slider', {
                          perPage: 3,
                          breakpoints: {
                              '991': {
                                  perPage: 2,
                              },
                              '640': {
                                  perPage: 1,
                              },
                          },
                          rewind : true,
                          gap: 20,
                          padding: 10,
                          keyboard: false,
                      } ).mount();
                      // Initialize Product Thumbnail Slider for Product Cards - required for ANY page with Thumbnail slider in Product card
                      document.querySelectorAll('.card-slider').forEach(carousel => new Splide( carousel, {
                          type   : 'loop',
                          perPage: 1,
                          drag   : false,
                          breakpoints: {
                              '991': {
                                  pagination: false,
                              },
                          },
                          keyboard: false,
                      } ).mount());
                  </script>
        </c:if>
	</c:otherwise>
</c:choose>


<cms:previewJS cmsPageRequestContextData="${cmsPageRequestContextData}" />
