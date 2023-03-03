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
		<script src="https://ajax.aspnetcdn.com/ajax/jquery.validate/1.11.1/jquery.validate.min.js"></script>
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

		<%-- added custom js bl-custom.js for gift card --%>
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
		<script src="${commonResourcePathHtml}/js/acc.account.js"></script>
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
    <script src="${commonResourcePathHtml}/js/acc.wishlist.js"></script>
    <script src="${commonResourcePathHtml}/js/acc.blanalyticsevent.js"></script>

		<script src="${commonResourcePathHtml}/js/acc.csv-import.js"></script>

		<script src="${commonResourcePathHtml}/js/_autoload.js"></script>

		<%-- custom js file --%>
        <script src="${commonResourcePathHtml}/js/blcustom.js"></script>
        <script src="${commonResourcePathHtml}/js/acc.privacyconsent.js"></script>


		<%-- Cms Action JavaScript files --%>
		<c:forEach items="${cmsActionsJsFiles}" var="actionJsFile">
		    <script src="${commonResourcePathHtml}/js/cms/${fn:escapeXml(actionJsFile)}"></script>
		</c:forEach>
		
		<%-- AddOn JavaScript files --%>
		<c:forEach items="${addOnJavaScriptPaths}" var="addOnJavaScript">
		    <script src="${fn:escapeXml(addOnJavaScript)}"></script>
		</c:forEach>
		
		<script src="${commonResourcePathHtml}/js/bootstrap.bundle.min.js"></script>
        <script src="${commonResourcePathHtml}/js/select-script.js"></script>
		<script src="${commonResourcePathHtml}/js/mmenu.js"></script>
		<script src="https://cdn.jsdelivr.net/npm/litepicker/dist/litepicker.js"></script>
		<script src="https://cdn.jsdelivr.net/npm/litepicker/dist/plugins/mobilefriendly.js"></script>
		<script src="${commonResourcePathHtml}/js/splide.min.js"></script>
		<script src="${commonResourcePathHtml}/js/mmenu-light.js"></script>
    	<script src="${commonResourcePathHtml}/js/mburger.js"></script>

		<input type="hidden" id="enableSaturdays" value="${enableSaturdays}">
		<script>
		
			$(document).ready(function(){

				 if('${rentalDate.selectedFromDate}' != '' && '${rentalDate.selectedToDate}' != '')
				 {
					 $("#litepicker").val('');
					 $("#litepicker").attr('placeholder','${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}');
					 $("#mobile-litepicker").val('');
					 $("#mobile-litepicker").attr('placeholder','${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}');
					 $("#product-litepicker").val('');
					 $("#product-litepicker").attr('placeholder','${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}');
					 $("#mobile-product-litepicker").val('');
					 $("#mobile-product-litepicker").attr('placeholder','${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}');
					 $("#summary-litepicker").val('');
					 $("#summary-litepicker").attr('placeholder','${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}');
				 }
				 
				 $('#saved-payment-action-payBill').on('click','li',function(e){
					 e.preventDefault();
						var paymentId = $(this).find("button").data("id");
						var paymentnonce = $(this).find("button").data("nonce");
					    var buttonData = $(this).find("button").html();
					    $("#savedCards").html(buttonData);
					 	$("#paymentId").val(paymentId);
						$("#paymentNonce").val(paymentnonce);
				 });
				 
				 var mediaQuery = window.matchMedia('(min-width: 768px)');
				 if(mediaQuery.matches)
				 {
					 $("#js-site-search-input-mob").val("");	 
				 }
				 else
				 {
					 $("#js-site-search-input").val("");
				 }
			});
		</script>
		
        <c:if test="${cmsPage.uid eq 'DeliveryOrPickupCartpage'}">
            <script src="${commonResourcePathHtml}/js/blcustomshipping.js"></script>
        </c:if>

		<c:if test="${cmsPage.uid eq 'homepage'}">

		<script>
		$('.social').eq(1).remove();
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
			 type: 'loop',
			 autoplay: true,
        } ).mount();

        //  BL-450 adding removing pagination and arrow dynamically on homepage
        document.addEventListener( 'DOMContentLoaded', function () {
        new Splide( '#cat-slider', {
            perPage: 4,
             arrows :false,
            pagination:false,
            breakpoints: {
                //'991': {
                //    perPage: 3,
                //},
                '640': {
                    perPage: 3,
                },
               /* BL-536 - A.1. */
            },
            rewind : true,
            gap: 30,
            keyboard: false,
        } ).mount();
         
            
           var cat_slider_image_qty = document.getElementById("cat-slider-list").getElementsByTagName("li").length;
           
            if(cat_slider_image_qty<=4){
                 new Splide( '#cat-slider', {
                perPage: 4,
                arrows :false,
                pagination:false,
                breakpoints: {
                '540': {
                    perPage: 3, 
                  pagination:true
                },
              }
               } ).mount();

            }

         if(cat_slider_image_qty>4){
       
          new Splide( '#cat-slider', {
            perPage: 4,
            arrows :true,
            pagination:true,
            breakpoints: {
                '991': {
                    perPage: 4,
                },
                '640': {
                    
                    perPage: 4,
                   
                },
                '540': {
                    perPage: 3,
                     
                    
                },
            },
            rewind : true,
            gap: 30,
           } ).mount();
         }

        if(cat_slider_image_qty<=3 && screen.width<=540){
           document.querySelector("#cat-slider .splide__pagination").classList.add("d-none");
         }
         }); 
 // code end here for BL-450 --end--
        document.addEventListener( 'DOMContentLoaded', function () {
         new Splide( '#gear-slider', {
            perPage: 3,
            breakpoints: {
                '991': {
                    perPage: 2,
                    gap: 10,
                    padding: 0
                },
                /* BL-536 - A.2. */
            },
            rewind : true,
            gap: 20,
            padding: 10,
            keyboard: false,
        } ).mount();  
        });
        document.addEventListener( 'DOMContentLoaded', function () {
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
         }); 
        
        
       /*  document.addEventListener( 'DOMContentLoaded', function () {  
        document.querySelectorAll('.logo-slider').forEach(carousel => new Splide( carousel, {
            type   : 'loop',
            perPage: 3,
            gap: 20,
            //drag   : true,
            keyboard: false,
        } ).mount());
        });  */
        
        //handled brands for mobile
        document.addEventListener( 'DOMContentLoaded', function () {
            new Splide( '#logo-slider', {
                perPage: 4,
                 arrows :false,
                pagination:false,
                breakpoints: {
                    //'991': {
                    //    perPage: 3,
                    //},
                    '640': {
                        perPage: 3,
                    },
                   /* BL-536 - A.1. */
                },
                rewind : true,
                gap: 20,
                keyboard: false,
            } ).mount();
             
                
               var cat_slider_image_qty = document.getElementById("logo-slider-list").getElementsByTagName("li").length;
               
                if(cat_slider_image_qty<=4){
                     new Splide( '#logo-slider', {
                    perPage: 5,
                    arrows :false,
                    pagination:false,
                    breakpoints: {
                    '540': {
                        perPage: 3, 
                      pagination:true
                    },
                  }
                   } ).mount();

                }

             if(cat_slider_image_qty>4){
           
              new Splide( '#logo-slider', {
                perPage: 5,
                arrows :true,
                pagination:true,
                breakpoints: {
                    '991': {
                        perPage: 3,
                    },
                    '640': {
                        
                        perPage: 3,
                       
                    },
                    '540': {
                        perPage: 3,
                         
                        
                    },
                },
                rewind : true,
                gap: 22,
               } ).mount();
             }

            if(cat_slider_image_qty<=3 && screen.width<=540){
               document.querySelector("#logo-slider .splide__pagination").classList.add("d-none");
             }
             }); 
        
        
        
        
        
        
        document.addEventListener( 'DOMContentLoaded', function () {
         new Splide( '#testimonials-slider', {
            perPage: 1,
            type: 'fade',
            arrows: false,
            keyboard: false,
        } ).mount(); 
        }); 
        document.addEventListener( 'DOMContentLoaded', function () {
        new Splide( '#blog-slider', {
            perPage: 3,
            breakpoints: {
                '991': {
                    perPage: 2,
                },
               /* BL-536 - A. 3 */
            },
            rewind : true,
            gap: 20,
            padding: 10,
            keyboard: false,
        } ).mount();
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
          //BL-678 changes added fixedheight
          document.querySelectorAll('.card-slider').forEach(carousel => new Splide( carousel, {
              type   : 'loop',
              perPage: 1,
              drag   : false,
              fixedHeight :275,
              breakpoints: {
                  '1025': {
                      fixedHeight:200,
                  },
                  '991': {
                      pagination: false,
                  },
              },
              keyboard: false,
          } ).mount());
      </script>
  	</c:if>

  	<!-- This js is used for rental search box component-->
  	<c:if test="${fn:containsIgnoreCase(blPageType, 'rentalgear') || isRentalPage eq true || cmsPage.uid eq 'howItworkPage' || cmsPage.uid eq 'contactUsPage' || cmsPage.uid eq 'shipOrPickupPage' || cmsPage.uid eq 'productDetails'}">
  	<script type="text/javascript">

            if ($(window).width() < 400 ) {
                $("input#litepicker").attr("placeholder","Dates...");
            }
            else { $("input#litepicker").attr("placeholder","Select dates...");}
            //BL-520 - disable previous dates
             let date = new Date();
             let dd = String(date.getDate() - 1).padStart(2, '0');
             let mm = String(date.getMonth() + 1).padStart(2, '0'); //January is 0!
             let yyyy = date.getFullYear();
             let today = mm + '/' + dd + '/' + yyyy;

             //BL-520 - disable dates after one year from today's date
                 let oneYearFromNow = new Date();
                 let disableDatesOneYearFomNow = oneYearFromNow.setFullYear(oneYearFromNow.getFullYear() + 1);
			     const disallowedDates = [['2001-01-01', today], '2023-01-16', '2023-02-20','2023-05-29', '2023-07-04', '2023-09-04', '2023-11-23', '2023-11-24', '2023-12-25', '2023-12-26', '2023-12-29', '2024-01-01'];
            const picker = new Litepicker({
                element: document.getElementById('litepicker'),
                //plugins: ['mobilefriendly'],
                singleMode: false,
                numberOfMonths: 2,
                numberOfColumns: 2,
                autoApply: false,
                format: "MMM D, YYYY",
                resetButton: () => {
  					 let btn = document.createElement('button');
  					 btn.innerText = 'Reset Dates';
  					 btn.className = 'reset-button';
  					 btn.addEventListener('click', (evt) => {
  					 evt.preventDefault();
  					 $.ajax({
                        url: ACC.config.encodedContextPath + '/resetDatepicker',
                        type: "GET",
                        success: function (data) {
                        	if(data=='success')
                            window.location.reload();
                        },
                        error: function (xhr, textStatus, error) {
                           
                        }
                    });
  					});
  					return btn;
  					},
  					tooltipNumber: (totalDays) => {
              return totalDays - 1;
            },
                setup: (picker) => {
        			picker.on('button:apply', (date1, date2) => {
        				var searchText = document.getElementById('js-site-search-input').value;
        				trackDateSelection(date1,date2);
          				var rentalGear = 'rentalGear';
          				var contextPath = ACC.config.contextPath;
          				$.ajax({
      	                    url: ACC.config.encodedContextPath + '/datepicker',
      	                    data: {selectedFromDate: date1.toDateString(), selectedToDate: date2.toDateString()},
      	                    type: "GET",
      	                    success: function (data) {
      	                    	if(searchText == '' && data=='success'){
      	                    		window.location.reload();
      	                    	}
      	                    	else{
      	                    		document.location.href=contextPath+"/search/?text="+searchText+"&blPageType="+rentalGear;
      	                    	}
      	                    },
      	                    error: function (xhr, textStatus, error) {
      	                       
      	                    }
      	                }); 
        			
        			});
        			},
        			 //BL-520 - disable weekends in the calendar
                         lockDaysFilter: (day) => {
                             const d = day.getDay();
                             if($("#enableSaturdays").val() === 'true'){
                            	 return [0].includes(d);
                             }
                             return [6, 0].includes(d);
                            },
                         lockDays: disallowedDates,
                      //Limit days selection to 91 days
                         maxDays: 91,
                         minDays: 2,
                      //Disable dates after one year from today
                         maxDate: disableDatesOneYearFomNow,
                      //Set Sunday to be the first day in the calendar's header
                         firstDay: 0,
                     //Change the defaul button values
                          buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
            });
            const mpicker = new Litepicker({
                element: document.getElementById('mobile-litepicker'),
                plugins: ['mobilefriendly'],
                singleMode: false,
                numberOfMonths: 1,
                numberOfColumns: 1,
                autoApply: false,
                format: "MMM D, YYYY",
                resetButton: () => {
    				 let btn = document.createElement('button');
    				 btn.innerText = 'Reset';
    				 btn.className = 'reset-button';
    				 btn.addEventListener('click', (evt) => {
    				 evt.preventDefault();
    				 $.ajax({
                        url: ACC.config.encodedContextPath + '/resetDatepicker',
                        type: "GET",
                        success: function (data) {
                        	if(data=='success')
                            window.location.reload();
                        },
                        error: function (xhr, textStatus, error) {
                           
                        }
                    });
    				});
    				return btn;
    				},
             tooltipNumber: (totalDays) => {
              return totalDays - 1;
            },

                setup: (picker) => {
          			picker.on('button:apply', (date1, date2) => {
          				var searchText = document.getElementById('js-site-search-input-mob').value;
          				trackDateSelection(date1,date2);
          				var rentalGear = 'rentalGear';
          				var contextPath = ACC.config.contextPath;
          				$.ajax({
      	                    url: ACC.config.encodedContextPath + '/datepicker',
      	                    data: {selectedFromDate: date1.toDateString(), selectedToDate: date2.toDateString()},
      	                    type: "GET",
      	                    success: function (data) {
      	                    	if(searchText == '' && data=='success'){
      	                    		window.location.reload();
      	                    	}
      	                    	else{
      	                    		document.location.href=contextPath+"/search/?text="+searchText+"&blPageType="+rentalGear;
      	                    	}
      	                    },
                        error: function (xhr, textStatus, error) {
                           
                        }
                    });
          			});
          			},
          			 //BL-520 - disable weekends in the calendar
                        lockDaysFilter: (day) => {
                              const d = day.getDay();
                              if($("#enableSaturdays").val() === 'true'){
                             	 return [0].includes(d);
                              }
                               return [6, 0].includes(d);
                            },
                         lockDays: disallowedDates,
                      //Limit days selection to 91 days
                         maxDays: 91,
                         minDays: 2,
                      //Disable dates after one year from today
                         maxDate: disableDatesOneYearFomNow,
                      //Set Sunday to be the first day in the calendar's header
                         firstDay: 0,
                      //Change the defaul button values
                         buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
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

                                             //     BL-574:code edited for product-thumbnail start here

                                             var secondarySlider = new Splide( '#product-thumbnails', {
                                                 rewind      : true,
                                                 fixedWidth  : 115,
                                                 fixedHeight : 115,
                                                 isNavigation: true,
                                                 gap         : 10,
                                                 pagination  : false,
                                                 cover       : true,
                                                 arrows      : false,
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
                                                 arrows     : true,
                                                 keyboard   : false,
                                                 fixedHeight  : 380,
                                             } );

                                             // Set the thumbnails slider as a sync target and then call mount - Required for Single Product Page
                                             primarySlider.sync( secondarySlider ).mount();

                                              var image_qty =   document.getElementById("product-thumbnails-list").getElementsByTagName("li").length;
                                             
                                             
                                               if(image_qty>4)
                                               {
                                                
                                             var secondarySlider = new Splide( '#product-thumbnails', {
                                                 rewind      : true,
                                                 fixedWidth  : 115,
                                                 fixedHeight : 115,
                                                 isNavigation: true,
                                                 gap         : 10,
                                                 focus       : 'center',
                                                 pagination  : false,
                                                 cover       : true,
                                                 arrows      : true,
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
                                                 arrows     : true,
                                                 keyboard   : false,
                                                 fixedHeight : 380,
                                             } );
                                             // Set the thumbnails slider as a sync target and then call mount - Required for Single Product Page
                                             primarySlider.sync( secondarySlider ).mount();
                                               }
                                              // BL:574 code ends here

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
                                             // BL-605 BL-682 :changes starts here

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

                                             let DontForgetCardQty= document.querySelectorAll("#gear-slider .card").length;
                                             if (DontForgetCardQty!=0){
                                             if(DontForgetCardQty<=4 && screen.width>991){
                                                 document.querySelector("#gear-slider .splide__arrows").style.display="none";
                                                  document.querySelector("#gear-slider .splide__pagination").style.display="none";
                                             }
                                              if(DontForgetCardQty<=3 && screen.width<=991 && screen.width>767){
                                                 document.querySelector("#gear-slider .splide__arrows").style.display="none";
                                                  document.querySelector("#gear-slider .splide__pagination").style.display="none";
                                             }
                                              if(DontForgetCardQty<=2 && screen.width<=767 && screen.width>640){
                                                 document.querySelector("#gear-slider .splide__arrows").style.display="none";
                                                  document.querySelector("#gear-slider .splide__pagination").style.display="none";
                                             }
                                              if(DontForgetCardQty==1 && screen.width<640){
                                                  document.querySelector("#gear-slider .splide__pagination").style.display="none";
                                             }
                                             }
                                             // BL-605: changes end here
                                             
                                             // Initialize Calendar Litepicker - required for ANY page with the Calendar picker

                                             // Initialize PRODUCT Calendar Litepicker - required for ANY page with the PRODUCT Calendar picker
                                             const prodpicker = new Litepicker({
                                                 element: document.getElementById('product-litepicker'),
                                                 singleMode: false,
                                                 numberOfMonths: 2,
                                                 numberOfColumns: 2,
                                                 autoApply: false,
                                                 format: "MMM D",
                                                 resetButton: () => {
												 let btn = document.createElement('button');
												 btn.innerText = 'Reset Dates';
												 btn.className = 'reset-button';
												 btn.addEventListener('click', (evt) => {
												 evt.preventDefault();
												 $.ajax({
                                                     url: ACC.config.encodedContextPath + '/resetDatepicker',
                                                     type: "GET",
                                                     success: function (data) {
                                                     	if(data=='success')
                                                         window.location.reload();
                                                     },
                                                     error: function (xhr, textStatus, error) {
                                                        
                                                     }
                                                 });
												});
												return btn;
												},
                         tooltipNumber: (totalDays) => {
                          return totalDays - 1;
                        },
                                                 setup: (picker) => {
                                           			picker.on('button:apply', (date1, date2) => {
                                           			trackDateSelection(date1,date2);
                                          				$.ajax({
                                      	                    url: ACC.config.encodedContextPath + '/datepicker',
                                      	                    data: {selectedFromDate: date1.toDateString(), selectedToDate: date2.toDateString()},
                                      	                    type: "GET",
                                      	                    success: function (data) {
                                      	                    	window.location.reload();
                                      	                    },
                                      	                    error: function (xhr, textStatus, error) {
                                      	                       
                                      	                    }
                                      	                }); 
                                           			
                                           			});
                                           			},
                                           			 tooltipNumber: (totalDays) => {
                                                              return totalDays - 1;
                                                            },

                                           	//BL-520 - disable weekends in the calendar
                                                    lockDaysFilter: (day) => {
                                                         const d = day.getDay();
                                                         if($("#enableSaturdays").val() === 'true'){
                                                        	 return [0].includes(d);
                                                         }
                                                         return [6,0].includes(d);
                                                       },
                                                    lockDays: disallowedDates,
                                            //Limit days selection to 91 days
                                                    maxDays: 91,
                                                    minDays: 2,
                                            //Disable dates after one year from today
                                                    maxDate: disableDatesOneYearFomNow,
                                            //Set Sunday to be the first day in the calendar's header
                                                    firstDay: 0,
                                            //Change the defaul button values
                                                    buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
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
                                                 resetButton: () => {
                                     				 let btn = document.createElement('button');
                                     				 btn.innerText = 'Reset';
                                     				 btn.className = 'reset-button';
                                     				 btn.addEventListener('click', (evt) => {
                                     				 evt.preventDefault();
                                     				 $.ajax({
                                                         url: ACC.config.encodedContextPath + '/resetDatepicker',
                                                         type: "GET",
                                                         success: function (data) {
                                                         	if(data=='success')
                                                             window.location.reload();
                                                         },
                                                         error: function (xhr, textStatus, error) {
                                                            
                                                         }
                                                     });
                                     				});
                                     				return btn;
                                     				},
                                             tooltipNumber: (totalDays) => {
                                              return totalDays - 1;
                                            },
                                                 setup: (picker) => {
                                           			picker.on('button:apply', (date1, date2) => {
                                           			trackDateSelection(date1,date2);
                                           			$.ajax({
                                                         url: ACC.config.encodedContextPath + '/datepicker',
                                                         data: {selectedFromDate: date1.toDateString(), selectedToDate: date2.toDateString()},
                                                         type: "GET",
                                                         success: function (data) {
                                                         	if(data=='success')
                                                             window.location.reload();
                                                         },
                                                         error: function (xhr, textStatus, error) {
                                                            
                                                         }
                                                     });
                                           			});
                                           			},
                                           //BL-520 - disable weekends in the calendar
                                                    lockDaysFilter: (day) => {
                                                         const d = day.getDay();
                                                         if($("#enableSaturdays").val() === 'true'){
                                                        	 return [0].includes(d);
                                                         }
                                                         return [6, 0].includes(d);
                                                       },
                                                    lockDays: disallowedDates,
                                          //Limit days selection to 91 days
                                                    maxDays: 91,
                                                    minDays: 2,
                                          //Disable dates after one year from today
                                                    maxDate: disableDatesOneYearFomNow,
                                          //Set Sunday to be the first day in the calendar's header
                                                    firstDay: 0,
                                          //Change the defaul button values
                                                    buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
                                             });
                                         // Initialize Product Thumbnail Slider for Product Cards - required for ANY page with Thumbnail slider in Product card
                                         // BL-605 : fixedHeight added
                                                 document.querySelectorAll('.card-slider').forEach(carousel => new Splide( carousel, {
                                                     type   : 'loop',
                                                     perPage: 1,
                                                     fixedHeight:200,
                                                     drag   : false,
                                                     breakpoints: {
                                                         '1025': {
                                                              fixedHeight:150,
                                                         },
                                                         '991': {
                                                             pagination: false,
                                                         },
                                                         '640': {
                                                             pagination: false,
                                                             fixedHeight: 300,
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
              //BL-573 and BL-572 : added + icon and html code for + and _
                if(swap.innerHTML === "Show More +") {
                   swap.innerHTML = "Show Less &#8722;";
                    } else {
                  swap.innerHTML = "Show More &#43;";
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
           // BL-574 : product thumbnail center code start here

             var secondarySlider = new Splide( '#product-thumbnails', {
                 rewind      : true,
                 fixedWidth  : 115,
                 fixedHeight : 115,
                 isNavigation: true,
                 gap         : 10,
                 pagination  : false,
                 cover       : true,
                 arrows      : false,
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
                 arrows     : true,
                 keyboard   : false,
                 fixedHeight : 380,
             } );
             // Set the thumbnails slider as a sync target and then call mount - Required for Single Product Page
             primarySlider.sync( secondarySlider ).mount();

              var image_qty =   document.getElementById("product-thumbnails-list").getElementsByTagName("li").length;

              if(image_qty>4){
                         var secondarySlider = new Splide( '#product-thumbnails', {
                                                 rewind      : true,
                                                 fixedWidth  : 115,
                                                 fixedHeight : 115,
                                                 isNavigation: true,
                                                 gap         : 10,
                                                 focus       : 'center',
                                                 pagination  : false,
                                                 cover       : true,
                                                 arrows      : true,
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
                                                 arrows     : true,
                                                 keyboard   : false,
                                                 fixedHeight : 380,
                                             } );
                                             primarySlider.sync( secondarySlider ).mount();
                                              }
         // BL:574 code ends here part2

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

<!-- This js will load on new gear PDP  and it is required for all new gear pdp component to make it work -->
<c:if test="${cmsPage.uid eq 'productDetails' && IsRentalPage eq 'false' && product.retailGear eq true}">
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
           // BL-574 : product thumbnail center code start here

             var secondarySlider = new Splide( '#product-thumbnails', {
                 rewind      : true,
                 fixedWidth  : 115,
                 fixedHeight : 115,
                 isNavigation: true,
                 gap         : 10,
                 pagination  : false,
                 cover       : true,
                 arrows      : false,
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
                 arrows     : true,
                 keyboard   : false,
                 fixedHeight : 380,
             } );
             // Set the thumbnails slider as a sync target and then call mount - Required for Single Product Page
             primarySlider.sync( secondarySlider ).mount();

              var image_qty =   document.getElementById("product-thumbnails-list").getElementsByTagName("li").length;

              if(image_qty>4){
                         var secondarySlider = new Splide( '#product-thumbnails', {
                                                 rewind      : true,
                                                 fixedWidth  : 115,
                                                 fixedHeight : 115,
                                                 isNavigation: true,
                                                 gap         : 10,
                                                 focus       : 'center',
                                                 pagination  : false,
                                                 cover       : true,
                                                 arrows      : true,
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
                                                 arrows     : true,
                                                 keyboard   : false,
                                                 fixedHeight : 380,
                                             } );
                                             primarySlider.sync( secondarySlider ).mount();
                                              }
         // BL:574 code ends here part2
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

        <c:if test="${cmsPage.uid eq 'DeliveryOrPickupCartpage'}">
         <input type="hidden" id="isFromSummaryPage" value="true">
            <script>
                //Replace button text
                $(".dropdown-menu li button").click(function(){
                  $(this).parents(".dropdown").find('.btn').html($(this).text() + ' <span class="caret"></span>');
                  $(this).parents(".dropdown").find('.btn').val($(this).data('value'));
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

                $(document).ready(function() {
                    $("#shippingChangeRentalDate").click(function(e) {
                    	e.preventDefault();
                    	var rentalStartDate = $("#rentalStartDate").val();
                    	var rentalEndDate = $("#rentalEndDate").val();
                    	$.ajax({
                        url: ACC.config.encodedContextPath + '/datepicker',
                        data: {selectedFromDate: rentalStartDate, selectedToDate: rentalEndDate},
                        type: "GET",
                        success: function (data) {
                            if(data=='success')
                            window.location.href = ACC.config.encodedContextPath + '/cart';
                        },
                        error: function (xhr, textStatus, error) {

                        }
                    }); 
                    });
                    
                    $("#shippingCloseModal").click(function(e) {
                    	resetDateValues(e);
                    });
                    
                    $("#shippingCloseIconModal").click(function(e) {
                    	resetDateValues(e);
                    });
                }); 
                
                function resetDateValues(e)
                {
                	e.preventDefault();
                	$("#litepicker").val('');
                	$("#summary-litepicker").val('');
                	$("#rentalStartDate").val("");
                	$("#rentalEndDate").val("");
                	$('#editWarning').modal('hide');
                }
            </script>
        </c:if>
          <c:if test="${fn:containsIgnoreCase(blPageType, 'rentalSummary')}">
        		<script>
                 // Initialize Calendar Litepicker - required for ANY page with the Calendar picker
                        //BL-520 - disable previous dates
                           let date = new Date();
                           let dd = String(date.getDate() - 1).padStart(2, '0');
                           let mm = String(date.getMonth() + 1).padStart(2, '0'); //January is 0!
                           let yyyy = date.getFullYear();
                           let today = mm + '/' + dd + '/' + yyyy;

                        //BL-520 - disable dates after one year from today's date
                           let oneYearFromNow = new Date();
                           let disableDatesOneYearFomNow = oneYearFromNow.setFullYear(oneYearFromNow.getFullYear() + 1);
				           const disallowedDates = [['2001-01-01', today], '2023-01-16', '2023-02-20','2023-05-29', '2023-07-04', '2023-09-04', '2023-11-23', '2023-11-24', '2023-12-25', '2023-12-26', '2023-12-29', '2024-01-01'];
				 const picker = new Litepicker({
                            element: document.getElementById('litepicker'),
                            plugins: ['mobilefriendly'],
                            singleMode: false,
                            numberOfMonths: 2,
                            numberOfColumns: 2,
                            autoApply: false,
                            format: "MMM D, YYYY",
                            resetButton: () => {
                				 let btn = document.createElement('button');
                				 btn.innerText = 'Reset Dates';
                				 btn.className = 'reset-button';
                				 btn.addEventListener('click', (evt) => {
                				 evt.preventDefault();
                				 $.ajax({
                                    url: ACC.config.encodedContextPath + '/resetDatepicker',
                                    type: "GET",
                                    success: function (data) {
                                    	if(data=='success')
                                        window.location.reload();
                                    },
                                    error: function (xhr, textStatus, error) {

                                    }
                                });
                				});
                				return btn;
                				},
                				     tooltipNumber: (totalDays) => {
                                return totalDays - 1;
                              },
                            setup: (picker) => {
                      			picker.on('button:apply', (date1, date2) => {
                                  //var isFromSummaryPage = $("#isFromSummaryPage").val();
                                  //alert(isFromSummaryPage);
                                  if($("#isFromSummaryPage").val() === 'true'){
                      			      $("#rentalStartDate").val(date1.toDateString());
                                      $("#rentalEndDate").val(date2.toDateString());
                                      $('#editWarning').modal('show');
                                   }else{
                               //    trackDateSelection(date1,date2);
                      				$.ajax({
                  	                    url: ACC.config.encodedContextPath + '/datepicker',
                  	                    data: {selectedFromDate: date1.toDateString(), selectedToDate: date2.toDateString()},
                  	                    type: "GET",
                  	                    success: function (data) {
                  	                    	window.location.reload();
                  	                    },
                  	                    error: function (xhr, textStatus, error) {

                  	                    }
                  	                });
        }
                      			});
                      			},
                      		 //BL-520 - disable weekends in the calendar
                                lockDaysFilter: (day) => {
                                    const d = day.getDay();
                                    if($("#enableSaturdays").val() === 'true'){
                                   	 return [0].includes(d);
                                    }
                                    return [6, 0].includes(d);
                                  },
                                lockDays: disallowedDates,
                             //Limit days selection to 91 days
                                maxDays: 91,
                                minDays: 2,
                            //Disable dates after one year from today
                                maxDate: disableDatesOneYearFomNow,
                           //Set Sunday to be the first day in the calendar's header
                                firstDay: 0,
                           //Change the defaul button values
                                buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
                        });
                        // Initialize Calendar Litepicker - required for ANY page with the Calendar picker
                        const summarypicker = new Litepicker({
                            element: document.getElementById('summary-litepicker'),
                            plugins: ['mobilefriendly'],
                            singleMode: false,
                            numberOfMonths: 2,
                            numberOfColumns: 2,
                            autoApply: false,
                            format: "MMM D, YYYY",
                            resetButton: () => {
                				 let btn = document.createElement('button');
                				 btn.innerText = 'Reset Dates';
                				 btn.className = 'reset-button';
                				 btn.addEventListener('click', (evt) => {
                				 evt.preventDefault();
                				 $.ajax({
                                    url: ACC.config.encodedContextPath + '/resetDatepicker',
                                    type: "GET",
                                    success: function (data) {
                                    	if(data=='success')
                                        window.location.reload();
                                    },
                                    error: function (xhr, textStatus, error) {

                                    }
                                });
                				});
                				return btn;
                				},
                         tooltipNumber: (totalDays) => {
                          return totalDays - 1;
                        },
                            setup: (picker) => {
                      			picker.on('button:apply', (date1, date2) => {
                      			// var isFromSummaryPage = $("#isFromSummaryPage").val();
                                    if($("#isFromSummaryPage").val() === 'true'){
                      			      $("#rentalStartDate").val(date1.toDateString());
                                      $("#rentalEndDate").val(date2.toDateString());
                                      $('#editWarning').modal('show');
                                      }else{
                                //      trackDateSelection(date1,date2);
                      			$.ajax({
                                    url: ACC.config.encodedContextPath + '/datepicker',
                                    data: {selectedFromDate: date1.toDateString(), selectedToDate: date2.toDateString()},
                                    type: "GET",
                                    success: function (data) {
                                    	if(data=='success')
                                        window.location.reload();
                                    },
                                    error: function (xhr, textStatus, error) {

                                    }
                                });
                                }
                      			});
                      			},
                      //BL-520 - disable weekends in the calendar
                               lockDaysFilter: (day) => {
                                       const d = day.getDay();
                                       if($("#enableSaturdays").val() === 'true'){
                                      	 return [0].includes(d);
                                       }
                                       return [6, 0].includes(d);
                                     },
                               lockDays: disallowedDates,
                      //Limit days selection to 91 days
                              maxDays: 91,
                              minDays: 2,
                      //Disable dates after one year from today
                              maxDate: disableDatesOneYearFomNow,
                      //Set Sunday to be the first day in the calendar's header
                              firstDay: 0,
                      //Change the defaul button values
                              buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
                        });
                  </script>
        		</c:if>
				<c:if test="${cmsPage.uid eq 'multiStepCheckoutSummaryPage' and fn:containsIgnoreCase(currentPage, 'Review') == false}">
        		<input type="hidden" id="isFromPaymentPage" value="true">
        			<script>
        			$(document).ready(function() {
                        $("#shippingChangeRentalDate").click(function(e) {
                        	e.preventDefault();
                        	$('.page-loader-new-layout').show();
                        	var rentalStartDate = $("#rentalStartDate").val();
                        	var rentalEndDate = $("#rentalEndDate").val();
                        	$.ajax({
                            url: ACC.config.encodedContextPath + '/datepicker',
                            data: {selectedFromDate: rentalStartDate, selectedToDate: rentalEndDate},
                            type: "GET",
                            success: function (data) {
                                if(data=='success')
                                window.location.href = ACC.config.encodedContextPath + '/cart';
                            },
                            error: function (xhr, textStatus, error) {

                            }
                        }); 
                        });
                        
                        $("#shippingCloseModal").click(function(e) {
                        	resetDateValues(e);
                        });
                        
                        $("#shippingCloseIconModal").click(function(e) {
                        	resetDateValues(e);
                        });
                    }); 
                    
                    function resetDateValues(e)
                    {
                    	e.preventDefault();
                    	$("#litepicker").val('');
                    	$("#summary-litepicker").val('');
                    	$("#rentalStartDate").val("");
                    	$("#rentalEndDate").val("");
                    	$('#editWarning').modal('hide');
                    }
                 // Initialize Calendar Litepicker - required for ANY page with the Calendar picker
                        //BL-520 - disable previous dates
                           let date = new Date();
                           let dd = String(date.getDate() - 1).padStart(2, '0');
                           let mm = String(date.getMonth() + 1).padStart(2, '0'); //January is 0!
                           let yyyy = date.getFullYear();
                           let today = mm + '/' + dd + '/' + yyyy;

                        //BL-520 - disable dates after one year from today's date
                           let oneYearFromNow = new Date();
                           let disableDatesOneYearFomNow = oneYearFromNow.setFullYear(oneYearFromNow.getFullYear() + 1);
					       const disallowedDates = [['2001-01-01', today], '2023-01-16', '2023-02-20','2023-05-29', '2023-07-04', '2023-09-04', '2023-11-23', '2023-11-24', '2023-12-25', '2023-12-26', '2023-12-29', '2024-01-01'];
                           </script>
                           </c:if>
                           <c:if test="${enableDatePicker == true and cmsPage.uid eq 'multiStepCheckoutSummaryPage' and fn:containsIgnoreCase(currentPage, 'Review') == false}">
        					
        			<script>
                        const picker = new Litepicker({
                            element: document.getElementById('litepicker'),
                            plugins: ['mobilefriendly'],
                            singleMode: false,
                            numberOfMonths: 2,
                            numberOfColumns: 2,
                            autoApply: false,
                            format: "MMM D, YYYY",
                            resetButton: () => {
                				 let btn = document.createElement('button');
                				 btn.innerText = 'Reset Dates';
                				 btn.className = 'reset-button';
                				 btn.addEventListener('click', (evt) => {
                				 evt.preventDefault();
                				 $.ajax({
                                    url: ACC.config.encodedContextPath + '/resetDatepicker',
                                    type: "GET",
                                    success: function (data) {
                                    	if(data=='success')
                                        window.location.reload();
                                    },
                                    error: function (xhr, textStatus, error) {

                                    }
                                });
                				});
                				return btn;
                				},
                         tooltipNumber: (totalDays) => {
                          return totalDays - 1;
                        },
                            setup: (picker) => {
                      			picker.on('button:apply', (date1, date2) => {
                                  //var isFromSummaryPage = $("#isFromSummaryPage").val();
                                  //alert(isFromSummaryPage);
                                  if($("#isFromPaymentPage").val() === 'true'){
                      			      $("#rentalStartDate").val(date1.toDateString());
                                      $("#rentalEndDate").val(date2.toDateString());
                                      $('#editWarning').modal('show');
                                   }else{
                                 //  trackDateSelection(date1,date2);
                      				$.ajax({
                  	                    url: ACC.config.encodedContextPath + '/datepicker',
                  	                    data: {selectedFromDate: date1.toDateString(), selectedToDate: date2.toDateString()},
                  	                    type: "GET",
                  	                    success: function (data) {
                  	                    	window.location.reload();
                  	                    },
                  	                    error: function (xhr, textStatus, error) {

                  	                    }
                  	                });
        }
                      			});
                      			},
                      		 //BL-520 - disable weekends in the calendar
                                lockDaysFilter: (day) => {
                                    const d = day.getDay();
                                    if($("#enableSaturdays").val() === 'true'){
                                   	 return [0].includes(d);
                                    }
                                    return [6, 0].includes(d);
                                  },
                                lockDays: disallowedDates,
                             //Limit days selection to 91 days
                                maxDays: 91,
                                minDays: 2,
                            //Disable dates after one year from today
                                maxDate: disableDatesOneYearFomNow,
                           //Set Sunday to be the first day in the calendar's header
                                firstDay: 0,
                           //Change the defaul button values
                                buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
                        });
                        // Initialize Calendar Litepicker - required for ANY page with the Calendar picker
                        const summarypicker = new Litepicker({
                            element: document.getElementById('summary-litepicker'),
                            plugins: ['mobilefriendly'],
                            singleMode: false,
                            numberOfMonths: 2,
                            numberOfColumns: 2,
                            autoApply: false,
                            format: "MMM D, YYYY",
                            resetButton: () => {
                				 let btn = document.createElement('button');
                				 btn.innerText = 'Reset Dates';
                				 btn.className = 'reset-button';
                				 btn.addEventListener('click', (evt) => {
                				 evt.preventDefault();
                				 $.ajax({
                                    url: ACC.config.encodedContextPath + '/resetDatepicker',
                                    type: "GET",
                                    success: function (data) {
                                    	if(data=='success')
                                        window.location.reload();
                                    },
                                    error: function (xhr, textStatus, error) {

                                    }
                                });
                				});
                				return btn;
                				},
                				tooltipNumber: (totalDays) => {
                          return totalDays - 1;
                        },
                            setup: (picker) => {
                      			picker.on('button:apply', (date1, date2) => {
                      			// var isFromSummaryPage = $("#isFromSummaryPage").val();
                                    if($("#isFromPaymentPage").val() === 'true'){
                      			      $("#rentalStartDate").val(date1.toDateString());
                                      $("#rentalEndDate").val(date2.toDateString());
                                      $('#editWarning').modal('show');
                                      }else{
                                //      trackDateSelection(date1,date2);
                      			$.ajax({
                                    url: ACC.config.encodedContextPath + '/datepicker',
                                    data: {selectedFromDate: date1.toDateString(), selectedToDate: date2.toDateString()},
                                    type: "GET",
                                    success: function (data) {
                                    	if(data=='success')
                                        window.location.reload();
                                    },
                                    error: function (xhr, textStatus, error) {

                                    }
                                });
                                }
                      			});
                      			},
                      //BL-520 - disable weekends in the calendar
                               lockDaysFilter: (day) => {
                                       const d = day.getDay();
                                       if($("#enableSaturdays").val() === 'true'){
                                      	 return [0].includes(d);
                                       }
                                       return [6, 0].includes(d);
                                     },
                               lockDays: disallowedDates,
                      //Limit days selection to 91 days
                              maxDays: 91,
                              minDays: 2,
                      //Disable dates after one year from today
                              maxDate: disableDatesOneYearFomNow,
                      //Set Sunday to be the first day in the calendar's header
                              firstDay: 0,
                      //Change the defaul button values
                              buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
                        });
                  </script>
                  
        		</c:if>
        		
        		<!-- Order Confirmation Page -->
        		<c:if test="${cmsPage.uid eq 'orderConfirmation'}">
        			
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
        
        // Initialize Blog Slider - required for ANY page with the Blog slider
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
            keyboard: false,
        } ).mount();
    </script>
        		</c:if>
    	<c:if test="${cmsPage.uid eq 'multiStepCheckoutReviewPage' and fn:containsIgnoreCase(currentPage, 'review') == true}">
        	<input type="hidden" id="isFromReviewPage" value="true">
        		<script>
        			$(document).ready(function() {			
        				$(".reviewEdit").click(function(e) {
							e.preventDefault();
        					var sectionSelected = this.getAttribute("data-section");
        					var redirectUrl = this.getAttribute("data-redirect-url");        					
        					$("#urlToRedirect").val(redirectUrl);
        					$("#clickedSection").val(sectionSelected);
        				});
        				
                        $("#continueChanges").click(function(e) {
                        	e.preventDefault();
                        	if($("#clickedSection").val() == 'summaryRentalDate')
                        	{
                        		$('.page-loader-new-layout').show();
                            	var rentalStartDate = $("#rentalStartDate").val();
                            	var rentalEndDate = $("#rentalEndDate").val();
                            	$.ajax({
                                	url: ACC.config.encodedContextPath + '/datepicker',
                                	data: {selectedFromDate: rentalStartDate, selectedToDate: rentalEndDate},
                                	type: "GET",
                                	success: function (data) {
                                	    if(data=='success')
                                	    window.location.href = ACC.config.encodedContextPath + '/cart';
                                	},
                                	error: function (xhr, textStatus, error) {
	
	                                }
                            	});
                        	}
                        	else
                        	{
                        		window.location.href = $("#urlToRedirect").val();
                        	}
                        });
                        
                        $("#shippingCloseModal").click(function(e) {
                        	resetDateValues(e);
                        });
                        
                        $("#shippingCloseIconModal").click(function(e) {
                        	resetDateValues(e);
                        });
                    }); 
        			
                    function resetDateValues(e)
                    {
                    	e.preventDefault();
                    	$("#summary-litepicker").val('');
                    	$("#rentalStartDate").val("");
                    	$("#rentalEndDate").val("");
                    	$("#urlToRedirect").val("");
                    	$("#clickedSection").val("");
                    	$('#editWarning').modal('hide');
                    }
                 // Initialize Calendar Litepicker - required for ANY page with the Calendar picker
                 // BL-520 - disable previous dates
                    let date = new Date();
                    let dd = String(date.getDate() - 1).padStart(2, '0');
                    let mm = String(date.getMonth() + 1).padStart(2, '0'); //January is 0!
                    let yyyy = date.getFullYear();
                    let today = mm + '/' + dd + '/' + yyyy;

                 // BL-520 - disable dates after one year from today's date
                    let oneYearFromNow = new Date();
                    let disableDatesOneYearFomNow = oneYearFromNow.setFullYear(oneYearFromNow.getFullYear() + 1);
					const disallowedDates = [['2001-01-01', today], '2023-01-16', '2023-02-20','2023-05-29', '2023-07-04', '2023-09-04', '2023-11-23', '2023-11-24', '2023-12-25', '2023-12-26', '2023-12-29', '2024-01-01'];

                 // Initialize Calendar Litepicker - required for ANY page with the Calendar picker
                    const summarypicker = new Litepicker({
                        element: document.getElementById('summary-litepicker'),
                        plugins: ['mobilefriendly'],
                        singleMode: false,
                        numberOfMonths: 2,
                        numberOfColumns: 2,
                        autoApply: false,
                        format: "MMM D, YYYY",
                        resetButton: () => {
							let btn = document.createElement('button');
							btn.innerText = 'Reset Dates';
							btn.className = 'reset-button';
							btn.addEventListener('click', (evt) => {
								evt.preventDefault();
								$.ajax({
									url: ACC.config.encodedContextPath + '/resetDatepicker',
									type: "GET",
									success: function (data) {
										if(data=='success')
										window.location.reload();
									},
									error: function (xhr, textStatus, error) {
	
									}
								});
							});
							return btn;
						},
						 tooltipNumber: (totalDays) => {
              return totalDays - 1;
            },
                        setup: (picker) => {
                      		picker.on('button:apply', (date1, date2) => {
							 //	var isFromSummaryPage = $("#isFromSummaryPage").val();
								if($("#isFromReviewPage").val() === 'true')
								{
									$("#rentalStartDate").val(date1.toDateString());
									$("#rentalEndDate").val(date2.toDateString());
									$("#clickedSection").val('summaryRentalDate');
									$('#editWarning').modal('show');
								}
								else
								{
								//trackDateSelection(date1,date2);
									$.ajax({
										url: ACC.config.encodedContextPath + '/datepicker',
										data: {selectedFromDate: date1.toDateString(), selectedToDate: date2.toDateString()},
										type: "GET",
										success: function (data) {
											if(data=='success')
											window.location.reload();
										},
										error: function (xhr, textStatus, error) {
	
										}
									});
								}
							});
                      	},
                      //BL-520 - disable weekends in the calendar
                        lockDaysFilter: (day) => {
                            const d = day.getDay();
                            if($("#enableSaturdays").val() === 'true'){
                           	 return [0].includes(d);
                            }
                            return [6, 0].includes(d);
                        },
                        lockDays: disallowedDates,
                      //Limit days selection to 91 days
                        maxDays: 91,
                        minDays: 2,
                      //Disable dates after one year from today
                        maxDate: disableDatesOneYearFomNow,
                      //Set Sunday to be the first day in the calendar's header
                        firstDay: 0,
                      //Change the defaul button values
                        buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
                    });
                </script>
		</c:if>

	<c:if test="${cmsPage.uid eq 'extendRentalOrderDetails'}">
		<input type="hidden" id="rentalEndDate" value = "${orderData.rentalEndDateForJs}">
		<input type="hidden" id="rentalStartDate" value = "${orderData.rentalStartDateForJs}">
		<input type="hidden" id="orderCode" value = "${fn:escapeXml(orderData.code)}">
	<script>
          //Replace button text
          $(".dropdown-menu li button").click(function(){
            $(this).parents(".dropdown").find('.btn').html($(this).html() + ' <span class="caret"></span>');
            $(this).parents(".dropdown").find('.btn').val($(this).data('value'));
          });


          // Initialize RENTAL EXTENSION Calendar Litepicker - required for THIS page
           let endDate = $("#rentalEndDate").val(); // Existing Order End Date
           let rentalStartDate = $("#rentalStartDate").val();  // Existing Order start Date
           const startDate = new Date(rentalStartDate);
           var orderCode = $("#orderCode").val(); // To Get Order Code

           startDate.setDate(startDate.getDate() + 89); // To add max days from existing order startDate
		  const disallowedDatesForExendOrder = [['2001-01-01', endDate], '2023-01-16', '2023-02-20','2023-05-29', '2023-07-04', '2023-09-04', '2023-11-23', '2023-11-24', '2023-12-25', '2023-12-26', '2023-12-29', '2024-01-01'];
                                       const extendRentalDatePicker = new Litepicker({
                                      element: document.getElementById('rental-litepicker'),
                                  //    plugins: ['mobilefriendly'],
                                      singleMode: true,
                                      numberOfMonths: 2,
                                      numberOfColumns: 2,
                                      autoApply: false,
                                      format: "MMM D, YYYY",
                                      resetButton: () => {
                          				 let btn = document.createElement('button');
                          				 btn.innerText = 'Reset Dates';
                          				 btn.className = 'reset-button';
                          				 btn.addEventListener('click', (evt) => {
                          				 evt.preventDefault();
                                   window.location.reload();
                                  });
                          				return btn;
                          				},
                          				 tooltipNumber: (totalDays) => {
                                    return totalDays - 1;
                                  },
                                      setup: (extendRentalDatePicker) => {
                                			extendRentalDatePicker.on('button:apply', (newEndDate) => {
                                				$.ajax({
                            	                    url: ACC.config.encodedContextPath +'/my-account/extendDate/',
                            	                    data: {extendEndDate: newEndDate.toDateString() , orderCode : orderCode , orderEndDate:endDate},
                            	                    type: "GET",
                            	                    success: function (data) {
                            	                    $('#orderSummary').html(data);
                            	                    $('#js-totalCost-update').html( $('#js-totalExtendCost').html());
                            	                                      $('.js-extend-button-enable').attr('disabled',false);
                            	                    var dayOrDays = "";
                            	                    if(($('#js-totalExtendDays').val() == 1)) {
                            	                    dayOrDays = $('#js-totalExtendDays').val() + ' ' + 'Day';
                            	                    }
                            	                    else {
                            	                    dayOrDays = $('#js-totalExtendDays').val() + ' ' + 'Days';
                            	                    }
                            	                    $('#js-totaldays-update').html(dayOrDays);
                            	                    $('#js-totalDamegeWaiverCost-update').html( $('#js-totalDamageWaiver').html());
                            	                    if($('#js-isAllProductExtendabe').val() !== '') {
                            	                    if($("#add-error-message").hasClass("d-none")){
                                                                                    $("#add-error-message").removeClass("d-none");
                                                  }
                                                  }
                                                  if($('#js-isAllProductExtendabe').val() === '') {
                                                    $("#add-error-message").addClass("d-none");
                                                    }
                            	                    },
                            	                    error: function (xhr, textStatus, error) {

                            	                    }
                            	                });
                                			});
                                			},
                                          lockDaysFilter: (day) => {
                                              const d = day.getDay();
                                              if($("#enableSaturdays").val() === 'true'){
                                             	 return [0].includes(d);
                                              }
                                              return [6, 0].includes(d);
                                            },
                                          lockDays: disallowedDatesForExendOrder,
                                      //Disable dates after one year from today
                                          maxDate: startDate,
                                     //Change the defaul button values
                                          buttonText: {"apply":"Apply", cancel: "Cancel", "reset":"Reset Dates"}
                                  });

          // Initialize RENTAL EXTENSION MOBILE Calendar Litepicker - required for THIS page
          const rmpicker = new Litepicker({
              element: document.getElementById('rental-mobile-litepicker'),
              plugins: ['mobilefriendly'],
              singleMode: true,
              numberOfMonths: 1,
              numberOfColumns: 1,
              autoApply: false,
              format: "MMM D YYYY",
              resetButton: true,
              buttonText : {"reset":"Reset"},
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
      </script>
  </c:if>
  
  
  
  
  	<%-- Added JS for all myaccount pages & other pages --%>
		<c:if test="${cmsPage.uid eq 'orderConfirmation' || cmsPage.uid eq 'address-book' || cmsPage.uid eq 'orders' || cmsPage.uid eq 'saved-carts' ||
		               cmsPage.uid eq 'payment-details'   || cmsPage.uid eq 'update-email' || cmsPage.uid eq 'updatePassword' ||
		               cmsPage.uid eq 'verificationImages'   || cmsPage.uid eq 'cart-rental' ||  cmsPage.uid eq 'multiStepCheckoutSummaryPage' ||
		               cmsPage.uid eq 'multiStepCheckoutReviewPage' || cmsPage.uid eq 'order'}">
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

            </script>
        </c:if>
        
      

	</c:otherwise>
</c:choose>


<cms:previewJS cmsPageRequestContextData="${cmsPageRequestContextData}" />
