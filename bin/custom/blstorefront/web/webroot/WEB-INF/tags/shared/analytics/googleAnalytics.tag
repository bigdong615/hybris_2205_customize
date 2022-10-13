<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<c:if test="${not empty googleAnalyticsTrackingId}">
<script async src="https://www.googletagmanager.com/gtag/js?id=${ycommerce:encodeJavaScript(googleAnalyticsTrackingId)}"></script>
<script>
/* Google Analytics */

var googleAnalyticsTrackingId = '${ycommerce:encodeJavaScript(googleAnalyticsTrackingId)}';

window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
gtag('config', googleAnalyticsTrackingId);

<c:choose>
	<c:when test="${pageType == 'PRODUCT'}">
		<c:set var="categories" value="" />
		<c:set var="blPageType" value="${IsRentalPage ? 'rental gear' : 'used gear'}"/>
		<c:forEach items="${product.categories}" var="category">
			<c:set var="categories">${ycommerce:encodeJavaScript(category.name)}</c:set>
		</c:forEach>

		<c:if test="${not empty product.stock.stockLevelStatus.code}">
    <c:set var="stockStatus" value="${fn:trim(product.stock.stockLevelStatus.code)}" />
		</c:if>
		<c:set var="stockStatus" value="${empty stockStatus ? 'Item In Stock' : 'Item Out of Stock'}" />

    gtag('event', 'view_item', {
       'event_category': 'Product View',
       'event_label' : '${product.code}',
      "items": [
        {
          "id": "${product.code}",
          "name": "${product.name}",
          "brand": "${product.manufacturer}",
          "category": "${categories}",
          "variant" : "${ycommerce:encodeJavaScript(blPageType)}",
          "stockStatus" : "${stockStatus}"
        }
      ]
    });
	</c:when>

	<c:when test="${pageType == 'CATEGORY' || pageType == 'PRODUCTSEARCH'}">
		 <c:set var="listName" value="${pageType == 'CATEGORY' ? 'List' : 'Search'}"/>
     <c:set var="variantName" value="${ blPageType == 'rentalGear' ? 'Rental gear' : 'Used gear'}"/>
     <c:set var="_href" value="${not empty header.referer ? header.referer : 'javascript:window.history.back()'}" />
     <c:set var="req" value="${pageContext.request}" />
     <c:set var="withDates" value="" />

        var datePickerText = $("#litepicker").attr('placeholder');
        var withDates = "viewWithDates";
        if(datePickerText.indexOf("Select dates...") > -1){
            withDates = "viewWithoutDates";
        }

		<c:choose>
			<c:when test="${searchPageData.pagination.totalNumberOfResults > 0}">
				<c:if test="${not empty searchPageData.results}">
                        <c:choose>
                            <c:when test="${pageType eq 'CATEGORY'}">
                                    gtag('event',withDates, {
                                        "event_category": "Category View",
                                        "event_label": window.location.pathname.split("category/")[1]
                                    });
                            </c:when>
                            <c:otherwise>
                                    gtag('event', withDates, {
                                        "event_category": "Category View",
                                        "event_label": (window.location.search.split("text=")[1]).split("&")[0]
                                    });
                            </c:otherwise>
                        </c:choose>

						gtag('event', 'view_item_list', {
							"event_category": "Category View",
							"event_label": "${ycommerce:encodeJavaScript(listName)}",
							"items": [
              				<c:forEach items='${searchPageData.results}' var='product' varStatus='status'>
              					{
                              "id":"${product.code}",
                              "name":"${product.displayName}",
                              "brand": "${product.manufacturer}",
                              <c:choose>
                              <c:when test="${not empty product.categories}">
                             	"category": "${ycommerce:encodeJavaScript(product.categories[fn:length(product.categories) - 1].name)}",
                            	</c:when>
                            	<c:otherwise>
                             	"category": "",
                              </c:otherwise>
                              </c:choose>
                               "list_position": ${status.index},
                               "variant" :"${ycommerce:encodeJavaScript(variantName)}"
                               <c:if test="${not empty rentalDate.selectedFromDate}">
                                  ,
                               "rentalDays" : "${rentalDate.numberOfDays}"
                               </c:if>
                       	}
                       	<c:if test='${not status.last}'>
                        ,
                        </c:if>
                       </c:forEach>
                 	]
						});
				</c:if>
			</c:when>
			<c:otherwise>
				gtag('config', googleAnalyticsTrackingId, { 'dimension2': '${ycommerce:encodeJavaScript(searchPageData.freeTextSearch)}' });
			</c:otherwise>
		</c:choose>

	</c:when>
  <c:when test="${pageType == 'CART' && !_href.contains('cart')}">
      <c:set var="couponCodes" value=""/>
      <c:forEach items='${cartData.appliedVouchers}' var='voucher' varStatus='status'>
        <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
      </c:forEach>
        if(window.document.referrer.indexOf('cart') == -1){
  	    	gtag('event', 'begin_checkout', {
        	    "event_category": "Cart Page",
            	"event_label": "View Cart",
            	"checkout_step" : 1,
            	"checkout_option": ${cartType},
            	"value": ${ycommerce:encodeJavaScript(cartData.totalPrice.value)},
              "items": [
        				<c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
        					{
        					  "id": "${ycommerce:encodeJavaScript(entry.product.code)}",
        					  "name": "${ycommerce:encodeJavaScript(entry.product.name)}",
                     "list_position": ${status.index},
        					  "quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
        					  "price": "${ycommerce:encodeJavaScript(entry.basePrice.value)}"
        					}
        					<c:if test='${not status.last}'>,</c:if>
        			  </c:forEach>
        			],
        			"coupon": "${ycommerce:encodeJavaScript(couponCodes)}"
        		});
        }
  	</c:when>

  	<c:when test="${pageType == 'shippingPage'}">
          <c:set var="couponCodes" value=""/>
          <c:forEach items='${cartData.appliedVouchers}' var='voucher' varStatus='status'>
            <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
          </c:forEach>
      		gtag('event', 'checkout_progress', {
                  	    "event_category": "Shipping Page",
                      	"event_label": "Delivery Method",
                        "checkout_step": 2,
                        //"checkout_option": "Delivery Method",
                       "value": ${ycommerce:encodeJavaScript(cartData.totalPrice.value)},
                        "items": [
                  				<c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
                  					{
                  					  "id": "${ycommerce:encodeJavaScript(entry.product.code)}",
                  					  "name": "${ycommerce:encodeJavaScript(entry.product.name)}",
                               "list_position": ${status.index},
                  					  "quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
                  					  "price": "${ycommerce:encodeJavaScript(entry.basePrice.value)}"
                  					}
                  					<c:if test='${not status.last}'>,</c:if>
                  			  </c:forEach>
                  			],
                  				"coupon": "${ycommerce:encodeJavaScript(couponCodes)}"
                  		});
      	</c:when>

      		<c:when test="${pageType == 'paymentPage'}">
                  <c:set var="couponCodes" value=""/>
                  <c:forEach items='${cartData.appliedVouchers}' var='voucher' varStatus='status'>
                    <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
                  </c:forEach>
               		gtag('event', 'checkout_progress', {
                          	    "event_category": "Payment Page",
                              	"event_label": "Payment Method",
                              	"checkout_step": 3,
                                //"checkout_option": "Payment Method",
                                "value": ${ycommerce:encodeJavaScript(cartData.totalPrice.value)},
                                "items": [
                          				<c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
                          					{
                          					  "id": "${ycommerce:encodeJavaScript(entry.product.code)}",
                          					  "name": "${ycommerce:encodeJavaScript(entry.product.name)}",
                                       "list_position": ${status.index},
                          					  "quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
                          					  "price": "${ycommerce:encodeJavaScript(entry.basePrice.value)}"
                          					}
                          					<c:if test='${not status.last}'>,</c:if>
                          			  </c:forEach>
                          			],
                          			"coupon": "${ycommerce:encodeJavaScript(couponCodes)}"
                          		});
              	</c:when>

              		<c:when test="${pageType == 'reviewSummaryPage'}">
                                  <c:set var="couponCodes" value=""/>
                                  <c:forEach items='${cartData.appliedVouchers}' var='voucher' varStatus='status'>
                                    <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
                                  </c:forEach>
           gtag('event', 'checkout_progress', {
                   	    "event_category": "Review Page",
                       	"event_label": "Review Order",
                       	"checkout_step": 4,
                       // "checkout_option": "Review Order",
                       	"value": ${ycommerce:encodeJavaScript(cartData.totalPrice.value)},
                        "items": [
                   				<c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
                   					{
                   					  "id": "${ycommerce:encodeJavaScript(entry.product.code)}",
                   					  "name": "${ycommerce:encodeJavaScript(entry.product.name)}",
                               "list_position": ${status.index},
                   					  "quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
                   					  "price": "${ycommerce:encodeJavaScript(entry.basePrice.value)}"
                   					}
                   					<c:if test='${not status.last}'>,</c:if>
                   			  </c:forEach>
                   			],
                   			 "coupon": "${ycommerce:encodeJavaScript(couponCodes)}"
                   		});
                     	</c:when>

	<c:when test="${pageType == 'ORDERCONFIRMATION'}">
		<c:set var="orderCode" value="${ycommerce:encodeJavaScript(orderData.code)}"/>
    <c:set var="couponCodes" value=""/>
    <c:forEach items='${orderData.appliedVouchers}' var='voucher' varStatus='status'>
      <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
    </c:forEach>
    <c:set var="cartType" value=""/>
                      <c:choose>
                      <c:when test="${orderData.hasGiftCart}">
                        <c:set var="cartType" value="Gift Cart Order"/>
                      </c:when>
                       <c:when test="${orderData.isRetailGearOrder}">
                         <c:set var="cartType" value="New Gear Order"/>
                       </c:when>
                       <c:when test="${orderData.isRentalCart}">
                         <c:set var="cartType" value="Rental Order"/>
                       </c:when>
                      <c:otherwise>
                        <c:set var="cartType" value="Used Gear Order"/>
                      </c:otherwise>
                      </c:choose>
		gtag('event', 'purchase', {
		  "event_category": "Order Confirmation",
    	"event_label": "Confirmed",
		  "transaction_id": "${orderCode}",
		  "affiliation": "${ycommerce:encodeJavaScript(siteName)}",
		  "value": ${ycommerce:encodeJavaScript(orderData.totalPrice.value)},
		  "currency": "USD",
		  "tax": ${ycommerce:encodeJavaScript(orderData.totalTax.value)},
		  "shipping": ${ycommerce:encodeJavaScript(orderData.deliveryCost.value)},
   	  "items": [
				<c:forEach items='${orderData.entries}' var='entry' varStatus='status'>
					{
					  "id": "${ycommerce:encodeJavaScript(entry.product.code)}",
					  "name": "${ycommerce:encodeJavaScript(entry.product.name)}",
					  "brand": "${ycommerce:encodeJavaScript(entry.product.manufacturer)}",
					  <c:choose>
						<c:when test="${not empty entry.product.categories}">
							"category": "${ycommerce:encodeJavaScript(entry.product.categories[fn:length(entry.product.categories) - 1].name)}",
						</c:when>
						<c:otherwise>
							"category": "",
						</c:otherwise>
					  </c:choose>
            "list_position": ${status.index},
					  "quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
					  "price": "${ycommerce:encodeJavaScript(entry.basePrice.value)}"
					   }
					<c:if test='${not status.last}'>
						,
					</c:if>
			  </c:forEach>
			]
		});
	</c:when>
	<c:otherwise>
	</c:otherwise>
</c:choose>

function trackAddToCart_google(productCode, productName,quantity,productBrand,productType,productCategory) {
	gtag('event', 'add_to_cart', {
	   "event_category": "Cart",
     "event_label": "Add to Cart",
	   "currency" : "USD",
	   "value": quantity,
	  "items": [
        {
          "id": productCode,
          "name":productName ,
          "brand": productBrand,
          "category":productCategory,
          "variant": productType
        }
      ]
	});
}

function trackRemoveFromCart(productCode,productName,initialQuantity) {
	gtag('event', 'remove_from_cart', {
	 "event_category": "Cart",
   "event_label": "Remove From Cart",
	  "currency" : "USD",
    "items": [
      {
        "id": productCode,
        "name":productName,
        "quantity": initialQuantity
      }
    ]
  });
}

window.mediator.subscribe('trackAddToCart', function(data) {
	if (data.productCode && data.quantity)
	{
		trackAddToCart_google(data.productCode, data.productName,data.quantity,data.productBrand,data.productType,data.productCategory);
	}
});

window.mediator.subscribe('trackRemoveFromCart', function(data) {
	if (data.productCode && data.initialCartQuantity)
	{
		trackRemoveFromCart(data.productCode,data.productName,data.initialCartQuantity);
	}
});

window.mediator.subscribe('productClick_gtm', function(data) {
	if (data.productCode && data.productName)
	{
		trackProductClick(data.productCode, data.productName,data.brand,data.productType);
	}
});
function trackProductClick(productCode, productName,brand,productType) {
	gtag('event', 'product_click', {
	  "event_category": "Product",
	  "event_label": productCode,
    "content_type": "product",
    "items": [
      {
        "id": productCode,
        "name": productName,
        "brand": brand,
        "variant": productType
      }
    ]
  });
}

window.mediator.subscribe('searchRentalDate', function(data) {
	if (data.lengthOfRental)
	{
		trackDatePickerClick(data.daysInAdvance,data.lengthOfRental);
	}
});

function trackDatePickerClick(daysInAdvance,lengthOfRental) {
    gtag('event', 'select_date', {
        'event_category': 'Search Rental Dates',
        'event_label': "Open"
    });
	gtag('event', 'select_date', {
      'event_category': 'Search Rental Dates',
      'event_label': "[#] Days in Advance",
      'value' : daysInAdvance
	});
    gtag('event', 'select_date', {
        'event_category': 'Search Rental Dates',
        'event_label': "[#] Length of Rental",
        'value' : lengthOfRental
    });
}

window.mediator.subscribe('trackSearch', function(data) {
		trackSearchClick(data.searchText);
});

function trackSearchClick(searchText) {
    gtag('event', 'select_search', {
        'event_category': 'Search Bar',
        'event_label': searchText
    });
	gtag('event', 'select_search', {
      'event_category': 'Search Bar',
      'event_label': 'Submit'
	});
}

window.mediator.subscribe('usedGearNavClick', function(data) {
    trackUsedGearNavClick(data.pageType);
});

function trackUsedGearNavClick(pageType){
    gtag('event', 'usedGearNavClick', {
        'event_category': 'Navigation',
        'event_label': pageType
    });
}


window.mediator.subscribe('loginClick', function(data) {
	if (data.userId)
	{
		trackLoginClick(data.userId,data.pageType);
	}
});

function trackLoginClick(userId,pageType) {
	gtag('event', 'loginTrack', {
	    'event_category': 'Login',
       'event_label': pageType,
	     'id': userId
	});
}

window.mediator.subscribe('registerClick', function(data) {
	if (data.userId)
	{
		trackRegisterClick(data.userId,data.pageType);
	}
});

function trackRegisterClick(userId,pageType) {
	gtag('event', 'registerClick', {
	 'event_category': 'Register',
   'event_label': pageType,
	  'id': userId
	});
}

window.mediator.subscribe('applyPromo', function(data) {
	if (data.voucherError)
	{
  trackPromoCLick(data.voucherError, data.pageType);
	}
});

 function trackPromoCLick(voucherError,pageType){
     var eventCategory = 'Cart';
     if(pageType == 'shippingPage'){
         eventCategory = 'Checkout [Delivery]';
     }else if(pageType == 'paymentPage'){
         eventCategory = 'Checkout [Billing]';
     }
   gtag('event', 'Add Promo - Error', {
     'event_label': 'Promo:' + voucherError,
     'event_category': eventCategory,
     'non_interaction': true
   });
 }

window.mediator.subscribe('applyCreditCart', function(data) {
	if (data.paymentError)
	{
	trackCreditCart(data.paymentError);
	}
});

 function trackCreditCart(paymentError){
    gtag('event', 'Card Error', {
    'event_label': paymentError,
    'event_category': 'Checkout [Billing]',
    'non_interaction': true
  });
 }

window.mediator.subscribe('applyPayPal', function(data) {
	if (data.paymentError)
	{
  trackPayPalClick(data.paymentError);
	}
});

function trackPayPalClick(paymentError) {
  gtag('event', 'PayPal Error', {
   'event_label': paymentError,
   'event_category': 'Checkout [Billing]',
   'non_interaction': true
  });
}

window.mediator.subscribe('applyPO', function(data) {
	if (data.paymentError)
	{
	 trackPOClick(data.paymentError);
	}
});

function trackPOClick(paymentError) {
   gtag('event', 'PO Error', {
   'event_label': 'Missing PO Details',
   'event_category': 'Checkout [Billing]',
   'non_interaction': true
 });
}

window.mediator.subscribe('changeDamageWaiver', function(data) {
	if (data.productCode && data.damageWaiverType)
	{
	 trackChangeDamageWaiverClick(data.productCode,data.damageWaiverType);
	}
});

function trackChangeDamageWaiverClick(productCode,damageWaiverType) {
   gtag('event', 'Change Damage Waiver', {
   'event_category': 'Cart',
   'event_label': damageWaiverType,
    'value' : productCode
 });
}

window.mediator.subscribe('continueShippingClick', function(data) {
	if (data.shippingType)
	{
	 trackContinueShippingClick(data.shippingType,data.shippingOption,data.stockStatus);
	}
});

function trackContinueShippingClick(shippingType,shippingOption,stockStatus){
  gtag('event', 'continueShippingClick', {
     'event_label': stockStatus,
     'event_category': shippingType.toString() + "-" + shippingOption.toString() ,
     'non_interaction': true
   });
}

window.mediator.subscribe('continuePaymentClick', function(data) {
	if (data.paymentType)
	{
	 trackContinuePaymentClick(data.paymentType);
	}
});

function trackContinuePaymentClick(paymentType){
  gtag('event', 'continuePaymentClick', {
     'event_label': paymentType,
     'event_category': 'checkoutPayment' ,
     'non_interaction': true
   });
}

window.mediator.subscribe('placeOrderClick', function(data) {
	if (data.reviewPageError)
	{
  trackPlaceOrderClick(data.reviewPageError);
	}
});

 function trackPlaceOrderClick(reviewPageError){
   gtag('event', 'Place Order Error', {
     'event_label': reviewPageError,
     'event_category': 'Checkout [Review Order]',
     'non_interaction': true
   });
 }


</script>
</c:if>