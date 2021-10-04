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
     <c:set var="variantName" value="${ blPageType == 'rentalgear' ? 'Rental gear' : 'Used gear'}"/>

		<c:choose>
			<c:when test="${searchPageData.pagination.totalNumberOfResults > 0}">
				<c:if test="${not empty searchPageData.results}">
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
                               "rentalDate" : "${rentalDate.numberOfDays}"
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

  <c:when test="${pageType == 'CART'}">
      <c:set var="couponCodes" value=""/>
      <c:forEach items='${cartData.appliedVouchers}' var='voucher' varStatus='status'>
        <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
      </c:forEach>
      <c:set var="cartType" value=""/>
      <c:choose>
      <c:when test="${cartData.hasGiftCart}">
        <c:set var="cartType" value="Gift Cart"/>
      </c:when>
      <c:when test="${cartData.isNewGearOrder}">
         <c:set var="cartType" value="New Gear Cart"/>
      </c:when>
      <c:when test="${cartData.isRentalCart}">
         <c:set var="cartType" value="Rental Cart"/>
      </c:when>
      <c:otherwise>
        <c:set var="cartType" value="Used Gear Cart"/>
      </c:otherwise>
      </c:choose>
  		gtag('event', 'cart', {
  		  "event_category": "Cart Page",
      	"event_label": "${cartType}",
  		  "affiliation": "${ycommerce:encodeJavaScript(siteName)}",
  		  "value": ${ycommerce:encodeJavaScript(cartData.totalPrice.value)},
  		  "currency": "USD",
  		  "coupon": "${ycommerce:encodeJavaScript(couponCodes)}",
       	"damageWaiver" : "${ycommerce:encodeJavaScript(cartData.totalDamageWaiverCost.value)}",
        "subtotal": "${ycommerce:encodeJavaScript(cartData.subTotal.value)}"
  		  "items": [
  				<c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
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
  					  "subtotal": "${ycommerce:encodeJavaScript(entry.totalPrice.value)}"
  					}
  					<c:if test='${not status.last}'>,</c:if>
  			  </c:forEach>
  			]
  		});
  	</c:when>

  	<c:when test="${pageType == 'shippingPage'}">
          <c:set var="couponCodes" value=""/>
          <c:forEach items='${cartData.appliedVouchers}' var='voucher' varStatus='status'>
            <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
          </c:forEach>
          <c:set var="cartType" value=""/>
          <c:choose>
          <c:when test="${cartData.hasGiftCart}">
            <c:set var="cartType" value="Gift Cart Order"/>
          </c:when>
          <c:when test="${cartData.isNewGearOrder}">
             <c:set var="cartType" value="New Gear Order"/>
          </c:when>
          <c:when test="${cartData.isRentalCart}">
             <c:set var="cartType" value="Rental Order"/>
          </c:when>
          <c:otherwise>
            <c:set var="cartType" value="Used Gear Order"/>
          </c:otherwise>
          </c:choose>
      		gtag('event', 'shipping', {
      		  "event_category": "Shipping Page",
          	"event_label": "${cartType}",
      		  "affiliation": "${ycommerce:encodeJavaScript(siteName)}",
      		  "value": ${ycommerce:encodeJavaScript(cartData.totalPrice.value)},
      		  "currency": "USD",
      		  "coupon": "${ycommerce:encodeJavaScript(couponCodes)}",
           	"damageWaiver" : "${ycommerce:encodeJavaScript(cartData.totalDamageWaiverCost.value)}",
            "subtotal": "${ycommerce:encodeJavaScript(cartData.subTotal.value)}",
            "deliveryCost" : "${ycommerce:encodeJavaScript(cartData.deliveryCost.value)}",
            "tax": "${ycommerce:encodeJavaScript(cartData.taxAvalaraCalculated.value)}"
      		  "items": [
      				<c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
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
      					  "price": "${ycommerce:encodeJavaScript(entry.basePrice.value)}",
      					  "subtotal": "${ycommerce:encodeJavaScript(entry.totalPrice.value)}"
      					}
      					<c:if test='${not status.last}'>,</c:if>
      		  	  </c:forEach>
      			]
      		});
      	</c:when>

      		<c:when test="${pageType == 'paymentPage'}">
                  <c:set var="couponCodes" value=""/>
                  <c:forEach items='${cartData.appliedVouchers}' var='voucher' varStatus='status'>
                    <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
                  </c:forEach>
                  <c:set var="cartType" value=""/>
                  <c:choose>
                  <c:when test="${cartData.hasGiftCart}">
                    <c:set var="cartType" value="Gift Cart Order"/>
                  </c:when>
                   <c:when test="${cartData.isNewGearOrder}">
                     <c:set var="cartType" value="New Gear Order"/>
                   </c:when>
                   <c:when test="${cartData.isRentalCart}">
                     <c:set var="cartType" value="Rental Order"/>
                   </c:when>
                  <c:otherwise>
                    <c:set var="cartType" value="Used Gear Order"/>
                  </c:otherwise>
                  </c:choose>
              		gtag('event', 'billing', {
              		  "event_category": "Payment Page",
                  	"event_label": "${cartType}",
              		  "affiliation": "${ycommerce:encodeJavaScript(siteName)}",
              		  "value": ${ycommerce:encodeJavaScript(cartData.totalPrice.value)},
              		  "currency": "USD",
              		  "coupon": "${ycommerce:encodeJavaScript(couponCodes)}",
                   	"damageWaiver" : "${ycommerce:encodeJavaScript(cartData.totalDamageWaiverCost.value)}",
                    "subtotal": "${ycommerce:encodeJavaScript(cartData.subTotal.value)}",
                    "deliveryCost" : "${ycommerce:encodeJavaScript(cartData.deliveryCost.value)}",
                    "tax": "${ycommerce:encodeJavaScript(cartData.taxAvalaraCalculated.value)}"
              		  "items": [
              				<c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
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
              					  "price": "${ycommerce:encodeJavaScript(entry.basePrice.value)}",
              					  "subtotal": "${ycommerce:encodeJavaScript(entry.totalPrice.value)}"
              					}
              					<c:if test='${not status.last}'>,</c:if>
              			   </c:forEach>
              			]
              		});
              	</c:when>

              		<c:when test="${pageType == 'reviewSummaryPage'}">
                                  <c:set var="couponCodes" value=""/>
                                  <c:forEach items='${cartData.appliedVouchers}' var='voucher' varStatus='status'>
                                    <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
                                  </c:forEach>
                                  <c:set var="cartType" value=""/>
                                  <c:choose>
                                  <c:when test="${cartData.hasGiftCart}">
                                    <c:set var="cartType" value="Gift Cart Order"/>
                                  </c:when>
                                   <c:when test="${cartData.isNewGearOrder}">
                                     <c:set var="cartType" value="New Gear Order"/>
                                   </c:when>
                                   <c:when test="${cartData.isRentalCart}">
                                     <c:set var="cartType" value="Rental Order"/>
                                   </c:when>
                                  <c:otherwise>
                                    <c:set var="cartType" value="Used Gear Order"/>
                                  </c:otherwise>
                                  </c:choose>
                              		gtag('event', 'review', {
                              		  "event_category": "Review Page",
                                  	"event_label": "${cartType}",
                              		  "affiliation": "${ycommerce:encodeJavaScript(siteName)}",
                              		  "value": ${ycommerce:encodeJavaScript(cartData.totalPrice.value)},
                              		  "currency": "USD",
                              		  "coupon": "${ycommerce:encodeJavaScript(couponCodes)}",
                                   	"damageWaiver" : "${ycommerce:encodeJavaScript(cartData.totalDamageWaiverCost.value)}",
                                    "subtotal": "${ycommerce:encodeJavaScript(cartData.subTotal.value)}",
                                    "deliveryCost" : "${ycommerce:encodeJavaScript(cartData.deliveryCost.value)}",
                                    "tax": "${ycommerce:encodeJavaScript(cartData.taxAvalaraCalculated.value)}"
                              		  "items": [
                              				<c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
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
                              					  "price": "${ycommerce:encodeJavaScript(entry.basePrice.value)}",
                              					  "subtotal": "${ycommerce:encodeJavaScript(entry.totalPrice.value)}"
                              					}
                              					<c:if test='${not status.last}'>,</c:if>
                              					</c:forEach>
                              			]
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
                       <c:when test="${orderData.isNewGearOrder}">
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
    	"event_label": "${cartType}",
		  "transaction_id": "${orderCode}",
		  "affiliation": "${ycommerce:encodeJavaScript(siteName)}",
		  "value": ${ycommerce:encodeJavaScript(orderData.totalPrice.value)},
		  "currency": "USD",
		  "tax": ${ycommerce:encodeJavaScript(orderData.totalTax.value)},
		  "shipping": ${ycommerce:encodeJavaScript(orderData.deliveryCost.value)},
		  "damageWaiver" : "${ycommerce:encodeJavaScript(orderData.totalDamageWaiverCost.value)}",
      "subtotal": "${ycommerce:encodeJavaScript(orderData.subTotal.value)}",
      "deliveryCost" : "${ycommerce:encodeJavaScript(orderData.deliveryCost.value)}",
      "coupon": "${ycommerce:encodeJavaScript(couponCodes)}"
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
					  "price": "${ycommerce:encodeJavaScript(entry.basePrice.value)}",
					   "subtotal": "${ycommerce:encodeJavaScript(entry.totalPrice.value)}"
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
      'event_category': 'Search Rental Date',
      'event_label': daysInAdvance,
      'value' : lengthOfRental
	});
}

window.mediator.subscribe('trackSearch', function(data) {
		trackSearchClick(data.searchText);
});

function trackSearchClick(searchText) {
	gtag('event', 'select_search', {
      'event_category': 'Search',
      'event_label': searchText
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
  trackPromoCLick(data.voucherError);
	}
});

 function trackPromoCLick(voucherError){
   gtag('event', 'Add Promo - Error', {
     'event_label': voucherError,
     'event_category': 'Cart',
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
    gtag('event', 'cardPayment', {
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
  gtag('event', 'PayPalPayment', {
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
   gtag('event', 'POPayment', {
   'event_label': paymentError,
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
     'event_category': shippingType - shippingOption ,
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
     'event_category': 'Checkout Review',
     'non_interaction': true
   });
 }


</script>
</c:if>