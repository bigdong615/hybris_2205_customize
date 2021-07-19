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
		<c:set var="blPageType" value="${IsRentalPage ? 'Rental' : 'Used'}"/>
		<c:forEach items="${product.categories}" var="category">
			<c:set var="categories">${ycommerce:encodeJavaScript(category.name)}</c:set>
		</c:forEach>
    gtag('event', 'view_item', {
       'event_category': 'engagement',
       'event_label' : 'Product',
      "items": [
        {
          "id": "${product.code}",
          "name": "${product.name}",
          "brand": "${product.manufacturer}",
          "category": "${categories}",
          "variant" : "${ycommerce:encodeJavaScript(blPageType)}"
        }
      ]
    });
	</c:when>

	<c:when test="${pageType == 'CATEGORY' || pageType == 'PRODUCTSEARCH'}">
		 <c:set var="listName" value="${pageType == 'CATEGORY' ? 'List' : 'Search'}"/>
     <c:set var="variantName" value="${ blPageType == 'rentalgear' ? 'Rental' : 'Used'}"/>

		<c:choose>
			<c:when test="${searchPageData.pagination.totalNumberOfResults > 0}">
				<c:if test="${not empty searchPageData.results}">
						gtag('event', 'view_item_list', {
							"event_category": "engagement",
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

	<c:when test="${pageType == 'ORDERCONFIRMATION'}">
		<c:set var="orderCode" value="${ycommerce:encodeJavaScript(orderData.code)}"/>

		gtag('event', 'purchase', {
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
					  "price": "${ycommerce:encodeJavaScript(entry.product.price.value)}"
					}
					<c:if test='${not status.last}'>
						,
					</c:if>
			  </c:forEach>
			],

			<c:if test="${not empty orderData.appliedVouchers}">
				"coupon": "${ycommerce:encodeJavaScript(orderData.appliedVouchers[0])}"
			</c:if>
		});
	</c:when>
	<c:otherwise>
	</c:otherwise>
</c:choose>

function trackAddToCart_google(productCode, quantityAdded,cartData,productBrand,productType,category) {
	gtag('event', 'add_to_cart', {
	   "event_category": "ecommerce",
     "event_label": "Cart",
	   "currency" : "USD",
	   "value": cartData.productPrice,
	  "items": [
        {
          "id": productCode,
          "name":cartData.productName ,
          "brand": productBrand,
          "category":category,
          "variant": productType,
          "quantity": quantityAdded
        }
      ]
	});
}

function trackRemoveFromCart(productCode,productName,initialQuantity) {
	gtag('event', 'remove_from_cart', {
	 "event_category": "ecommerce",
   "event_label": "RemoveFromCart",
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
		trackAddToCart_google(data.productCode, data.quantity,data.cartData,data.productBrand,data.productType,data.category);
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
	gtag('event', 'select_content', {
	  "event_category": "engagement",
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
	gtag('event', 'select_content', {
      'event_category': 'Search Rental Date',
      'event_label': daysInAdvance,
      'value' : lengthOfRental
	});
}

window.mediator.subscribe('trackSearch', function(data) {
		trackSearchClick(data.searchText);
});

function trackSearchClick(searchText) {
	gtag('event', 'select_content', {
      'event_category': 'Search',
      'event_label': searchText
	});
}


window.mediator.subscribe('loginClick', function(data) {
	if (data.userId)
	{
		trackLoginClick(data.userId);
	}
});

function trackLoginClick(userId) {
	gtag('event', 'loginTrack', {
	  'id': userId
	});
}

window.mediator.subscribe('registerClick', function(data) {
	if (data.userId)
	{
		trackRegisterClick(data.userId);
	}
});

function trackRegisterClick(userId) {
	gtag('event', 'registerClick', {
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
     'event_label': 'voucherError',
     'event_category': 'Cart',
     'non_interaction': true
   });
 }

window.mediator.subscribe('applyCreditCart', function(data) {
	if (data.paymentError)
	{
	trackCreditCart(data.paymentError)
	}
});

 function trackCreditCart(paymentError){
    gtag('event', 'cardPayment', {
    'event_label': 'paymentError',
    'event_category': 'Checkout',
    'non_interaction': true
  });
 }

window.mediator.subscribe('applyPayPal', function(data) {
	if (data.paymentError)
	{
  trackPayPalClick(data.paymentError)
	}
});

function trackPayPalClick(paymentError) {
  gtag('event', 'PayPalPayment', {
   'event_label': 'paymentError',
   'event_category': 'Checkout',
   'non_interaction': true
  });
}

window.mediator.subscribe('applyPO', function(data) {
	if (data.paymentError)
	{
	 trackPOClick(data.paymentError)
	}
});

function trackPOClick(paymentError) {
   gtag('event', 'POPayment', {
   'event_label': 'paymentError',
   'event_category': 'Checkout',
   'non_interaction': true
 });
}

</script>
</c:if>