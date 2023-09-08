<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ tag import="com.tealium.dataconnector.hybris.HybrisDataController" %>
<%@ tag import="com.tealium.dataconnector.hybris.HybrisDataConverter" %>
<%@ tag import="com.tealium.dataconnector.hybris.TealiumCustomData" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<%HybrisDataConverter.registerCustomDataClass("ID", new TealiumCustomData()); %>

<%--           ------            Get Page Type         ------           --%>

<c:set var="testPageType" value="${ fn:toLowerCase(pageType) }"/>
<c:set var="currentUserId" value="${ycommerce:encodeJavaScript(cmsPageRequestContextData.user.uid)}"/>
<c:set var="blPageType" value="${IsRentalPage ? 'rental gear' : 'used gear'}"/>
<c:set var="variantName" value="${ blPageType == 'rentalGear' ? 'Rental gear' : 'Used gear'}"/>
<c:set var="currentUserStatus" value="${ (ycommerce:encodeJavaScript(cmsPageRequestContextData.user.uid)) =='anonymous' ? 'anonymous' : 'logged-in'}"/>


<c:choose>
	<c:when test="${ not empty testPageType }">
		<c:set var="currentPageType" value="${testPageType}" />
	</c:when>
	<c:otherwise>
		<c:if test="${not empty cmsPage.label}">
			<c:set var="currentPageType" value="${fn:toLowerCase(cmsPage.label)}" />
		</c:if>
	</c:otherwise>
</c:choose>

<c:choose>
 <c:when test="${pageType == 'CATEGORY' || pageType == 'PRODUCTSEARCH'}">
 	<c:set var="totalSearchResults" value="${searchPageData.pagination.totalNumberOfResults}" />
 	<c:set var="searchKeyword" value="${searchPageData.freeTextSearch}" />
 	<c:set var="blPageType" value="${IsRentalPage ? 'Rental' : 'Used'}"/>
 </c:when>
 </c:choose>

<c:choose>
	<c:when test="${pageType == 'PRODUCT'}">
	 <c:if test="${not empty rentalDate.selectedFromDate}">
	   <c:set var="rentalDays" value="${rentalDate.numberOfDays}" />
                </c:if>
		<c:set var="categories" value="" />
		<c:set var="blPageType" value="${IsRentalPage ? 'Rental' : 'Used'}"/>
		<c:forEach items="${product.categories}" var="category">
			<c:set var="categories">${ycommerce:encodeJavaScript(category.name)}</c:set>
		</c:forEach>

		<c:if test="${not empty product.stock.stockLevelStatus.code}">
    <c:set var="stockStatus" value="${fn:trim(product.stock.stockLevelStatus.code)}" />
		</c:if>
		<c:set var="stockStatus" value="${empty stockStatus ? 'Item In Stock' : 'Item Out of Stock'}" />

	</c:when>
</c:choose>


<c:choose>
<c:when test="${currentPageType == 'orders'}">
	<c:set var="genericPageType" value="My_Account_Orders"/>
</c:when>
<c:when test="${currentPageType == 'address-book'}">
	<c:set var="genericPageType" value="My_Account_Addresses"/>
</c:when>
<c:when test="${currentPageType == 'bookmarks'}">
	<c:set var="genericPageType" value="My_Account_Bookmarks"/>
</c:when>
<c:when test="${currentPageType == 'update-email'}">
	<c:set var="genericPageType" value="My_Account_Update_Email"/>
</c:when>
<c:when test="${currentPageType == 'updatepassword'}">
	<c:set var="genericPageType" value="My_Account_Update_Password"/>
</c:when>
<c:when test="${currentPageType == 'saved-carts'}">
	<c:set var="genericPageType" value="My_Account_Saved_Carts"/>
</c:when>
<c:when test="${currentPageType == 'verificationimages'}">
	<c:set var="genericPageType" value="My_Account_Verification_Images"/>
</c:when>
<c:when test="${currentPageType == 'payment-details'}">
	<c:set var="genericPageType" value="My_Account_Payment_Details"/>
</c:when>
<c:when test="${currentPageType == 'update-profile'}">
	<c:set var="genericPageType" value="My_Account_Update_Profile"/>
</c:when>
<c:when test="${currentPageType == '/ship-or-pickup'}">
	<c:set var="genericPageType" value="Content_Ship_or_Pickup"/>
</c:when>
<c:when test="${currentPageType == '/affiliate'}">
	<c:set var="genericPageType" value="Content_Affiliate"/>
</c:when>
<c:when test="${currentPageType == '/howitworks'}">
	<c:set var="genericPageType" value="Content_How_it_Works"/>
</c:when>
<c:when test="${currentPageType == 'notfound'}">
	<c:set var="genericPageType" value="Error404"/>
</c:when>
</c:choose>

<c:choose>

  <c:when test="${pageType == 'CART' && !_href.contains('cart')}">
      <c:set var="couponCodes" value=""/>
      <c:forEach items='${cartData.appliedVouchers}' var='voucher' varStatus='status'>
        <c:set var="couponCodes" value="${couponCodes}${voucher}${not status.last ? ',':''}"/>
      </c:forEach>
        }
  	</c:when>
  	</c:choose>
  	

<%--                    ---------------------                            --%>
<!-- UDO for page type "${currentPageType}" -->
<c:choose>
	<c:when test="${currentPageType == 'homepage'}">
		
		<script type="text/javascript">
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "HomePage"
				};
			dmpgDl.user = 
			  	{
    				id: "${currentUserId }",
  					status: "${currentUserStatus}"
			  	};

		</script>
		
		
	</c:when>
	
	<c:when test="${currentPageType == 'productsearch'}">
		
		<script type="text/javascript">
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Search_Result_Page"
				};
			dmpgDl.user = 
			  	{
    				id: "${ currentUserId }",
  					status: "logged-in"
			  	};
			  	
		</script>
	</c:when>
	
	<c:when test="${currentPageType == 'category'}">
	
	<script type="text/javascript">
		
           window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "${blPageType}_Category"
				};
			dmpgDl.user = 
			  	{
    				id: "${currentUserId}",
  					status: "logged-in"
			  	};

		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'product'}">
	
	<script type="text/javascript">
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "${blPageType}_product"
				};
			dmpgDl.user = 
			  	{
    				id: "${currentUserId}",
  					status: "logged-in"
			  	};
			  	
		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'cart'}">
	
	<script type="text/javascript">
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Cart"
				};
			dmpgDl.user = 
			  	{
    				id: "${ currentUserId }",
  					status: "logged-in"
			  	};

		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'profile'}">
	
	<script type="text/javascript">
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "homepage"
				};
			dmpgDl.user = 
			  	{
    				id: "${ currentUserId }",
  					status: "logged-in"
			  	};

		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'orderconfirmation'}">
	
	<script type="text/javascript">
	<c:set var="paymentType" value="${orderData.paymentInfo != null ? 'card': 'gift-card'}"/>
	
	<c:forEach items="${orderData.consignments}" var="consignment">
	<c:set var="shipmentType" value="${consignment.optimizedShippingMethodType == 'WAREHOUSE2CUSTOMER' ? 'Ship to Home' : 'Ship to UPS Store'}"/>
	 </c:forEach>
	
	<c:set var="orderType" value=""/>
					<c:choose>
    					<c:when test="${orderData.hasGiftCart}">
                          <c:set var="orderType" value="Gift Cart"/>
    					</c:when>
   					   <c:when test="${orderData.isRetailGearOrder eq true}">
       					 <c:set var="orderType" value="New Gear"/>
   					   </c:when>
   					  <c:when test="${orderData.isRentalCart}">
       					 <c:set var="orderType" value="rental"/>
    				  </c:when>
   					 <c:otherwise>
      				   <c:set var="orderType" value="usedGear"/>
    				</c:otherwise>
				</c:choose>


		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Order_Confirmation"
				};
			dmpgDl.user = 
			  	{
    				id: "${ currentUserId }",
  					status: "logged-in"
			  	};
			  	
			  	
			  	dmpgDl.transaction = 
			  	{
    				id: "${orderCode}",
  					daysUntilRental: "logged-in",
  					rentalStartDate: "${orderData.rentalStartDateForJs.replace(' ','').replace(',','-')}",
  					rentalDuration: "${orderData.rentalEndDate} - {orderData.rentalStartDate}"
  					"cart" : 
			  			{
			  				"lines" : [
			  					{
			  					 product: 
			  					   {
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
					  						 "subCategory2": "",
            								"subCategory3": "",
					  						"variant": "${orderType}",
            		  						"list_position": ${status.index},
					  						"quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
					  						"value": {
             									 "displayGross": "${ycommerce:encodeJavaScript(entry.basePrice.value)}",
              						 			"displayTax": "${ycommerce:encodeJavaScript(orderData.totalTax.value)}"
         							 		 }
					  					 }
											<c:if test='${not status.last}'>
												,
											</c:if>
			 						 </c:forEach>
			 				 
  						
			  				shipping : {
			  					lines : [
			  						{
    									tier: "${shipmentType}",
  										method: "${ycommerce:encodeJavaScript(orderData.deliveryMode.code)}",
  										"value": {
             									 "displayGross": "${ycommerce:encodeJavaScript(orderData.deliveryCost.value)}",
              						 			 "displayTax": "${ycommerce:encodeJavaScript(orderData.totalTax.value)}"
         							 		 }
  									}
  								]},
  					{
			  			payment : [
			  			{
         				 "type": "${paymentType}",
         				 "value": {
          					  "displayGross": "${ycommerce:encodeJavaScript(orderData.totalPrice.value)}"
          					}
  					],
  					{
			  			value :
			  				{
    							displayGross: "${ycommerce:encodeJavaScript(orderData.totalPrice.value)}",
    							displayTax: "${ycommerce:encodeJavaScript(orderData.totalTax.value)}"
  							}
  					 }
			  		}
			  	}
			  	}
			  	
		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'shippingpage'}">
	
	<script type="text/javascript">
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Cart_Delivery_Details"
				};
			dmpgDl.user = 
			  	{
    				id: "${ currentUserId }",
  					status: "logged-in"
			  	};

		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'paymentpage'}">
	
	<script type="text/javascript">
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Cart_Delivery_Details"
				};
			dmpgDl.user = 
			  	{
    				id: "${ currentUserId }",
  					status: "logged-in"
			  	};
		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'reviewsummarypage'}">
	
	<script type="text/javascript">
		
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Cart_Order_Review"
				};
			dmpgDl.user = 
			  	{
    				id: "${ currentUserId }",
  					status: "logged-in"
			  	};
		</script>
		
	</c:when>
	
	
	
	<c:otherwise>
		
		<script type="text/javascript">
		${currentPageType}
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "${genericPageType}"
				};
			dmpgDl.user = 
			  	{
    				id: "${ currentUserId }",
  					status: "logged-in"
			  	};

		</script>
		
	</c:otherwise>
</c:choose>
