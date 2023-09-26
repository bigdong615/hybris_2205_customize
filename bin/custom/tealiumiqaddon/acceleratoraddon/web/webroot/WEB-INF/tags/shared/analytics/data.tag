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
<c:set var="currentUserId" value="${ycommerce:encodeJavaScript(cmsPageRequestContextData.user.customerID)}"/>

<c:set var="variantName" value="${ blPageType == 'rentalGear' ? 'Rental gear' : 'Used gear'}"/>
<c:set var="currentUserStatus" value="${ (ycommerce:encodeJavaScript(cmsPageRequestContextData.user.uid)) =='anonymous' ? 'logged-out' : 'logged-in'}"/>

<c:set var="datesSettedInSession" value="${empty rentalDate.selectedFromDate ? false : true}" />

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
 	<c:set var="blCategoryPageType" value="${blPageType == 'rentalgear' ? 'Rental' : 'Used'}"/>
 </c:when>
 </c:choose>
<c:set var="blPageType" value="${IsRentalPage ? 'Rental' : 'Used'}"/>
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
		<c:set var="stockStatus" value="${empty stockStatus ? 'in stock' : 'Item Out of Stock'}" />

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
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalDate.selectedFromDateMMDDYYY}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
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
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalDate.selectedFromDateMMDDYYY}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			  dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
				};
			  	
			  	dmpgDl.products = [
			  	
			  	 <c:forEach items='${searchPageData.results}' var='product' varStatus='status'>
			  	{
			  	"id": "${product.code}",
			  	"name": "${product.name}",
          		"brand": "${product.manufacturer}",
          		<c:choose>
				  <c:when test="${not empty product.categoriesList[0]}">
					"category": "${ycommerce:encodeJavaScript(product.categoriesList[0].code)}",
				  </c:when>
				  <c:when test="${not empty product.categoriesList[1]}">
					"subCategory2": "${ycommerce:encodeJavaScript(product.categoriesList[1].code)}",
				  </c:when>
				  <c:when test="${not empty product.categoriesList[2]}">
				    "subCategory3": "${ycommerce:encodeJavaScript(product.categoriesList[2].code)}",
				  </c:when>
			    </c:choose>
         		"variant" : "${ycommerce:encodeJavaScript(blCategoryPageType).toLowerCase()}",
         		"listName": "${searchKeyword}",
         		"stockAvailability" : "${datesSettedInSession ? (product.stock.stockLevelStatus.code == 'outOfStock' ? 'out of stock' : 'in stock') : ''}",
  				"index": "${status.index + 1}",
			  	"value": {
       					"displayGross": "${product.price.formattedValue.replace('$','')}"
     					 }
			  	 },	
			  	 </c:forEach>
			  	 ]
			  	}
			  	
		</script>
	</c:when>
	
	<c:when test="${currentPageType == 'category'}">
	
	<script type="text/javascript">
		
           window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
				<c:choose>
				<c:when test="${ blCategoryPageType == 'Rental' }">
					<c:set var="categoryScreenName" value="${blCategoryPageType}_${categoryName}" />
				</c:when>
				<c:otherwise>
					<c:set var="categoryScreenName" value="${blCategoryPageType}" />
				</c:otherwise>
			</c:choose>
				
  					"type": "${categoryScreenName}"
				};
			dmpgDl.user = 
			  	{
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalDate.selectedFromDateMMDDYYY}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			 dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
				};

			dmpgDl.products = [
			  	
			  	 <c:forEach items='${searchPageData.results}' var='product' varStatus='status'>
			  	{
			  	"id": "${product.code}",
			  	"name": "${product.name}",
          		"brand": "${product.manufacturer}",
          		<c:choose>
				  <c:when test="${not empty product.categoriesList[0]}">
					"category": "${ycommerce:encodeJavaScript(product.categoriesList[0].code)}",
				  </c:when>
				  <c:when test="${not empty product.categoriesList[1]}">
					"subCategory2": "${ycommerce:encodeJavaScript(product.categoriesList[1].code)}",
				  </c:when>
				  <c:when test="${not empty product.categoriesList[2]}">
				    "subCategory3": "${ycommerce:encodeJavaScript(product.categoriesList[2].code)}",
				  </c:when>
			    </c:choose>
         		"variant" : "${ycommerce:encodeJavaScript(blCategoryPageType).toLowerCase()}",
         		"listName": "${searchKeyword}",
  				"index": "${status.index+1}",
  				"stockAvailability" : "${datesSettedInSession ? (product.stock.stockLevelStatus.code == 'outOfStock' ? 'out of stock' : 'in stock') : ''}",
			  	"value": 
			  	    {
       				  "displayGross": "${product.price.formattedValue.replace('$','')}"
     		    	 }
			  	 },	
			  	 
			  	 </c:forEach>
			  	 ]
		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'product'}">
	
	<script type="text/javascript">
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "${blPageType}_${product.code}"
				};
			dmpgDl.user = 
			  	{
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalStartDate}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			  	dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
				};
			  
			  dmpgDl.products = [
			  	{
			  	"id": "${product.code}",
			  	"name": "${product.name}",
          		"brand": "${product.manufacturer}",
          		<c:choose>
				 <c:when test="${not empty product.categoriesList[0]}">
				 "category": "${ycommerce:encodeJavaScript(product.categoriesList[0].code)}",
				  </c:when>
				  <c:when test="${not empty product.categoriesList[1]}">
					"subCategory2": "${ycommerce:encodeJavaScript(product.categoriesList[1].code)}",
				  </c:when>
				  <c:when test="${not empty product.categoriesList[2]}">
				    "subCategory3": "${ycommerce:encodeJavaScript(product.categoriesList[2].code)}",
				  </c:when>
			    </c:choose>
         		"variant" : "${ycommerce:encodeJavaScript(blPageType.toLowerCase())}",
         		"stockStatus" : "${datesSettedInSession ? (product.stock.stockLevelStatus.code == 'outOfStock' ? 'out of stock' : 'in stock') : ''}",
			  	"value": 
			  	    {
       				   "displayGross": "${product.price.formattedValue.replace('$','')}"
     				}
			  	 }	
			  	 ],
			  	
			  	 dmpgDl.modules = [
   					{
   					 "id": "${categories}-modules-pdp",
   					 "name": "dont-forget",
     				 "placement": "pdp-add-products",
     				 "items": [
			           <c:forEach items='${productReferences}' var='productReference' varStatus='status'>
       					 {
         				 "id": "${productReference.target.code}",
          				 "type": "product",
        				 "position": "${status.index + 1}"
       					 }<c:if test='${not status.last}'>,</c:if>
               		</c:forEach>
               			]
               			}
               			],
               			
               			
               		dmpgDl.assets = 
   							 {
     						   "products": [
			           <c:forEach items='${productReferences}' var='productReference' varStatus='status'>
       							  {
         							"id": "${productReference.target.code}",
       								 "name": "${productReference.target.name}",
       								 "brand": "${productReference.target.manufacturer}",
        							<c:choose>
				 						 <c:when test="${not empty productReference.target.categoriesList[0]}">
											"category": "${ycommerce:encodeJavaScript(productReference.target.categoriesList[0].code)}",
				  						 </c:when>
				 					     <c:when test="${not empty productReference.target.categoriesList[1]}">
											"subCategory2": "${ycommerce:encodeJavaScript(productReference.target.categoriesList[1].code)}",
				 						 </c:when>
				  						 <c:when test="${not empty productReference.target.categoriesList[2]}">
				   						    "subCategory3": "${ycommerce:encodeJavaScript(productReference.target.categoriesList[2].code)}",
				 						 </c:when>
			    					 </c:choose>
			    					 "variant" : "${ycommerce:encodeJavaScript(blPageType).toLowerCase()}",
       								 "value": 
       								    {
         								 "displayGross": "${productReference.target.price.formattedValue.replace('$','')}"
        								}
       							    }<c:if test='${not status.last}'>,</c:if>
               			</c:forEach>
               			   ]
               			  }
               			}
		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'cart'}">
	
	<c:set var="orderType" value=""/>
					<c:choose>
    					<c:when test="${cartData.hasGiftCart}">
                          <c:set var="orderType" value="Gift Cart"/>
    					</c:when>
   					   <c:when test="${cartData.isRetailGearOrder eq true}">
       					 <c:set var="orderType" value="New Gear"/>
   					   </c:when>
   					  <c:when test="${cartData.isRentalCart}">
       					 <c:set var="orderType" value="rental"/>
    				  </c:when>
   					 <c:otherwise>
      				   <c:set var="orderType" value="used"/>
    				</c:otherwise>
				</c:choose>
	
	<script type="text/javascript">
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Cart"
				};
			dmpgDl.user = 
			  	{
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalDate.selectedFromDateMMDDYYY}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
				};
				
				 dmpgDl.cart  = 
				    {
				    "lines": [
				         <c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
        					{
        					"product": {
			  					"id": "${entry.product.code}",
			  					"name": "${entry.product.name}",
          						"brand": "${entry.product.manufacturer}",
          						<c:choose>
				 		   		 <c:when test="${not empty entry.product.categoriesList[0]}">
								"category": "${ycommerce:encodeJavaScript(entry.product.categoriesList[0].code)}",
				 		    	</c:when>
				 				 <c:when test="${not empty entry.product.categoriesList[1]}">
								"subCategory2": "${ycommerce:encodeJavaScript(entry.product.categoriesList[1].code)}",
				 				 </c:when>
				  				<c:when test="${not empty entry.product.categoriesList[2]}">
				   				 "subCategory3": "${ycommerce:encodeJavaScript(entry.product.categoriesList[2].code)}",
				  				</c:when>
			    				</c:choose>
			   					 "quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
         						"variant" : "${ycommerce:encodeJavaScript(orderType)}",
         						"stockAvailability" : "${datesSettedInSession ? (entry.product.stock.stockLevelStatus.code== 'outOfStock' ? 'out of stock' : 'in stock') : ''}",
			  					"value": 
			  	  			 	 {
       				  		   	   "displayGross": "${entry.basePrice.value}"
     							  }
			  				 }	
        					}<c:if test='${not status.last}'>,</c:if>
        			  </c:forEach>
        			],
        			"value" :
			  				{
    							"displayGross": "${ycommerce:encodeJavaScript(cartData.totalPrice.value)}",
    							"displayTax": "${ycommerce:encodeJavaScript(cartData.totalTax.value)}"
  							}
			  	}
			  	}
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
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalDate.selectedFromDateMMDDYYY}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
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
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${daysUntilRental}",
  					"rentalStartDate": "${rentalStartDateForTealium}",
  					"rentalDuration": "${orderData.rentalDates.numberOfDays}"
			  	};
			 dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
				};
			  	
			  	dmpgDl.transaction = 
			  	{
    				"id": "${orderCode}",
  					"cart" : 
			  			{
			  				"lines" : [
			  					   <c:forEach items='${orderData.entries}' var='entry' varStatus='status'>
			  					      {
			  					          "product":  {
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
					  						"quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
					  						"value": {
             									 "displayGross": "${ycommerce:encodeJavaScript(entry.basePrice.value)}",
              						 			 "displayTax": "${ycommerce:encodeJavaScript(entry.avalaralinetax)}"
         							 		 }
         							 		}
					  					 }<c:if test='${not status.last}'>,</c:if>
											
			 						 </c:forEach>
  						     ],
  						     
			  				"shipping" : {
			  					"lines" : [
			  						{
    									"tier": "${shipmentType}",
  										"method": "${ycommerce:encodeJavaScript(orderData.deliveryMode.code)}",
  										"value": {
             									 "displayGross": "${ycommerce:encodeJavaScript(orderData.deliveryCost.formattedValue.replace('$',''))}"
         							 		 }
  									}
  								]},
  								
			  			"payment" : [
			  			{
         				 "type": "${paymentType}",
         				 "value": {
          					  "displayGross": "${ycommerce:encodeJavaScript(orderData.totalPrice.value)}"
          					}
  					],
  					
			  			"value" :
			  				{
    							"displayGross": "${ycommerce:encodeJavaScript(orderData.totalPrice.value)}",
    							"displayTax": "${ycommerce:encodeJavaScript(orderData.totalTax.value)}"
  							}
  					 }
			  		}
		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'shippingpage'}">
	
	<script type="text/javascript">
	<c:set var="paymentType" value="${cartData.paymentInfo != null ? 'card': 'gift-card'}"/>
		
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Cart_Delivery_Details"
				};
			dmpgDl.user = 
			  	{
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalDate.selectedFromDateMMDDYYY}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
				};
				
			dmpgDl.transaction = 
			  	{
  					"cart" : 
			  			{
			  				"lines" : [
			  					   <c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
			  					      {
			  					          "product":  {
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
					  						"quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
					  						"value": {
             									 "displayGross": "${ycommerce:encodeJavaScript(entry.basePrice.value)}",
              						 			 "displayTax": "${ycommerce:encodeJavaScript(entry.avalaralinetax)}"
         							 		 }
         							 		}
					  					 }<c:if test='${not status.last}'>,</c:if>
											
			 						 </c:forEach>
  						     ],
  					
			  			"value" :
			  				{
    							"displayGross": "${ycommerce:encodeJavaScript(cartData.totalPrice.value)}",
    							"displayTax": "${ycommerce:encodeJavaScript(cartData.totalTax.value)}"
  							}
  					 }
			  		}
		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'paymentpage'}">
	<script type="text/javascript">
	<c:set var="paymentType" value="${cartData.paymentInfo != null ? 'card': 'gift-card'}"/>
		<c:set var="shipmentType" value="${cartData.deliveryMode.code.startsWith('UPS_STORE') ? 'Ship to UPS Store' : 'Ship to Home'}"/>
	
	
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Cart_Delivery_Details"
				};
			dmpgDl.user = 
			  	{
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalDate.selectedFromDateMMDDYYY}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
				};	
				
				dmpgDl.transaction = 
			  	{
  					"cart" : 
			  			{
			  				"lines" : [
			  					   <c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
			  					      {
			  					          "product":  {
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
					  						"quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
					  						"value": {
             									 "displayGross": "${ycommerce:encodeJavaScript(entry.basePrice.value)}",
              						 			 "displayTax": "${ycommerce:encodeJavaScript(entry.avalaralinetax)}"
         							 		 }
         							 		}
					  					 }<c:if test='${not status.last}'>,</c:if>
											
			 						 </c:forEach>
  						     ],
  						     
			  				"shipping" : {
			  					"lines" : [
			  						{
    									"tier": "${shipmentType}",
  										"method": "${ycommerce:encodeJavaScript(cartData.deliveryMode.code)}",
  										"value": {
             									 "displayGross": "${ycommerce:encodeJavaScript(cartData.deliveryCost.formattedValue.replace('$',''))}"
         							 		 }
  									}
  								]},
			  			"value" :
			  				{
    							"displayGross": "${ycommerce:encodeJavaScript(cartData.totalPrice.value)}",
    							"displayTax": "${ycommerce:encodeJavaScript(cartData.totalTax.value)}"
  							}
  					 }
			  		}
		</script>
		
	</c:when>
	
	<c:when test="${currentPageType == 'reviewsummarypage'}">
	
		<script type="text/javascript">
	<c:set var="paymentType" value="${cartData.paymentInfo != null ? 'card': 'gift-card'}"/>
	<c:set var="shipmentType" value="${cartData.deliveryMode.code.startsWith('UPS_STORE') ? 'Ship to UPS Store' : 'Ship to Home'}"/>
	
          window.dmpgDl = window.dmpgDl || {};
			dmpgDl.events = [];

			dmpgDl.screen = 
				{
  					type: "Cart_Order_Review"
				};
			dmpgDl.user = 
			  	{
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalDate.selectedFromDateMMDDYYY}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			 dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
				};
				dmpgDl.transaction = 
			  	{
  					"cart" : 
			  			{
			  				"lines" : [
			  					   <c:forEach items='${cartData.entries}' var='entry' varStatus='status'>
			  					      {
			  					          "product":  {
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
					  						"quantity": ${ycommerce:encodeJavaScript(entry.quantity)},
					  						"value": {
             									 "displayGross": "${ycommerce:encodeJavaScript(entry.basePrice.value)}",
              						 			 "displayTax": "${ycommerce:encodeJavaScript(entry.avalaralinetax)}"
         							 		 }
         							 		}
					  					 }<c:if test='${not status.last}'>,</c:if>
											
			 						 </c:forEach>
  						     ],
  						     
			  				"shipping" : {
			  					"lines" : [
			  						{
    									"tier": "${shipmentType}",
  										"method": "${ycommerce:encodeJavaScript(cartData.deliveryMode.code)}",
  										"value": {
             									 "displayGross": "${ycommerce:encodeJavaScript(cartData.deliveryCost.formattedValue.replace('$',''))}"
         							 		 }
  									}
  								]},
  								
			  			"payment" : [
			  			{
         				 "type": "${paymentType}",
         				 "value": {
          					  "displayGross": "${ycommerce:encodeJavaScript(cartData.totalPrice.value)}"
          					}
  					],
			  			"value" :
			  				{
    							"displayGross": "${ycommerce:encodeJavaScript(cartData.totalPrice.value)}",
    							"displayTax": "${ycommerce:encodeJavaScript(cartData.totalTax.value)}"
  							}
  					 }
			  		}
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
    				id: "${currentUserId}",
  					status: "${currentUserStatus}",
  					"daysUntilRental": "${rentalDate.daysUntilRental}",
  					"rentalStartDate": "${rentalDate.selectedFromDateMMDDYYY}",
  					"rentalDuration": "${rentalDate.selectedDays}"
			  	};
			dmpgDl.platform = 
			   {
 				   "env": "${jalosession.tenant.config.getParameter('tealiumiqaddon.target')}"
				};
		</script>
		
	</c:otherwise>
</c:choose>
