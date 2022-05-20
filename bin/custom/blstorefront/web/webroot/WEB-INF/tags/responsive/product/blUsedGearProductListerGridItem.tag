<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="product" required="true" type="de.hybris.platform.commercefacades.product.data.ProductData" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="action" tagdir="/WEB-INF/tags/responsive/action" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="theme" tagdir="/WEB-INF/tags/shared/theme" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<spring:theme code="text.addToCart" var="addToCartText"/>
<c:url value="${product.url}" var="productUrl"/>
<c:set value="${not empty product.potentialPromotions}" var="hasPromotion"/>
<c:set value="image coming soon" var="altText"/>
<c:choose>
 <c:when test="${product.retailGear eq true}">
                <product:blNewGearProductListerGridItem product="${product}"/>
</c:when>
<c:otherwise>
<div class="col-md-6 col-lg-4">
<div class="card">
<c:if test ="${product.productTagValues ne null && product.productTagValues ne 'Great Value' && product.productTagValues ne 'Staff Pick'}">
<span class="badge badge-new">${product.productTagValues}</span>
</c:if>

<!-- BL-926: Added new file for Gift Card --> 
<product:productListImagePanel productType="UsedGearProduct" product="${product}"/>
		

<!-- BL-926: Added condition for Gift Card as per requirement --> 
      <c:if test="${product.productType ne 'GIFTCARD'}">
		<p class="overline">${product.manufacturer}</p>
	  </c:if>
 <h3 class="product">
	 <div class="sizeAdj-6">
 <c:url var="usedGearUrl" value="/buy/product/${product.code}"/>
            <a href="${usedGearUrl}" role="button"  class="js-pdplinkUrl" data-productCode="${product.code}" data-brand="${product.manufacturer}"
             data-productName="${ycommerce:sanitizeHTML(product.displayName)}" data-productType="used gear">
             <c:out escapeXml="false" value="${ycommerce:sanitizeHTML(product.name)}"/> </a>
	 </div>
  </h3>
		<ycommerce:testId code="product_wholeProduct">

			<c:if test="${not empty product.potentialPromotions}">
				<div class="promo">
					<c:forEach items="${product.potentialPromotions}" var="promotion">
						${ycommerce:sanitizeHTML(promotion.description)}
					</c:forEach>
				</div>
			</c:if>
	<h4 class="price">
		<div class="sizeAdj-6">
	<!-- BL-926: Added condition for Gift Card as per requirement --> 
	<c:choose>
	    <c:when test="${product.productType eq 'GIFTCARD'}">
	     <spring:theme code="slp.giftcard.price" /> 
	    </c:when>
	   <c:otherwise>
	      <small><spring:theme code="text.used.product.card.price.starting.at"/></small>
	   </c:otherwise>
	</c:choose>
			<c:choose>
				<c:when test="${not empty product.serialIncentivizedPrice }">
					<format:price priceData="${product.serialIncentivizedPrice}" />&nbsp;<small><strike><format:price priceData="${product.serialfinalSalePrice}" /></strike></small>
				</c:when>
				<c:otherwise>
					<format:price priceData="${product.serialfinalSalePrice}" />
				</c:otherwise>
			</c:choose>
		</div>
	</h4>
	<c:if test="${product.ugPromotionMessage ne null && product.serialPromotionPrice.value > 0 && product.onSale eq true}">
  	<p class="sale"><span class="saleprice"><format:price	priceData="${product.serialPromotionPrice}" />&nbsp;&nbsp;${fn:escapeXml(product.ugPromotionMessage)} </p>
	</c:if>

			<c:forEach var="variantOption" items="${product.variantOptions}">
				<c:forEach items="${variantOption.variantOptionQualifiers}" var="variantOptionQualifier">
					<c:if test="${variantOptionQualifier.qualifier eq 'rollupProperty'}">
	                    <c:set var="rollupProperty" value="${variantOptionQualifier.value}"/>
	                </c:if>
					<c:if test="${variantOptionQualifier.qualifier eq 'thumbnail'}">
	                    <c:set var="imageUrlHtml" value="${fn:escapeXml(variantOptionQualifier.value)}"/>
	                </c:if>
	                <c:if test="${variantOptionQualifier.qualifier eq rollupProperty}">
	                    <c:set var="variantNameHtml" value="${fn:escapeXml(variantOptionQualifier.value)}"/>
	                </c:if>
				</c:forEach>
				<img style="width: 32px; height: 32px;" src="${imageUrlHtml}" title="${variantNameHtml}" alt="${variantNameHtml}"/>
			</c:forEach>



		<c:set var="product" value="${product}" scope="request"/>
		<c:set var="addToCartText" value="${addToCartText}" scope="request"/>
		<c:set var="addToCartUrl" value="${addToCartUrl}" scope="request"/>
		<c:set var="isGrid" value="true" scope="request"/>
		<!-- BL-926: Added condition for Gift Card as per requirement --> 
		<c:url var="usedUrl" value="/buy/product/${product.code}"/>
			<c:choose>
				<c:when test="${product.productType eq 'GIFTCARD'}">
					<a href="${usedUrl}" class="btn btn-primary js-pdplinkUrl" data-productCode="${product.code}" data-brand="gift cart"
              data-productName="${ycommerce:sanitizeHTML(product.displayName)}" data-productType="used gear">
					<spring:theme code="text.product.list.by.now" /></a>
				</c:when>
				<c:otherwise>
					<div class="addtocart btnwidth">
						<div
							class="actions-container-for-${fn:escapeXml(component.uid)} <c:if test="${ycommerce:checkIfPickupEnabledForStore() and product.availableForPickup}"> pickup-in-store-available</c:if>">
							<action:actions element="div" parentComponent="${component}" />
						</div>
					</div>
				</c:otherwise>
			</c:choose>

		</ycommerce:testId>
</div>
</div>
</c:otherwise>
</c:choose>
