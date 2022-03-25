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
<c:set value="image coming soon" var="altText"/>

<div class="col-md-6 col-lg-4">
  <div class="card">
  <c:choose>
    <c:when test="${product.isRetailGearInStock eq true}">
      <span class="badge badge-limited-stock"><spring:theme
          code="text.product.newgear.flag.inStock"/></span>
    </c:when>
    <c:otherwise>
      <span class="badge badge-out-of-stock"><spring:theme
          code="text.product.newgear.flag.outOfStock" /></span>
  	</c:otherwise>
  	</c:choose>
    <product:productListImagePanel productType="UsedGearProduct" product="${product}"/>
    <h6 class="product">
     <c:url var="newGearUrl" value="/buy/product/${product.code}"/>
     <a href="${newGearUrl}" role="button"  class="js-pdplinkUrl" data-productCode="${product.code}">
       <c:out escapeXml="false" value="${ycommerce:sanitizeHTML(product.name)}"/> </a>
    </h6>
		<ycommerce:testId code="product_wholeProduct">
		  <h6 class="price"><format:price priceData="${product.retailGearPrice}"/></h6>
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
      <div class="addtocart btnwidth">
        <div
							class="actions-container-for-${fn:escapeXml(component.uid)} <c:if test="${ycommerce:checkIfPickupEnabledForStore() and product.availableForPickup}"> pickup-in-store-available</c:if>">
          <action:actions element="div" parentComponent="${component}" />
        </div>
      </div>

		</ycommerce:testId>
  </div>
</div>