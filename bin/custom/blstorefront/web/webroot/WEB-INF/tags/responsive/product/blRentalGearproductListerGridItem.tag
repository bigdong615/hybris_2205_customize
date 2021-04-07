<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="product" required="true" type="de.hybris.platform.commercefacades.product.data.ProductData" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="action" tagdir="/WEB-INF/tags/responsive/action" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="theme" tagdir="/WEB-INF/tags/shared/theme" %>


<spring:htmlEscape defaultHtmlEscape="true" />

<spring:theme code="text.addToCart" var="addToCartText"/>
<c:url value="${product.url}" var="productUrl"/>
<c:set value="${not empty product.potentialPromotions}" var="hasPromotion"/>
<c:set value="image coming soon" var="altText"/>

<div class="col-md-6 col-lg-4">
<div class="card">
<c:if test ="${product.productTagValues ne null}">
<span class="badge badge-limited-stock">${product.productTagValues}</span>
</c:if>

 <span class="bookmark"></span>
  <c:choose>
     <c:when test ="${not empty product.images}">
        <div class="card-slider splide">
           <div class="splide__track">
              <ul class="splide__list">
                 <c:forEach var="mediaLi" items="${product.images}">
                    <c:if test ="${mediaLi.format eq 'product'}">
                       <c:url value="${mediaLi.url}" var="primaryImageUrl" />
                       <c:set value="this is alternate" var="altTextHtml"/>
                       <li class="splide__slide""><img src="${fn:escapeXml(primaryImageUrl)}"/></li>
                    </c:if>
                 </c:forEach>
              </ul>
           </div>
        </div>
     </c:when>
     <c:otherwise>
        <c:set value="/blstorefront/_ui/responsive/theme-bltheme/images/missing_product_EN_300x300.jpg" var="altTextHtml1"/>
        <img src="${fn:escapeXml(altTextHtml1)}" alt="${altTextHtml}" title="${altText}" title="${altText}"/>
     </c:otherwise>
  </c:choose>

 <p class="overline">${product.manufacturer}</p>
 <h6 class="product">
            <c:out escapeXml="false" value="${ycommerce:sanitizeHTML(product.name)}" />
  </h6>
		<ycommerce:testId code="product_wholeProduct">

			<c:if test="${not empty product.potentialPromotions}">
				<div class="promo">
					<c:forEach items="${product.potentialPromotions}" var="promotion">
						${ycommerce:sanitizeHTML(promotion.description)}
					</c:forEach>
				</div>
			</c:if>

			<h6 class="price"><product:productListerItemPrice product="${product}"/><span class="period"> &nbsp;7 day rental</span></h6>
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
		<div class="addtocart">
			<div class="actions-container-for-${fn:escapeXml(component.uid)} <c:if test="${ycommerce:checkIfPickupEnabledForStore() and product.availableForPickup}"> pickup-in-store-available</c:if>">
				<action:actions element="div" parentComponent="${component}"/>
			</div>
		</div>
	</ycommerce:testId>
</div>
</div>