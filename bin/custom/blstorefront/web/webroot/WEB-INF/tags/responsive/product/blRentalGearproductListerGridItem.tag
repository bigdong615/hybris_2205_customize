<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="product" required="true" type="de.hybris.platform.commercefacades.product.data.ProductData" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="action" tagdir="/WEB-INF/tags/responsive/action" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="theme" tagdir="/WEB-INF/tags/shared/theme" %>
<%@ taglib prefix="sec"	uri="http://www.springframework.org/security/tags"%>


<spring:htmlEscape defaultHtmlEscape="true" />

<spring:theme code="text.addToCart" var="addToCartText"/>
<c:url value="${product.url}" var="productUrl"/>
<c:set value="${not empty product.potentialPromotions}" var="hasPromotion"/>
<c:set value="image coming soon" var="altText"/>

<div class="col-md-6 col-lg-4">
<div class="card">
<!-- BL-926: Added condition for Gift Card as per requirement --> 
 <c:if test="${product.code ne 'bl_giftcard'}">
		<c:choose>
			<c:when test="${product.stock.stockLevelStatus.code eq 'lowStock'}">
				<span class="badge badge-limited-stock"><spring:theme
						code="text.product.tile.flag.only.left"
						arguments="${product.stock.stockLevel}" /></span>
			</c:when>
			<c:when test="${product.stock.stockLevelStatus.code eq 'outOfStock'}">
				<span class="badge badge-out-of-stock"><spring:theme
						code="text.product.tile.flag.outOfStock"
						arguments="${product.stock.stockLevel}" /></span>
			</c:when>
			<c:otherwise>
				<c:if test="${product.productTagValues ne null}">
				<c:choose>
				<c:when test="${fn:containsIgnoreCase(product.productTagValues, 'New') || fn:containsIgnoreCase(product.productTagValues, 'Staff Pick') ||  fn:containsIgnoreCase(product.productTagValues, 'Great Value')}">
					<span class="badge badge-new">${product.productTagValues}</span>
				</c:when>
				<c:otherwise>
				  <span class="badge badge-limited-stock">${product.productTagValues}</span>
				</c:otherwise>
				</c:choose>
				</c:if>
			</c:otherwise>
		</c:choose>
		</c:if>
		<!-- BL-926: Added condition for Gift Card as per requirement --> 
   <c:if test="${product.code ne 'bl_giftcard'}">
    <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
		<form class="add_to_wishList_form" action="${addWishList}" method="post" id="js-wishlist-form">
                <input type="hidden" name="productCodePost" id="productCodePost" value="${product.code}">
                <c:choose>
                   <c:when test="${product.isBookMarked}">
                    <span class="bookmark set js-add-to-wishlist bookmarkicons" data-product-code="${product.code}"
                     data-bookmark-value="${product.isBookMarked}"></span>
                   </c:when>
                   <c:otherwise>
                    <span class="bookmark js-add-to-wishlist bookmarkicons"  data-product-code="${product.code}"
                    data-bookmark-value="${product.isBookMarked}"></span>
                   </c:otherwise>
                </c:choose>
    </form>
    </sec:authorize>
    </c:if>
    <!-- BL-926: Added condition for Gift Card as per requirement --> 
		<c:choose>
			<c:when test="${product.code eq 'bl_giftcard'}">
				<div class="gift-card-plp">
					<c:choose>
						<c:when test="${not empty product.images}">
							<c:forEach var="mediaLi" items="${product.images}">
								<c:if test="${mediaLi.format eq 'product'}">
									<c:url value="${mediaLi.url}" var="primaryImageUrl" />
									<c:set value="this is alternate" var="altTextHtml" />
									<li class="splide__slide""><c:url var="rentUrl"
											value="/rent/product/${product.code}" /> <a
										href="${rentUrl}"> <img
											src="${fn:escapeXml(primaryImageUrl)}" />
									</a></li>
								</c:if>
							</c:forEach>
						</c:when>
						<c:otherwise>
							<c:set
								value="/blstorefront/_ui/responsive/theme-bltheme/images/missing_product_EN_300x300.jpg"
								var="altTextHtml1" />
							<img src="${fn:escapeXml(altTextHtml1)}" alt="${altTextHtml}"
								title="${altText}" title="${altText}" />
						</c:otherwise>
					</c:choose>
				</div>
			</c:when>
			<c:otherwise>
				<c:choose>
					<c:when test="${not empty product.images}">
						<div class="card-slider splide">
							<div class="splide__track">
								<ul class="splide__list">
									<c:forEach var="mediaLi" items="${product.images}">
										<c:if test="${mediaLi.format eq 'product'}">
											<c:url value="${mediaLi.url}" var="primaryImageUrl" />
											<c:set value="this is alternate" var="altTextHtml" />
											<li class="splide__slide"">
												<!-- BL-534--> <c:url var="rentUrl"
													value="/rent/product/${product.code}" /> <a
												href="${rentUrl}"> <img
													src="${fn:escapeXml(primaryImageUrl)}" />
											</a>
											</li>
										</c:if>
									</c:forEach>
								</ul>
							</div>
						</div>
					</c:when>
					<c:otherwise>
						<c:set
							value="/blstorefront/_ui/responsive/theme-bltheme/images/missing_product_EN_300x300.jpg"
							var="altTextHtml1" />
						<img src="${fn:escapeXml(altTextHtml1)}" alt="${altTextHtml}"
							title="${altText}" title="${altText}" />
					</c:otherwise>
				</c:choose>
			</c:otherwise>
		</c:choose>
<!-- BL-926: Added condition for Gift Card as per requirement --> 
		<c:if test="${product.code ne 'bl_giftcard'}">
			<p class="overline">${product.manufacturer}</p>
		</c:if>
		<h6 class="product">
          <c:url var="rentUrl" value="/rent/product/${product.code}"/>
           <a href="${rentUrl}" role="button"> <c:out escapeXml="false" value="${ycommerce:sanitizeHTML(product.name)}" /> </a>
       </h6>
		<ycommerce:testId code="product_wholeProduct">

			<c:if test="${not empty product.potentialPromotions}">
				<div class="promo">
					<c:forEach items="${product.potentialPromotions}" var="promotion">
						${ycommerce:sanitizeHTML(promotion.description)}
					</c:forEach>
				</div>
			</c:if>
			<h6 class="price">
			<!-- BL-926: Added condition for Gift Card as per requirement --> 
				<c:choose>
					<c:when test="${product.code eq 'bl_giftcard'}">
                          <spring:theme code="slp.giftcard.price" />
					</c:when>
					<c:otherwise>
						<product:productListerItemPrice product="${product}" />
						<c:choose>
							<c:when
								test="${rentalDate.selectedFromDate ne null and rentalDate.selectedToDate ne null}">
								<span class="period">${rentalDate.selectedFromDate} -
									${rentalDate.selectedToDate}</span>
							</c:when>
							<c:otherwise>
								<span class="period">${rentalDate.numberOfDays}&nbsp;<spring:theme
										code="pdp.rental.product.recommendation.section.days.rental.text" /></span>
							</c:otherwise>
						</c:choose>
					</c:otherwise>
				</c:choose>
			</h6>
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
		<c:url var="rentUrl" value="/rent/product/${product.code}"/>
		<!-- BL-926: Added condition for Gift Card as per requirement --> 
			<c:choose>
				<c:when test="${product.code eq 'bl_giftcard'}">
					<a href="${rentUrl}" class="btn btn-primary"><spring:theme
							code="basket.add.to.basket" /></a>
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