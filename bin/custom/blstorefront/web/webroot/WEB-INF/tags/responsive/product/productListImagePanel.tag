<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="product" required="true"
	type="de.hybris.platform.commercefacades.product.data.ProductData"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ attribute name="productType" required="false"
	type="java.lang.String"%>
	<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<!-- BL-926: Added new file for condition as per Gift Card requirement --> 	
<c:choose>
	<c:when test="${productType eq 'RentalGearProduct' }">
		<c:set value="/rent/product" var="productTypeUrl" />
	</c:when>
	<c:otherwise>
		<c:set value="/buy/product" var="productTypeUrl" />
	</c:otherwise>

</c:choose>
<c:choose>
	<c:when test="${product.productType eq 'GIFTCARD'}">
		<div class="gift-card-plp">
			<c:choose>
				<c:when test="${not empty product.images}">
					<c:forEach var="mediaLi" items="${product.images}">
						<c:if test="${mediaLi.format eq 'product'}">
							<c:url value="${mediaLi.url}" var="primaryImageUrl" /> 
							<c:set value="this is alternate" var="altTextHtml" />
							<li class="splide__slide""><c:url var="giftCardUrl" value="${productTypeUrl}/${product.code}" />
									 <a href="${giftCardUrl}"  onclick="return productClickTealiumEvent('${product.code}');" class="js-pdplinkUrl" data-productCode="${product.code}" data-brand="gift cart"
                      data-productName="${ycommerce:sanitizeHTML(product.displayName)}" data-productType="Gift Cart"> <img
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
										<!-- BL-534--> <c:url var="rentUrl" value="${productTypeUrl}/${product.code}" />
										<a href="${rentUrl}" class="js-pdplinkUrl" onclick="return productClickTealiumEvent('${product.code}');" data-productCode="${product.code}" data-brand="${product.manufacturer}"
                          data-productName="${product.displayName}" data-productType="${productType eq 'RentalGearProduct' ? 'rental' : 'used gear' }">
										<img	src="${fn:escapeXml(primaryImageUrl)}" />
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
