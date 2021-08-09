<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="galleryImages" required="true" type="java.util.List"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>

<!-- BL-927: Created new file for gift card product image -->
<spring:htmlEscape defaultHtmlEscape="true" />
<c:choose>
	<c:when test="${galleryImages == null || galleryImages.size() == 0}">
		<div id="product-slider" class="gift-card-pdp">
			<div class="splide__track">
				<ul class="splide__list">
					<spring:theme code="img.missingProductImage.responsive.product"
						var="imagePath" htmlEscape="false" />
					<c:choose>
						<c:when test="${originalContextPath ne null}">
							<c:choose>
								<c:when test='${fn:startsWith(imagePath, originalContextPath)}'>
									<c:url value="${imagePath}" var="imageUrl" context="/" />
								</c:when>
								<c:otherwise>
									<c:url value="${imagePath}" var="imageUrl"
										context="${originalContextPath}" />
								</c:otherwise>
							</c:choose>
						</c:when>
						<c:otherwise>
							<c:url value="${imagePath}" var="imageUrl" />
						</c:otherwise>
					</c:choose>
					<li class="splide__slide"><img src="${fn:escapeXml(imageUrl)}"></li>
				</ul>
			</div>
		</div>
	</c:when>
	<c:otherwise>
		<div id="product-slider" class="gift-card-pdp">
			<c:forEach items="${galleryImages}" var="container"
				varStatus="varStatus">
				<img src="${fn:escapeXml(container.product.url)}"
					alt="${fn:escapeXml(container.thumbnail.altText)}">
			</c:forEach>
		</div>
	</c:otherwise>
</c:choose>
