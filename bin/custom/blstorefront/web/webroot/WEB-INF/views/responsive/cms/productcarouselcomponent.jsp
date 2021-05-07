<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="component" tagdir="/WEB-INF/tags/shared/component" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:choose>
	<c:when test="${not empty productData}">
		<h5>${title}</h5>
		<div id="gear-slider" class="splide mt-4">
			<div class="splide__track">
				<ul class="splide__list">
					<c:forEach items="${productData}" var="product">
						<li class="splide__slide">
							<div class="card">
							<c:choose>
									<c:when	test="${product.stock.stockLevelStatus.code eq 'lowStock'}">
										<span class="badge badge-limited-stock"><spring:theme
												code="text.product.tile.flag.only.left"
												arguments="${product.stock.stockLevel}" /></span>
									</c:when>
									<c:when	test="${product.stock.stockLevelStatus.code eq 'outOfStock'}">
										<span class="badge badge-out-of-stock"><spring:theme
												code="text.product.tile.flag.outOfStock"
												arguments="${product.stock.stockLevel}" /></span>
									</c:when>
									<c:otherwise>
										<c:if test="${product.productTagValues ne null}">
											<span class="badge badge-new">${product.productTagValues}</span>
										</c:if>
									</c:otherwise>
								</c:choose>
								
								<span class="bookmark"></span>
								<div class="card-slider splide">
									<div class="splide__track">
										<ul class="splide__list">
											<c:forEach items="${product.images}" var="productImage">
												<c:if test="${productImage.format eq 'product' and productImage.imageType eq 'GALLERY'}">
													<c:url value="${productImage.url}" var="primaryImageUrl" />
	                       							<c:set value="this is alternate" var="altTextHtml"/>
													<li class="splide__slide"><img src="${primaryImageUrl}"></li>
												</c:if>
											</c:forEach>
										</ul>
									</div>
								</div>
								<p class="overline"><a href="#">${product.manufacturer}</a></p>
								<c:url var="rentalPDPUrl" value="/rent/product/${product.code}"/>
								<h6 class="product"><a href="${rentalPDPUrl}"><c:out escapeXml="false" value="${ycommerce:sanitizeHTML(product.name)}" /></a></h6>
                                <h6 class="price"><format:price priceData="${product.price}"/> <span class="period">
                                <c:choose>
                                	<c:when test="${not empty rentalDate.selectedFromDate and not empty rentalDate.selectedToDate}">
                                	${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}
                                	</c:when>
                                	<c:otherwise>
                                		<spring:theme code="text.product.tile.rental.days" arguments="${rentalDate.numberOfDays}"/>    
                                	</c:otherwise>
                                </c:choose>              
                                </span></h6>
                                <cart:blRentalGearAddToRental productData="${product}"/>
							</div>
						</li>
					</c:forEach>
				</ul>
			</div>
		</div>
	</c:when>
	<c:otherwise>
		<component:emptyComponent/>
	</c:otherwise>
</c:choose>

