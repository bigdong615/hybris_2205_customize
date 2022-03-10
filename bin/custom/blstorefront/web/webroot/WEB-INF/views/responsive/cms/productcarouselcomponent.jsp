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
<%@ taglib prefix="sec"	uri="http://www.springframework.org/security/tags"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:choose>
	<c:when test="${not empty productData}">
		<h5>${title}</h5>
		<div id="gear-slider" class="splide mt-4">
			<div class="splide__track">
				<ul class="splide__list">
					<c:forEach items="${productData}" var="product" varStatus="loopindex">
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
								 <c:if test="${product.isDiscontinued ne 'true' }">
							<sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
							    <form class="add_to_wishList_form" action="${addWishList}" method="post" id="js-wishlist-form">
                         <input type="hidden" name="productCodePost" id="productCodePost" value="${product.code}">
                         <c:choose>
              		        <c:when test="${product.isBookMarked}">
                             <span class="bookmark set js-add-to-wishlist" id="card-${loopindex.index}" data-product-code="${product.code}"
                              data-bookmark-value="${product.isBookMarked}"></span>
                            </c:when>
                            <c:otherwise>
                             <span class="bookmark js-add-to-wishlist" id="card-${loopindex.index}" data-product-code="${product.code}"
                             data-bookmark-value="${product.isBookMarked}"></span>
                            </c:otherwise>
                         </c:choose>
                  </form>
               </sec:authorize>
               </c:if>
								<div class="card-slider splide">
									<div class="splide__track">
										<ul class="splide__list">
											<c:forEach items="${product.images}" var="productImage">
												<c:if test="${productImage.format eq 'product' and productImage.imageType eq 'GALLERY'}">
													<c:url value="${productImage.url}" var="primaryImageUrl" />
	                       							<c:set value="this is alternate" var="altTextHtml"/>
	                       							<c:url var="rentUrl" value="/rent/product/${product.code}"/>
													<li class="splide__slide"><a href="${rentUrl}" class="js-pdplinkUrl" data-productCode="${product.code}" data-brand="${product.manufacturer}"
                               data-productName="${ycommerce:sanitizeHTML(product.name)}" data-productType="rental">
													<img src="${primaryImageUrl}"></a></li>
												</c:if>
											</c:forEach>
										</ul>
									</div>
								</div>
								<p class="overline"><a href="#">${product.manufacturer}</a></p>
								<c:url var="rentalPDPUrl" value="/rent/product/${product.code}"/>
								<h3 class="product"><a href="${rentalPDPUrl}" class="js-pdplinkUrl" data-productCode="${product.code}"
                 data-brand="${product.manufacturer}" data-productName="${ycommerce:sanitizeHTML(product.name)}" data-productType="rental">
								<c:out escapeXml="false" value="${ycommerce:sanitizeHTML(product.name)}" /></a></h3>
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

