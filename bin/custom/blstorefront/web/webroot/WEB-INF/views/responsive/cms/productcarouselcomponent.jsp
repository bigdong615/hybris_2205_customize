<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="component" tagdir="/WEB-INF/tags/shared/component" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:url value="/cart/add" var="addToCartUrl"/>

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
                                	<c:when test="${product.isNew}">
                                		<span class="badge badge-new"><spring:theme code="text.product.tile.flag.new" /></span>
                                	</c:when>
                                	<c:otherwise>
                                	<!-- TO-DO Need to add Stock related flag once the stock related changes is implemented --> 
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
								<p class="overline">${product.manufacturer}</p>
								<c:url var="rentalPDPUrl" value="/rent/product/${product.code}"/>
								<h6 class="product"><a href="${rentalPDPUrl}"><c:out escapeXml="false" value="${ycommerce:sanitizeHTML(product.name)}" /></a></h6>
                                <h6 class="price"><product:productListerItemPrice product="${product}"/> <span class="period"><spring:theme code="text.product.tile.rental.days" arguments="7"/></span></h6>
                                <c:choose>
                                	<c:when test="${product.isDiscontinued}">
                                	<!-- TO-DO : Need to add Add to rental button with disabled action-->
                                		<a href="#" class="btn btn-outline btn-disabled">Add to Rental</a>
                                	</c:when>
                                	<c:when test="${product.isUpcoming}">
                                	<!-- TO-DO : Need to add Notify Me button -->
                                		<a href="#" class="btn btn-primary">Notify Me</a>
                                	</c:when>
                                	<c:otherwise>
                                	<!-- TO-DO : Need to add Add to rental button -->
                                	<form class="add_to_cart_form" action="${addToCartUrl}" method="post">
                                       <input type="hidden" name="productCodePost" value="${fn:escapeXml(product.code)}"/>
                                       <input type="hidden" name="productNamePost" value="${fn:escapeXml(product.name)}"/>
                                       <input type="hidden" name="productPostPrice" value="${fn:escapeXml(product.price.value)}"/>
                                       <input type="hidden" maxlength="3" size="1" id="qty" name="qty" class="qty js-qty-selector-input" value="1">
                                		   <button id="addToCartButton" type="submit" class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#addToCart">
                                       <spring:theme code="text.add.to.rental" />
                                  </form>
                                	</c:otherwise>
                                </c:choose>
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

