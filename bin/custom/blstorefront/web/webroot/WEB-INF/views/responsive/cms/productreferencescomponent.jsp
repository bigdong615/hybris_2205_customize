<%@ page trimDirectiveWhitespaces="true"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="component" tagdir="/WEB-INF/tags/shared/component"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:url value="/cart/add" var="addToCartUrl"/>
<c:url value="/wishlist/add" var="addWishList"/>

<c:choose>
	<c:when test="${not empty productReferences and component.maximumNumberProducts > 0}">
    		<div class="splide__track">
                  <ul class="splide__list">
                                <c:forEach end="${component.maximumNumberProducts}" items="${productReferences}" var="productReference">
            				        	<li class="splide__slide">
                                            <div class="card">
                                         <c:choose>

                                                  <c:when test="${productReference.target.stock.stockLevelStatus.code eq 'lowStock'}">
                                            				<span class="badge badge-limited-stock"><spring:theme
                                            						code="text.product.tile.flag.only.left"
                                            						arguments="${productReference.target.stock.stockLevel}" /></span>
                                            			</c:when>
                                            			<c:when test="${productReference.target.stock.stockLevelStatus.code eq 'outOfStock'}">
                                            				<span class="badge badge-out-of-stock"><spring:theme
                                            						code="text.product.tile.flag.outOfStock"
                                            						arguments="${productReference.target.stock.stockLevel}" /></span>
                                            			</c:when>
                                                  <c:otherwise>
                                                      <c:if test ="${productReference.target.productTagValues ne null}">
                                                             <span class="badge badge-new">${productReference.target.productTagValues}</span>
                                                      </c:if>
                                                  </c:otherwise>
                                         </c:choose>
                                               <form class="add_to_wishList_form" action="${addWishList}" method="post" id="js-wishlist-form">
                                               <input type="hidden" name="productCodePost" id="productCodePost" value="${productReference.target.code}">
                                               <c:choose>
                                                  <c:when test="${productReference.target.isBookMarked}">
                                                   <span class="bookmark bookmark-checked js-add-to-wishlist bookmarkicons" data-product-code="${productReference.target.code}"
                                                    data-bookmark-value="${productReference.target.isBookMarked}">${productReference.target.isBookMarked}</span>
                                                  </c:when>
                                                  <c:otherwise>
                                                   <span class="bookmark js-add-to-wishlist bookmarkicons" data-product-code="${productReference.target.code}"
                                                   data-bookmark-value="${productReference.target.isBookMarked}">${productReference.target.isBookMarked}</span>
                                                  </c:otherwise>
                                               </c:choose>
                                               </form>
                                                   <div class="card-slider splide">
                                                     <div class="splide__track">
                                                       <ul class="splide__list">
                                                       <c:forEach items="${productReference.target.images}" var="productImagePdp">
                                                          <c:if test="${productImagePdp.format eq 'product' and productImagePdp.imageType eq 'GALLERY'}">
                                                                <c:url value="${productImagePdp.url}" var="primaryImagePdpUrl" />
                                                                <c:set value="this is alternate" var="altTextHtml"/>
                                                                <!--BL-534: added <a> tag-->
                                                                    <li class="splide__slide">
                                                                      <c:url var="rentalPDPUrl" value="/rent/product/${productReference.target.code}"/>
                                                                       <a href ="${rentalPDPUrl}"><img src="${primaryImagePdpUrl}"></a</li> 
                                                          </c:if>
                                                       </c:forEach>
                                                       </ul>
                                                     </div>
                                                   </div>
                                                   <p class="overline"><a href="#">${fn:escapeXml(productReference.target.manufacturer)}</a></p>
                                                   <c:url var="rentalPDPUrl" value="/rent/product/${productReference.target.code}"/>
                                                   <h6 class="product"><a href="${rentalPDPUrl}">${fn:escapeXml(productReference.target.name)}</a></h6>
                                                   <!-- BL-483 : Getting price as per the selection on rental days or else default price for seven rentals days will be returned -->
                                                   <h6 class="price"><product:productListerItemPrice product="${productReference.target}"/>
                                                   <c:choose>
                                                        <c:when test="${rentalDate.selectedFromDate ne null and rentalDate.selectedToDate ne null}">
                                                            <span class="period">${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}</span></h6>
                                                        </c:when>
                                                        <c:otherwise>
                                                             <span class="period">${rentalDate.numberOfDays}&nbsp;<spring:theme code="pdp.rental.product.recommendation.section.days.rental.text"/></span></h6>
                                                        </c:otherwise>
                                                   </c:choose>
                                                    <c:choose>
                                                          <c:when test="${productReference.target.isDiscontinued || productReference.target.stock.stockLevelStatus.code eq 'outOfStock'}">
                                                                <button type="submit" class="btn btn-primary" disabled="disabled"><spring:theme code="pdp.rental.product.recommendation.section.addtorental.text"/> </button>
                                                          </c:when>
                                                          <c:when test="${productReference.target.isUpcoming}">
                                                               <a href="#" class="btn btn-primary"><spring:theme code="pdp.rental.product.recommendation.section.notifyme.text" /></a>
                                                          </c:when>
                                                           <c:otherwise>
                                                                <form class="add_to_cart_form" action="${addToCartUrl}" method="post">
                                                                    <button type="button" class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 js-add-to-cart" data-bs-toggle="modal"
                                                                       data-bs-target="#addToCart" data-product-code="${product.code}">
                                                                      <spring:theme code="pdp.rental.product.recommendation.section.addtorental.text" />
                                                                    </button>
                                                                </form>
                                                            </c:otherwise>
                                                    </c:choose>
                                            </div>
                                        </li>
                                </c:forEach>
                  </ul>
            </div>
    	</c:when>

	<c:otherwise>
		<component:emptyComponent />
	</c:otherwise>
</c:choose>