<%@ page trimDirectiveWhitespaces="true" contentType="application/json" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>


<c:set var="productName" value="${fn:escapeXml(product.name)}" />
<c:url value="/cart/updateQuantity" var="cartUpdateFormAction"/>
<c:url value="/cart" var="viewCartUrl"/>
<c:url value="/cart/add" var="addToCartUrl"/>

{"quickOrderErrorData": [
<c:forEach items="${quickOrderErrorData}" var="quickOrderEntry" varStatus="status">
	<c:set var="productCode" value="${fn:escapeXml(quickOrderEntry.productData.code)}" />
	<spring:theme code="${quickOrderEntry.errorMsg}" var="quickOrderEntryErrorMsg" htmlEscape="true"/>
	{
		"sku":		"${ycommerce:encodeJSON(productCode)}",
		"errorMsg": "${ycommerce:encodeJSON(quickOrderEntryErrorMsg)}"
	}<c:if test="${not status.last}">,</c:if>
</c:forEach>
],

"cartAnalyticsData":{"cartCode" : "${ycommerce:encodeJSON(cartCode)}","productPostPrice":"${ycommerce:encodeJSON(entry.basePrice.value)}","productName":"${ycommerce:encodeJSON(productName)}"}
,
"addToCartLayer":"<spring:escapeBody javaScriptEscape="true" htmlEscape="false">
	<spring:htmlEscape defaultHtmlEscape="true">
	<spring:theme code="text.addToCart" var="addToCartText"/>
	<c:url value="/cart" var="cartUrl"/>
	<ycommerce:testId code="addToCartPopup">
	<!-- BL-454 add to cart Modal -->
   <div class="modal-content">
            <div class="modal-header">
              <h5 class="modal-title"><spring:theme code="text.addtocart.popup"/> <i class="cart-check"></i></h5>
              <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
            <div class="modal-body">
              <div class="row">
                  <div class="col-md-2 text-center"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></div>
                  <div class="col-md-7 mt-4"><b>${product.name}</b>
                  <c:if test="${not empty rentalDate.selectedFromDate}">
                    <span class="gray80">${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}</span>
                  </c:if>
                  </div>
                  <div class="col-md-3 mt-4 text-md-end">
                      <b>${entry.basePrice.formattedValue}</b>
                      <form:form id="updateCartForm${entry.entryNumber}" action="${cartUpdateFormAction}" method="post"
                          modelAttribute="updateQuantityForm${entry.entryNumber}" class="js-qty-form${entry.entryNumber}">
                          <input type="hidden" name="entryNumber" value="${entry.entryNumber}" />
                          <input type="hidden" name="productCode" value="${entry.product.code}" />
                          <input type="hidden" name="initialQuantity" value="${entry.quantity}" />
                          <input type="hidden" name="quantity" value="${entry.quantity}" />
                        <spring:theme code="text.quantity"/>
                        <div class="quantity">
                        	<div class="input-group">
                        		<span class="input-group-btn">
                        			<button type="button" class="btn btn-default btn-number"
                        				data-type="minus" data-field="quant[1]">
                        				<span class="glyphicon glyphicon-minus"></span>
                        			</button>
                        		</span> <input type="text" name="quant[1]" class="form-control input-number"
                        			value="1" min="1" max="99"
                        			entryNumber="${entry.entryNumber}"> <span
                        			class="input-group-btn">
                        			<button type="button" class="btn btn-default btn-number"
                        				data-type="plus" data-field="quant[1]">
                        				<span class="glyphicon glyphicon-plus"></span>
                        			</button>
                        		</span>
                        	</div>
                        </div>
                      </form:form>
                  </div>
              </div>
              <hr>
              <!-- BL-455 TODO Additional Gear Slider -->
             <h5 class=" d-md-block"><spring:theme code="text.addtocart.dont.forget"/></h5>
              <c:choose>
                            	<c:when test="${not empty productReferences and productsLimit > 0}">
                            	<div id="addToCart-gear-sliders" class="splide mt-4">
                                		<div class="splide__track">
                                              <ul class="splide__list">
                                                            <c:forEach end="${productsLimit}" items="${productReferences}" var="productReference" varStatus="loopindex">
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
                                                                                                                                                               <c:when test="${productReference.target.isDiscontinued}">
                                                                                                                                                                    <span class="bookmark" disabled="disabled"></span>
                                                                                                                                                               </c:when>
                                                                                                                                                                <c:when test="${productReference.target.isBookMarked}">
                                                                                                                                                                  <span class="bookmark set js-add-to-wishlist" id="card-${loopindex.index}" data-product-code="${productReference.target.code}"
                                                                                                                                                                     data-bookmark-value="${productReference.target.isBookMarked}">${productReference.target.isBookMarked}</span>
                                                                                                                                                                </c:when>
                                                                                                                                                                <c:otherwise>
                                                                                                                                                                  <span class="bookmark js-add-to-wishlist" id="card-${loopindex.index}" data-product-code="${productReference.target.code}"
                                                                                                                                                                     data-bookmark-value="${productReference.target.isBookMarked}">${productReference.target.isBookMarked}</span>
                                                                                                                                                                </c:otherwise>
                                                                                                                                                            </c:choose>
                                                                             </form>
                                                                               <div class="card-sliders splide">
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
                                                                               <h6 class="price">${productReference.target.price.formattedValue}</h6>
                                                                              
                                                                                <c:choose>
                                                                                      <c:when test="${productReference.target.isDiscontinued || productReference.target.stock.stockLevelStatus.code eq 'outOfStock'}">
                                                                                            <button type="submit" class="btn btn-primary" disabled="disabled"><spring:theme code="pdp.rental.product.recommendation.section.addtorental.text"/> </button>
                                                                                      </c:when>
                                                                                      <c:when test="${productReference.target.isUpcoming}">
                                                                                           <a href="#" class="btn btn-primary"><spring:theme code="pdp.rental.product.recommendation.section.notifyme.text" /></a>
                                                                                      </c:when>
                                                                                       <c:otherwise>
                                                                                       <div class="page-loader-new-layout">
                                                                                                  <img src="${themeResourcePath}/assets/bl-loader.gif" alt="Loading.." title="Loading.." id="new_loading_Img">
                                                                                             </div>
                                                                                            <form class="add_to_cart_form" action="${addToCartUrl}" method="post">
                                                                                                <button type="button" class="btn btn-primary btn-block js-add-to-cart-popup" id="modalCard-${loopindex.index}" data-bs-toggle="modal"
                                                                                                   data-bs-target="#addToCart" data-product-code="${productReference.target.code}">
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
                                        </div>
                                	</c:when>

                            	<c:otherwise>
                            		<component:emptyComponent />
                            	</c:otherwise>
              </c:choose>
            <div class="modal-footer">
                <a href="#" class="btn btn-outline" data-bs-dismiss="modal"><spring:theme code="text.popup.button.continue"/></a>
                <a href="${viewCartUrl}" class="btn btn-primary"><spring:theme code="text.popup.button.viewcart"/></a>
            </div>
   </div>

  </ycommerce:testId>                
	</spring:htmlEscape>
</spring:escapeBody>"
}



