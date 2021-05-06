<%@ page trimDirectiveWhitespaces="true" contentType="application/json" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>

<c:set var="productName" value="${fn:escapeXml(product.name)}" />
<c:url value="/cart/updateQuantity" var="cartUpdateFormAction"/>
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
                  <div class="col-md-7 mt-4"><b>${product.name}</b><span class="gray80"><spring:theme code="pdp.rental.dates.label.text"/></span></div>
                  <div class="col-md-3 mt-4 text-md-end">
                      <b>${entry.basePrice.formattedValue}</b>
                      <form:form id="updateCartForm${entry.entryNumber}" action="${cartUpdateFormAction}" method="post"
                          modelAttribute="updateQuantityForm${entry.entryNumber}" class="js-qty-form${entry.entryNumber}">
                          <input type="hidden" name="entryNumber" value="${entry.entryNumber}" />
                          <input type="hidden" name="productCode" value="${entry.product.code}" />
                          <input type="hidden" name="initialQuantity" value="${entry.quantity}" />
                          <input type="hidden" name="quantity" value="${entry.quantity}" />
                        <spring:theme code="text.quantity"/>
                        <select class="mt-3 select js-select js-component-init js-update-quantity" id="shopping-cart-qty_${entry.entryNumber}" name="shopping-cart-qty">
                           <c:forEach var="item" begin="1" end="10">
                             <option value="${item}" ${item == quantity ? 'selected="selected"' : ''}>${item}</option>
                           </c:forEach>
                       </select>
                      </form:form>
                  </div>
              </div>
              <hr>
              <!-- BL-455 TODO Additional Gear Slider -->
              <h5 class="d-none d-md-block">Dont forget</h5>
              <div class="row d-none d-md-flex mt-4">
                  <div class="col-md-4">
                       <c:forEach  items="${productReferences}" var="productReference">
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
                                                            <span class="bookmark"></span>
                                                            <c:set value="true" var="continueLoop"/>
                                                            <c:forEach items="${productReference.target.images}" var="productImagePdp">
                                                              <c:if test="${productImagePdp.format eq 'product' and productImagePdp.imageType eq 'GALLERY' and continueLoop}">
                                                                <c:url value="${productImagePdp.url}" var="primaryImagePdpUrl" />
                                                                <c:set value="this is alternate" var="altTextHtml"/>
                                                                <img src="${primaryImagePdpUrl}">
                                                                 <c:set value="false" var="continueLoop"/>
                                                              </c:if>
                                                             </c:forEach>
                                                            <p class="overline"><a href="#">${fn:escapeXml(productReference.target.manufacturer)}</a></p>
                                                            <h6 class="product"><a href="#">${fn:escapeXml(productReference.target.name)}</a></h6>
                                                            <h6 class="price"><format:price priceData="${productReference.target.price}"/></h6>
                                                           <form class="add_to_cart_form" action="${addToCartUrl}" method="post">
                                                                 <button type="button"
                                                                      class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 js-add-to-cart1 SeeMore2" data-bs-toggle="modal" data-bs-target="#addToCart"
                                                                      data-popup="dataToPopUp" data-product-code="${productReference.target.code}">
                                                           <spring:theme code="basket.add.to.rental.cart.button.text" />
                                                           </button>
                                                           </form>
                                                        </div>
                                                    </c:forEach>
                  </div>
              </div>
            </div>
            <div class="modal-footer">
                <a href="#" class="btn btn-outline" data-bs-dismiss="modal"><spring:theme code="text.popup.button.continue"/></a>
                <a href="/blstorefront/bl/en/cart" class="btn btn-primary"><spring:theme code="text.popup.button.viewcart"/></a>
            </div>
   </div>

  </ycommerce:testId>
	</spring:htmlEscape>
</spring:escapeBody>"
}



