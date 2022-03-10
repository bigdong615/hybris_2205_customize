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
                    <span class="gray80">${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate} (${rentalDate.numberOfDays} days)</span>
                  </c:if>
                  </div>
                  <div class="col-md-3 mt-4 text-md-end">
                      <c:choose>
                          <c:when test="${replacementOrder == 'true'}">
                              <b>$0.00</b>
                          </c:when>
                          <c:otherwise>
                              <b>${entry.basePrice.formattedValue}</b>
                          </c:otherwise>
                      </c:choose>
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
              <h5 class="d-none d-md-block"><spring:theme code="text.addtocart.dont.forget"/></h5>
              <div class="row d-none d-md-flex mt-4">
                  <div class="col-md-4">
                      <div class="card">
                          <span class="badge badge-new">New</span>
                          <span class="bookmark"></span>
                          <img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                          <p class="overline"><a href="#">Canon</a></p>
                          <h3 class="product"><a href="#">BG-10 Battery Grip</a></h6>
                          <h6 class="price">$44</h6>
                          <a href="#" class="btn btn-primary">Add to Rental</a>
                      </div>
                  </div>
                  <div class="col-md-4">
                      <div class="card">
                          <span class="badge badge-new">New</span>
                          <span class="bookmark"></span>
                          <img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg">
                          <p class="overline"><a href="#">Canon</a></p>
                          <h3 class="product"><a href="#">BG-10 Battery Grip</a></h6>
                          <h6 class="price">$44</h6>
                          <a href="#" class="btn btn-primary">Add to Rental</a>
                      </div>
                  </div>
                  <div class="col-md-4">
                      <div class="card">
                          <span class="badge badge-new">New</span>
                          <span class="bookmark"></span>
                          <img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg">
                          <p class="overline"><a href="#">Canon</a></p>
                          <h3 class="product"><a href="#">BG-10 Battery Grip</a></h6>
                          <h6 class="price">$44</h6>
                          <a href="#" class="btn btn-primary">Add to Rental</a>
                      </div>
                  </div>
              </div>
            </div>
            <div class="modal-footer">
                <a href="#" class="btn btn-outline" data-bs-dismiss="modal"><spring:theme code="text.popup.button.continue"/></a>
                <a href="${viewCartUrl}" class="btn btn-primary"><spring:theme code="text.popup.button.viewcart"/></a>
            </div>
   </div>

  </ycommerce:testId>
	</spring:htmlEscape>
</spring:escapeBody>"
}



