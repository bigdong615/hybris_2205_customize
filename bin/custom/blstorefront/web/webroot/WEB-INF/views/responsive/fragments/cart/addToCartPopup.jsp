<%@ page trimDirectiveWhitespaces="true" contentType="application/json" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<c:set var="productName" value="${fn:escapeXml(product.name)}" />

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
	<!-- BL-456 add to cart Modal -->
      <div class="modal fade show" style="display: block; padding-right: 17px;" id="addToCart" tabindex="-1" aria-hidden="true">
        <div class="modal-dialog modal-dialog-centered modal-lg">
          <div class="modal-content">
            <div class="modal-header">
              <h5 class="modal-title">Added to Cart <i class="cart-check"></i></h5>
              <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
            <div class="modal-body">
              <div class="row">
                  <div class="col-md-2 text-center"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></div>
                  <div class="col-md-7 mt-4"><b>${product.name}</b><span class="gray80">Rental Dates</span></div>
                  <div class="col-md-3 mt-4 text-md-end">
                      <b>${product.price.formattedValue}$XX</b>
                      Qty
                      <select class="mt-3">
                          <option>1</option>
                          <option>2</option>
                          <option>3</option>
                          <option>4</option>
                          <option>5</option>
                          <option>6</option>
                          <option>7</option>
                          <option>8</option>
                      </select>
                  </div>
              </div>
              <hr>
              <!-- Additional Gear Slider -->
              <h5 class="d-none d-md-block">Dont forget</h5>
              <div class="row d-none d-md-flex mt-4">
                  <div class="col-md-4">
                      <div class="card">
                          <span class="badge badge-new">New</span>
                          <span class="bookmark"></span>
                          <img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                          <p class="overline"><a href="#">Canon</a></p>
                          <h6 class="product"><a href="#">BG-10 Battery Grip</a></h6>
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
                          <h6 class="product"><a href="#">BG-10 Battery Grip</a></h6>
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
                          <h6 class="product"><a href="#">BG-10 Battery Grip</a></h6>
                          <h6 class="price">$44</h6>
                          <a href="#" class="btn btn-primary">Add to Rental</a>
                      </div>
                  </div>
              </div>
            </div>
            <div class="modal-footer">
                <a href="#" class="btn btn-outline js-mini-cart-close-button"><spring:theme code="text.popup.button.continue"/></a>
                <a href="/blstorefront/bl/en/cart" class="btn btn-primary"><spring:theme code="text.popup.button.viewcart"/></a>
            </div>
          </div>
        </div>
      </div>

	</ycommerce:testId>
	</spring:htmlEscape>
</spring:escapeBody>"
}



