<%@ page trimDirectiveWhitespaces="true" contentType="application/json" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<c:url value="/cart" var="viewCartUrl"/>
{
"addToCartLayer":"<spring:escapeBody javaScriptEscape="true" htmlEscape="false">
	<spring:htmlEscape defaultHtmlEscape="true">
	<spring:theme code="text.addToCart" var="addToCartText"/>
	<c:url value="/cart" var="cartUrl"/>
	<ycommerce:testId code="addToCartPopup">
           <div class="modal-content">
           <div class="modal-header">
              <h5 class="modal-title"><spring:theme code="text.addtocart.popup"/> <i class="cart-check"></i></h5>
              <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
           
             <div class="modal-body">
                <div class="row">
                  <div class="col-md-2 text-center"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></div>
                  <div class="col-md-7 mt-4"><b>${product.name}++++++++++</b></div>
                  </div>
           </div>
           </div>
  </ycommerce:testId>
	</spring:htmlEscape>
</spring:escapeBody>",
"addToCartGiftCardLayer":"<spring:escapeBody javaScriptEscape="true" htmlEscape="false">
	<spring:htmlEscape defaultHtmlEscape="true">
	<spring:theme code="text.addToCart" var="addToCartText"/>
	<c:url value="/cart" var="cartUrl"/>
	<ycommerce:testId code="addToCartPopup">
           <div class="modal-content">
           <div class="modal-header">
              <h5 class="modal-title"><spring:theme code="text.addtocart.popup"/> <i class="cart-check"></i></h5>
              <button type="button" id="closeGiftCardModal" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
           
             <div class="modal-body">
                <div class="row">
                  <div class="col-md-2 text-center"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></div>
                  <div class="col-md-7 mt-4"><b>${product.name}</b></div>
                  
                  <div class="col-md-3 mt-4 text-md-end">
                      <b>${entry.basePrice.formattedValue}</b>
                      </div>
                      </div>
           </div>
              <div class="modal-footer">
                <a href="#" id="cancelGiftCardModal" class="btn btn-outline" data-bs-dismiss="modal"><spring:theme code="text.popup.button.continue"/></a>
                <a href="${viewCartUrl}" class="btn btn-primary"><spring:theme code="text.popup.button.viewcart"/></a>
            </div>
           </div>
                    
           
  </ycommerce:testId>
	</spring:htmlEscape>
</spring:escapeBody>"
}