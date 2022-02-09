<%@ page trimDirectiveWhitespaces="true" contentType="application/json" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>

{
"multipleGiftCardWarningLayer":"<spring:escapeBody javaScriptEscape="true" htmlEscape="false">
	<spring:htmlEscape defaultHtmlEscape="true">
	<spring:theme code="text.addToCart" var="addToCartText"/>
	<c:url value="/cart" var="cartUrl"/>
	<ycommerce:testId code="addToCartPopup">
           <div class="modal-content">
             <div class="modal-header">
               <h5 class="modal-title"><spring:theme code="shipping.interception.change.date.warning.wait"/></h5>
               <button id="closeMultipleGiftCardError" type="button" class="btn-close" aria-label="Close" data-bs-dismiss="modal"></button>
             </div>
             <div class="modal-body">
                 <p class="body14"><spring:theme code="giftcard.PurchaseForm.multiple.addtocart.popup"/></p>
                 <%-- 
                 //commented code , It can be used in future
                 <a href="#" class="btn btn-primary btn-block my-4" id="mixedProductInterception"><spring:theme code="shipping.interception.change.date.warning.continue"/></a>
                 <p class="text-center mb-0"><a href="#" id="cancelUsedCartModal" class="lightteal" aria-label="Close" data-bs-dismiss="modal"><spring:theme code="shipping.interception.change.date.warning.cancel"/></a></p>
             --%> </div>
           </div>
  </ycommerce:testId>
	</spring:htmlEscape>
</spring:escapeBody>"
}