<%@ page trimDirectiveWhitespaces="true" contentType="application/json" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>

<%-- ToDo - Need to add Interception pop-up for Rental product Add to Cart once provided, as of now used sample html. --%>

{
"addToCartLayer":"<spring:escapeBody javaScriptEscape="true" htmlEscape="false">
	<spring:htmlEscape defaultHtmlEscape="true">
	<spring:theme code="text.addToCart" var="addToCartText"/>
	<c:url value="/cart" var="cartUrl"/>
	<ycommerce:testId code="addToCartPopup">

   <div class="modal-content">
            <div class="modal-header">
              <h5 class="modal-title">Wait!</h5>
              <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
            <div class="modal-body">
              <div class="row">
                 Rental and used gear can not be ordered together.
              </div>
            </div>
            <div class="modal-footer">
                <a href="#" class="btn btn-outline" data-bs-dismiss="modal">Cancel</a>
                <a href="#" class="btn btn-primary" data-bs-dismiss="modal">Continue</a>
            </div>
   </div>

  </ycommerce:testId>
	</spring:htmlEscape>
</spring:escapeBody>"
}