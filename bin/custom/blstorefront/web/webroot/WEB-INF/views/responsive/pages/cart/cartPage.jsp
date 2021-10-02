<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<template:page pageTitle="${pageTitle}">

<%--if required then we can include this cart validation--%>
  <%-- <cart:cartValidation/> --%>

<c:choose>
   <c:when test="${cartData.hasGiftCart}">
      <cart:blGiftCardPurchaseCartPage/>
   </c:when>
   <c:when test="${cartData.isNewGearOrder eq true}">
      <cart:blNewGearCartPage/>
   </c:when>
   <c:when test="${cartData.isRentalCart}">
      <cart:blRentalCartPage/>
   </c:when>
   <c:otherwise>
      <cart:blUsedGearCartPage/>
   </c:otherwise>
</c:choose>

  

</template:page>



<div class="modal fade" id="saveCartModal" aria-hidden="true" aria-labelledby="..." tabindex="-1">
      <div class="modal-dialog modal-dialog-centered modal-sm">
        <div class="modal-content">
          <div class="modal-header">
          <h5 class="zero-margine"><spring:theme code="text.savedcart.warning.wait"/></h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
          <div class="modal-body">
              <spring:url value="/cart/save" var="saveCartUrl" htmlEscape="false"/>
              <p><spring:theme code="text.savecart.warning"/></p>
              <form:form action="${saveCartUrl}" id="saveCartForm" modelAttribute="saveCartForm" autocomplete="off">
         <div class="form-group">
				<c:if test="${not empty messageKey}">
					<div class="legend"><spring:theme code="${messageKey}"/></div>
				</c:if>

				<label class="control-label" for="name">
					<spring:theme code="basket.save.cart.name" />
				</label>
                <form:input cssClass="form-control" id="saveCartName" path="name" maxlength="30" />
                 <div class="help-block right-cartName" id="remain">
                </div>
            </div>
			<br>
			<div class="form-actions">
	            <div class="modal-actions">
                    <div class="row">
                        <div class="col-sm-12">
                            <button type="button" class="btn btn-primary btn-block save-cart-button js-validate-saved-cart" id="saveCartButton">
                                <spring:theme code="basket.save.cart.action.continue"/>
                            </button>
                            <br>
                            <p class="text-center mb-0"><a href="#" class="lightteal" aria-label="Close" data-bs-dismiss="modal" aria-label="Close" id="cancelSaveCartButton">
                                <spring:theme code="basket.save.cart.action.cancel"/> </a></p>
                        </div>
	                </div>
	            </div>
	        </div>
	      							<div class ="notification notification-error d-none" id="errorMessages_savecart"> </div>
              </form:form>
          </div>
        </div>
      </div>
    </div>

<script>
$(".js-validate-saved-cart").on("click", function(e) {
   			e.preventDefault();
   			var cartName = document.getElementById("saveCartName").value;
        if(cartName !== 'undefined' && cartName !== '' && !cartName.trim().length === false){
                         $('#saveCartForm').submit();
        }
        else{
          $("#errorMessages_savecart").removeClass("d-none");
          $("#errorMessages_savecart").html("Whoops, you need to name your saved cart.");
        }
   	});
</script>


