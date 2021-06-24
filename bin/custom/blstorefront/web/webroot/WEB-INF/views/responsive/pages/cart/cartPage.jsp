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
          <h5>Wait!</h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
          <div class="modal-body">
              <spring:url value="/cart/save" var="saveCartUrl" htmlEscape="false"/>
              <p>Saving these items will remove them from your current cart and place them in your account to order later. Are you sure you want to proceed?</p>
              <form:form action="${saveCartUrl}" id="saveCartForm" modelAttribute="saveCartForm" autocomplete="off">
         <div class="form-group">
				<c:if test="${not empty messageKey}">
					<div class="legend"><spring:theme code="${messageKey}"/></div>
				</c:if>

				<label class="control-label" for="name">
					<spring:theme code="basket.save.cart.name" />
				</label>
                <form:input cssClass="form-control" id="saveCartName" path="name" maxlength="255" />
                 <div class="help-block right-cartName" id="remain">
                </div>
            </div>
			<br>
			<div class="form-actions">
	            <div class="modal-actions">
                    <div class="row">
                        <div class="col-sm-12">
                            <button type="submit" class="btn btn-primary btn-block save-cart-button" id="saveCartButton">
                                <spring:theme code="basket.save.cart.action.save"/>
                            </button>
                            <br>
                            <p class="text-center mb-0"><a href="#" class="lightteal" aria-label="Close" data-bs-dismiss="modal" aria-label="Close" id="cancelSaveCartButton">
                                <spring:theme code="basket.save.cart.action.cancel"/> </a></p>
                        </div>
	                </div>
	            </div>
	        </div>
              </form:form>



          </div>
        </div>
      </div>
    </div>
