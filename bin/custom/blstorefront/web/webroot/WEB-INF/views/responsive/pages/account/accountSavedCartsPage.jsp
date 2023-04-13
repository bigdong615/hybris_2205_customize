<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<spring:htmlEscape defaultHtmlEscape="true"/>

<spring:url value="/my-account/saved-carts/" var="savedCartsLink" htmlEscape="false"/>
<c:set var="searchUrl" value="/my-account/saved-carts?sort=${ycommerce:encodeUrl(searchPageData.pagination.sort)}"/>

<div id="accountContent" class="col-lg-8 offset-lg-1">
	<h3><spring:theme code="text.saved.cart"/></h3>
	<c:choose>
	<c:when test="${empty searchPageData.results}">
      <div class="account-section-content content-empty">
          <hr>
            <div class="notification no-orders">
                       <p><strong><spring:theme code="text.empty.savedcart"/></strong></p>
                       <p><spring:theme code="text.empty.savedcart.paragraph"/></p>
            </div>

      </div>
  </c:when>
	<c:otherwise>
		<c:forEach items="${searchPageData.results}" var="savedCart" varStatus="loop">
			<div class="order-block">
				<div class="row">
					<c:if test="${not empty saved_cart_success && renamed_cart_code eq fn:escapeXml(savedCart.code)}">
                     		<div class="alert alert-info alert-dismissable getAccAlert col-md-6">${saved_cart_success}</div>
        	</c:if>
					<div class="col-12 col-md-7 my-auto"> <b>${fn:escapeXml(savedCart.name)}</b>
						<p class="gray80 body14">
							<fmt:formatDate value="${savedCart.saveTime}" dateStyle="medium" />
							<br> ${fn:escapeXml(savedCart.totalPrice.formattedValue)} </p>
						<div class="row mb-4 mb-md-0 order-images">
							<c:forEach items="${savedCart.entries}" var="entry">
								<div class="col-4 col-md-3">
									<product:productPrimaryImage product="${entry.product}" format="thumbnail" />
								</div>
							</c:forEach>
						</div>
					</div>
					<div class="col-6 col-md-3 offset-md-1 text-start text-md-end">
						<a href="#" class="js-restore-saved-cart restore-item-link btn btn-primary" data-savedcart-id="${fn:escapeXml(savedCart.code)}"
						 data-restore-popup-title="<spring:theme code='text.account.savedcart.restore.popuptitle'/>"> <span class="hidden-xs"><spring:theme code="text.savedcart.button.restore"/></span> </a>
					</div>
					<div class="col-6 col-md-1">
						<div class="btn-group"> <a id="btn-rental-${loop.index}" class="dropdown-toggle" data-bs-toggle="dropdown" aria-expanded="false" href="#"><i class="icon-dots"></i></a>
							<ul class="dropdown-menu" aria-labeledby="btn-rental-001">
							<c:url var="savedEditUrl" value="/my-account/saved-carts/${fn:escapeXml(savedCart.code)}/edit" />
							  <c:url var="removeSavedUrl" value="/my-account/saved-carts/${fn:escapeXml(savedCart.code)}/delete" />
								<li><a href="#" data-bs-toggle="modal" class ="js-rename-saved-carts" data-savedcart-id="${savedEditUrl}" data-bs-target="#renameCart"
								data-savedcart-name="${fn:escapeXml(savedCart.name)}" data-savedcart-code="${fn:escapeXml(savedCart.code)}"><spring:theme code="text.savedcart.button.rename"/></a></li>
								<li>
								<li><a href="#" data-bs-toggle="modal" class="js-remove-saved-carts" data-savedcart-id="${removeSavedUrl}"  data-bs-target="#removeCart"><spring:theme code="text.savedcart.button.remove"/></a></li>
								</li>
							</ul>
						</div>
					</div>
				</div>
			</div>

		</c:forEach>
		 <nav:pagination searchPageData="${searchPageData}" searchUrl="${searchUrl}"/>
	</c:otherwise>

</c:choose>
</div>


<!-- Rename Modals -->
<div class="modal fade" id="renameCart" aria-hidden="true" aria-labelledby="..." tabindex="-1">
	<div class="modal-dialog modal-dialog-centered modal-mm">
		<div class="modal-content">
			<div class="modal-header">
				<h5 class="modal-title"><spring:theme code="text.savedcart.rename"/></h5>
				<button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
			</div>
			<div class="modal-body">
			<input type="hidden"  id="renameCartIdUrl" value="" var="renameCartIdAction"/>
			<input type="hidden"  id="renameCartCode" value="" var="renameCartId"/>
			<input type="hidden" name="${CSRFToken.parameterName}" value="${CSRFToken.token}" />

				<form:form action="${renameCartIdAction}" id="renameCartForm" modelAttribute="saveCartForm" autocomplete="off">
					<div class="form-group">
						<c:if test="${not empty messageKey}">
							<div class="legend">
								<spring:theme code="${messageKey}" />
							</div>
						</c:if>
						<label class="control-label renameSaveCartName-css" for="name">
						<form:input cssClass="form-control title-case-rename" id="renameSaveCartName" path="name" maxlength="30" placeholder="Cart Name" />
						<div class="help-block right-cartName" id="remain"></div>
					</div>
					<br>
					<div class="form-actions">
						<div class="modal-actions">
							<div class="row">
								<div class="col-sm-12">
									<button type="submit" class="btn btn-primary btn-block js-validate-rename-cart" value="${editActionUrl}" data-cart-code="${renameCartId}">
										<spring:theme code="text.button.save" /> </button>
									<br>
									<p class="text-center mb-0">
										<a href="#" class="lightteal" aria-label="Close" data-bs-dismiss="modal" aria-label="Close" id="cancelSaveCartButton" >
											<spring:theme code="basket.save.cart.action.cancel" /> </a>
									</p>
								</div>
							</div>
						</div>
					</div>
				</form:form>
							<div class ="notification notification-error d-none" id="errorMessages_renamecart"> </div>
			</div>
		</div>
	</div>
</div>


<!-- Remove Modals -->
  <div class="modal fade" id="removeCart" tabindex="-1" aria-hidden="true">
        <div class="modal-dialog modal-dialog-centered modal-sm">
          <div class="modal-content">
            <div class="modal-header">
              <h5 class="modal-title"><spring:theme code="text.savedcart.warning.wait"/></h5>
              <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
            <div class="modal-body">
             <p><spring:theme code="text.savedcart.delete.warning"/></p>
             <input type="hidden"  id="removeCartIdUrl" value="" var="removeCartIdAction"/>
             <c:url var="savedEditUrl" value="/my-account/saved-carts/${removeCartIdAction}/delete" />
                 <a id="removecartUrl" href="${savedEditUrl}" class="btn btn-block btn-primary mt-4 "><spring:theme code="text.savedcart.profile.continue"/></a>
                  <p class="body14 text-center mb-0 mt-4"><a href="#" class="lightteal" data-bs-dismiss="modal" aria-label="Close">
                  <spring:theme code="text.savedcart.profile.cancel"/></a></p>
            </div>
          </div>
        </div>
      </div>

  <div class="modal fade signinbox" id="restorePopUp" aria-hidden="true" aria-labelledby="..." tabindex="-1">
       <div class="modal-dialog modal-dialog-centered modal-sm">
       </div>
    </div>
