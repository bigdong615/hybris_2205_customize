<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>

<spring:htmlEscape defaultHtmlEscape="true" />

      <div class="modal-dialog modal-dialog-centered modal-sm"  id="rentAgainPopUp">
      <div class="modal-content">
      <div class="modal-header">
      <h5>Wait!</h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
      <div class="modal-body">
         <p>
     <spring:theme code="text.saved.cart.restore" />
      </p>
      <div class="modal-actions">

      <spring:url var="restoreSavedCartPostUrl"
             value="/my-account/saved-carts/{/savedCartCode}/restorCart" htmlEscape="false">
          <spring:param name="savedCartCode" value="${commerceSaveCartResultData.savedCartData.code}"/>
      </spring:url>
                       <a id="restoreCartUrl" href="${restoreSavedCartPostUrl}" class="btn btn-block btn-primary">Continue</a>
                      <br>
                      <p class="text-center mb-0">
                     	<a href="#" class="lightteal" aria-label="Close" data-bs-dismiss="modal" aria-label="Close">
                     											<spring:theme code="basket.save.cart.action.cancel" /> </a></p>
          </div>

      </div>
      </div>
      </div>