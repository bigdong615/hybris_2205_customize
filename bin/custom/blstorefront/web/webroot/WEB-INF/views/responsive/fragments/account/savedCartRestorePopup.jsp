<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

      <div class="modal-dialog modal-dialog-centered modal-sm">
      <div class="modal-content savecart-modal">
      <div class="modal-header" style="display: inline-block; text-align: center; padding: 19px 0px !important; height: 66px;">
      <h5>Wait!</h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close" style=" top:26px;"></button>
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