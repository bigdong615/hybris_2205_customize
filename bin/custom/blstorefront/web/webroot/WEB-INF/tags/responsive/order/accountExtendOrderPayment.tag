<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ attribute name="order" required="true" type="de.hybris.platform.commercefacades.order.data.AbstractOrderData" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<div class="accordion" id="paymentOptions">
   <div class="accordion-item payProduct">
      <div class="row">
         <c:choose>
            <c:when test="${hostedFieldsEnable}">
               <div class="col-1 text-center pt-2">
                  <button class="btn-checkbox" type="button"
                     data-bs-toggle="collapse"
                     data-bs-target="#credit-card-expand"
                     aria-controls="credit-card-expand" aria-expanded="false">
                  <input type="radio" class="paypalselection" id="paymentMethodBT" name="paymentMethodSelection" value="bt">
                  <label for="paymentMethodBT"></label>
                  </button>
               </div>
            </c:when>
            <c:otherwise>
               <div style="overflow: auto;"></div>
            </c:otherwise>
         </c:choose>
         <div class="col-11">
            <b>Credit Card <img src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-cc.png" style="height: 44px; width: auto;"></b>
            <div class="collapse" id="credit-card-expand" data-bs-parent="#paymentOptions">
            <div class="dropdown my-2">
            <button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="savedCards" data-bs-toggle="dropdown" aria-expanded="false">
            	<c:choose>
                        <c:when test="false">
                        </c:when>
                        <c:otherwise>
                        	Select Card
                        </c:otherwise>
                     </c:choose>
                    </button>
                     <ul class="js-enable-extend-button dropdown-menu savedPaymentList" aria-labelledby="savedCards" id="saved-payment-action-ExtendBill">
                     <c:forEach items="${braintreePaymentInfos}" var="paymentInfo" varStatus="status">
                        <c:if test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'CreditCard')}">
                           <li>
                              <button class="dropdown-item" data-id="${paymentInfo.id}" data-nonce="${paymentInfo.paymentMethodNonce}">
                             
                                 <img src="${paymentInfo.accountHolderName }" style="max-width: 33px; height: auto;">
                                 ${paymentInfo.cardType} &nbsp ${fn:escapeXml(paymentInfo.cardNumber)} &nbsp exp ${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)}
                              
                              </button>
                           </li>
                        </c:if>
                     </c:forEach>
                  </ul>
            	</div>
               </br>
               <a href="#" data-bs-toggle="modal" data-bs-target="#addCrediCard" class="gray80"><spring:theme code="text.myaccount.extend.order.new.card"/></a>
            </div>
         </div>
      </div>
   </div>
</div>
<div class="accordion-item payProduct">
   <c:if test="${not empty userSelectedPayPalPaymentInfo}">
      <input type="hidden" id="isPayPalPresent" name="isPayPalPresent" value="true"/>
   </c:if>
   <div class="row">
      <div class="col-1 text-center pt-2">
         <c:choose>
            <c:when test="${disablePayment}">
               <button class="btn-checkbox paymentDisabled" type="button" disabled></button>
            </c:when>
            <c:otherwise>
               <button class="btn-checkbox" type="button"
                  data-bs-toggle="collapse"
                  data-bs-target="#paypal-expand"
                  aria-controls="paypal-expand" aria-expanded="false">
               <input type="radio" class="paypalselection js-enable-extend-button" id="paymentMethodPayPal" name="paymentMethodSelection" value="bt">
               <label for="paymentMethodPayPal"></label>
               </button>
            </c:otherwise>
         </c:choose>
      </div>
      <div class="col-11">
         <b> <spring:theme code="text.order.extend.paypal"/> <img src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-medium.png" style="height: 44px; width: auto;"></b>
         <div class="collapse" id="paypal-expand" data-bs-parent="#paymentOptions">
            <br/>
            <div id="mark-paypal-button" class="paypal_button_container btn btn-block"></div>
            <div id="text" class="paypalurl">
               <a style="padding-left: 10px;"
                  href="https://www.paypal.com/webapps/mpp/paypal-popup"
                  title="<spring:theme code="text.order.extend.po"/>"
                  onclick="javascript:window.open('https://www.paypal.com/webapps/mpp/paypal-popup','WIPaypal','toolbar=no, location=no, directories=no, status=no, menubar=no, scrollbars=yes, resizable=yes, width=1060, height=700'); return false;">
                  <spring:theme code="text.order.extend..what.is.paypal"/>
               </a>
            </div>
            <div id="payPalErrorMessage"></div>
         </div>
      </div>
   </div>
</div>

                <c:if test="${order.isPOEnabled}">
                	<div class="accordion-item payProduct">
                	  <c:if test="${not empty selectedPoNumber}">
                    		<input type="hidden" id="isPOPresent" name="isPOPresent" value="true"/>
                    </c:if>
                	  <div class="row">
                			<div class="col-1 text-center">
                			  <c:choose>
                        	<c:when test="${disablePayment}">
                        			<button class="btn-checkbox paymentDisabled" type="button" disabled></button>
                        	</c:when>
                        	<c:otherwise>
                				    <button class="btn-checkbox" type="button" data-bs-toggle="collapse"
                					    data-bs-target="#po-expand" aria-controls="po-expand"
                					    aria-expanded="false">
                					    <input type="radio" class="paypalselection js-enable-extend-button" id="paymentMethodPo" name="paymentMethodSelection" value="bt"><label
                						  for="paymentMethodPo"></label>
                				    </button>
                				  </c:otherwise>
                        </c:choose>
                			</div>
                			<div class="col-11">
                				<b><spring:theme code="text.payment.page.po" /></b>
                				<div class="collapse" id="po-expand"
                					data-bs-parent="#paymentOptions">
                				<form:form name="submitSavedPoForm" method="POST" id="submitSavedPoForm" action="${reviewSavePoPaymentAction}">
                					<input type="text" class="form-control po-number" name="extendPoNumberInput" id="extendPoNumberInput" min="1" max="30" maxlength="30" value="${selectedPoNumber}"
                						placeholder="<spring:theme code="text.payment.page.po.number.placeholder"/>">
                					<input type="text" class="form-control po-number" name="extendPoNotesInput" id="extendPoNotesInput" min="1" max="1000" maxlength="1000" value="${selectedPoNotes}"
                						placeholder="<spring:theme code="text.payment.page.po.notes.placeholder"/>">
                          <input type="hidden" id="poSelected" name="poSelected" value=""/>
                				</form:form>
                				</div>
                			</div>
                		</div>
                	</div>
                </c:if>

    <!-- Modals -->
    <div class="modal fade" id="addCrediCard" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-sm">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title"><spring:theme code="text.extend.order.credi.card"/></h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
          <div class="modal-body">
           <spring:theme code="text.extend.order.credi.card.message" />

           <form id="extend-order-payment-add-form"action="${request.contextPath}/my-account/add-payment-method" method="GET">
                     <input type="hidden" id="orderId" name="orderId" value="" />
                            <c:url value="/my-account/add-payment-method" var="addPaymentUrl" />
                          <a href="" class="btn btn-block btn-primary mt-4 add-cc-extendOrderform" data-order="${order.code}:extendOrder"><spring:theme code="text.extend.order.credi.continue"/></a>
                              <br>
                          <p class="text-center mb-0"><a href="#" class="lightteal" aria-label="Close" data-bs-dismiss="modal" aria-label="Close">
                          <spring:theme code="basket.save.cart.action.cancel"/> </a></p>
                     </form>
          </div>
        </div>
      </div>
    </div>


<script>
   var enableShippingAddress = "false";
   var addPaymentMethodsPage = "addPaymentMethodsPage";
   var deliveryAddressId = "${selectedAddressCode}";
   var paypalIntent = "${payPalConfigurationData.intent}";
   var storeInVault = "${payPalConfigurationData.storeInVault}";
      var clientToken = "${client_token}";
      var isCreditCardSelect = "${is_credit_card_select}";
      var isSingleUseSelect = "${is_single_use_select}";
      var advancedFraudToolsEnabled = "${payPalConfigurationData.advancedFraudTools}";
      var environment = "${payPalConfigurationData.environment}";
      var secure3d = "${payPalConfigurationData.secure3d}";
      var skip3dSecureLiabilityResult = "${payPalConfigurationData.skip3dSecureLiabilityResult}";
      var dbaName = "${payPalConfigurationData.dbaName}";
      var singleUse = "false";
      var locale = "${payPalConfigurationData.locale}";
      var currency = "${payPalConfigurationData.currency}";
      var braintreeLocale = "${braintreeLocale}";
      var billingAgreementDescription = "${billingAgreementDescription}"
   var userAction="${userAction}";
   var payPalStandardEnabled = ${payPalStandardEnabled};
   var venmoEnabled = ${venmoEnabled};
      var creditEnabled = false;
      var payPalButtonConfig = "${payPalMarkButtonConfig}";
   var googleMerchantId = "${googleMerchantId}";
   var googlePayCountryCode = "${googlePayCountryCode}";
   var googlePayEnabled = ${googlePayEnable};



</script>
<script type="text/javascript"
   src="https://js.braintreegateway.com/web/3.69.0/js/client.min.js"></script>
<script type="text/javascript"
   src="https://js.braintreegateway.com/web/3.69.0/js/hosted-fields.min.js"></script>
<script type="text/javascript"
   src="https://js.braintreegateway.com/web/3.69.0/js/data-collector.min.js"></script>
<script type="text/javascript" src="https://js.braintreegateway.com/web/3.69.0/js/paypal.min.js"></script>
<script type="text/javascript" src="https://js.braintreegateway.com/web/3.69.0/js/paypal-checkout.min.js"></script>