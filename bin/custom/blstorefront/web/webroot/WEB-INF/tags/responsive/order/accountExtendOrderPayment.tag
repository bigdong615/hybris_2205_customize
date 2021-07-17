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
												<select class="btn btn-block btn-outline dropdown-toggle text-start" id="saved-payment-action-ExtendBill">
																	<ul class="dropdown-menu savedPaymentList" aria-labelledby="savedCards" >
																	<c:choose>
																		<c:when test="false">

																		</c:when>
																		<c:otherwise>
																		<li>
																				<%-- <button class="dropdown-item" data-id="${paymentInfo.id}" data-nonce="${paymentInfo.paymentMethodNonce}"> --%>
																					<option>
																					Select Card
																					</option>
																				<!-- </button> -->
																			</li>
																		</c:otherwise>
																	</c:choose>
																		<c:forEach items="${braintreePaymentInfos}" var="paymentInfo" varStatus="status">
																		<c:if test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'CreditCard')}">
																			<li >
																				<%-- <button class="dropdown-item" data-id="${paymentInfo.id}" data-nonce="${paymentInfo.paymentMethodNonce}"> --%>
																					<option data-id="${paymentInfo.id}" data-nonce="${paymentInfo.paymentMethodNonce}">
																					<img src="${paymentInfo.accountHolderName }" style="max-width: 33px; height: auto;">
																					&nbsp ${fn:escapeXml(paymentInfo.cardNumber)} &nbsp exp ${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)}
																					</option>
																				<!-- </button> -->
																			</li>
																		</c:if>
																		</c:forEach>
																	</ul>
																	</select>


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
                														<input type="radio" class="paypalselection" id="paymentMethodPayPal" name="paymentMethodSelection" value="bt">
                														<label for="paymentMethodPayPal"></label>
                													</button>
                												</c:otherwise>
                											</c:choose>
                										</div>
                										<div class="col-11">
                											<b>PayPal <img src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-medium.png" style="height: 44px; width: auto;"></b>
                											<div class="collapse" id="paypal-expand" data-bs-parent="#paymentOptions">
                												<br/>
                												<div id="mark-paypal-button" class="paypal_button_container btn btn-block"></div>
                												<div id="text" class="paypalurl">
                													<a style="padding-left: 10px;"
                														href="https://www.paypal.com/webapps/mpp/paypal-popup"
                														title="How PayPal Works"
                														onclick="javascript:window.open('https://www.paypal.com/webapps/mpp/paypal-popup','WIPaypal','toolbar=no, location=no, directories=no, status=no, menubar=no, scrollbars=yes, resizable=yes, width=1060, height=700'); return false;">
                														<spring:theme code="braintree.text.what.is.paypal"/>?</a>
                												</div>
                												<div id="payPalErrorMessage"></div>
                											</div>
                										</div>



	                                 </div>
                								</div>


<script>
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