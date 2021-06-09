<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="multiCheckout"
	tagdir="/WEB-INF/tags/responsive/checkout/multi"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement"
	tagdir="/WEB-INF/tags/responsive/formElement"%>
<%@ taglib prefix="sec"
	uri="http://www.springframework.org/security/tags"%>
<%@ taglib prefix="address" tagdir="/WEB-INF/tags/responsive/address"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="util"
	tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/util"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement"
	tagdir="/WEB-INF/tags/responsive/formElement"%>
<%@ taglib prefix="checkout"
	tagdir="/WEB-INF/tags/responsive/checkout/multi"%>

<jsp:include page="../../../../messages/braintreeErrorMessages.jsp" />
<c:url var="savedPaymentInfoFormURL" value="/checkout/multi/payment-method/braintree/choose-cc" />

<spring:eval
	expression="@configurationService.configuration.getProperty('braintree.store.in.vault')"
	var="storeInVault" />
<c:url value="${currentStepUrl}" var="choosePaymentMethodUrl" />
<template:page pageTitle="${pageTitle}">
	<section id="cartProcess" class="cart">
		<div class="container">
			<div id="cartSteps" class="row justify-content-center">
				<div class="col-xl-10">
					<span class="step1 complete"><i class="icon-check"></i> Your
						Rental</span><span class="step2 complete"><i class="icon-check"></i>
						Delivery or Pickup</span><span class="step3 active"><i
						class="number">3</i> Payment</span><span class="step4"><i
						class="number">4</i> Review</span>
				</div>
			</div>
			<div class="row justify-content-center">
				<div class="col-xl-10">
					<div class="row">
						<div id="order" class="col-lg-7">
							<h1>Payment</h1>
							<hr>
							<p>
								<b>Dates</b>&emsp;<input type="text"
									class="form-control cart-picker" id="litepicker"
									placeholder="<spring:theme code="text.rental.cart.select.date"/>">
							</p>
							<p class="overline">Pay With</p>
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
														<input type="radio" class="paypalselection"
															id="paymentMethodBT" name="paymentMethodSelection"
															value="bt"><label for="paymentMethodBT"></label>
													</button>
												</div>
											</c:when>
											<c:otherwise>
												<div style="overflow: auto;"></div>
											</c:otherwise>
										</c:choose>
										<div class="col-11" id="cardDetails">
											<b>Credit Card <img src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-cc.png" style="height: 44px; width: auto;"></b>
											<div class="collapse" id="credit-card-expand"
												data-bs-parent="#paymentOptions">

												<ycommerce:testId code="paymentDetailsForm">
													<form:form id="braintree-payment-form"
														name="silentOrderPostForm"
														class="create_update_payment_form"
														modelAttribute="sopPaymentDetailsForm"
														action="${request.contextPath}/braintree/checkout/hop/response"
														method="POST">

														<div class="hostedFields">
															<div class="control-group cardForm"
																style="dispaly: none;" id="cardForn">
																
																
                                       	
																<div id="number"
																	class="controls form-control secure testing">
                                                                </div>

																<div class="row">
																	<div class="col-4">

																		<div id="expirationMonth" class="controls form-control"></div>
																	</div>
																	<div class="col-4">

																		<div id="expirationYear" class="controls form-control"></div>
																	</div>
																	<div class="col-4">

																		<div id="cvv" class="controls form-control"></div>

																	</div>
																</div>
																<c:if test="${not empty userSelectedPaymentInfo}">
																${userSelectedPaymentInfo.cardNumber}
                                                                 <input id="userSelectedPaymentInfo_number" type="hidden" value="${userSelectedPaymentInfo.cardNumber}"/>
                                                                 <input id="userSelectedPaymentInfo_month" type="hidden" value="${userSelectedPaymentInfo.expiryMonth}"/>
                                                                 <input id="userSelectedPaymentInfo_year" type="hidden" value="${userSelectedPaymentInfo.expiryYear}"/>
                                                               </c:if>  
																<%-- <c:if test="${payPalCheckoutData.storeInVault}"> --%>
																<c:if test="true">
																<div class="form-additionals" id="savePaymentInfoComponent">
                                                                     <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
                                                                          <formElement:formCheckbox idKey="savePaymentInfo"
                                                                           labelKey="checkout.multi.sop.savePaymentInfo"
                                                                           path="savePaymentInfo" inputCSS="" labelCSS=""
                                                                            mandatory="false"/>
                                                                      </sec:authorize>
                                                             </div>	
                                                             
                                </c:if>
															</div>
														</div>
														<br />
														<%-- <c:if test="${payPalCheckoutData.storeInVault}">
                                    <div id="savePaymentButton" class="hide">
                                        <c:if test="${not empty braintreePaymentInfos}">
                                            <div class="form-group">
                                                <button type="button" class="btn btn-default btn-block js-saved-payments">
                                                    <spring:theme
                                                            code="checkout.multi.paymentMethod.addPaymentDetails.useSavedPaymentMethod"/>

                                                </button>
                                            </div>
                                        </c:if>
                                    </div>
                                    <div class="form-additionals" id="savePaymentInfoComponent">
                                        <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
                                            <formElement:formCheckbox idKey="savePaymentInfo"
                                                                      labelKey="checkout.multi.sop.savePaymentInfo"
                                                                      path="savePaymentInfo" inputCSS="" labelCSS=""
                                                                      mandatory="false"/>
                                        </sec:authorize>
                                    </div>
                                    <script>
                                        if (document.getElementById("savePaymentInfo")) {
                                            if (paypalIntent.toLowerCase() === 'order') {
                                                document.getElementById("savePaymentInfo").checked = false;
                                                document.getElementById("savePaymentInfoComponent").style.display = 'none';
                                            } else {
                                                document.getElementById("savePaymentInfo").checked = true;
                                            }
                                        }
                                    </script>
                                </c:if> --%>

														<div class="billingAddressData">

															<%-- <div class="headline clear">
                                        <spring:theme
                                                code="checkout.multi.paymentMethod.addPaymentDetails.billingAddress"/>
                                    </div> --%>
                                    <c:if test="${not empty billingAddresses and billingAddresses.size() > 0}">
                                    <b class="mt-4">Saved Billing Addresses</b>
                                            <div class="dropdown my-2">
                                            <c:forEach items="${billingAddresses}" var="billingAddress" begin="0" end="0">
                                            	<input type="hidden" id="savedBillingAddressId" name="savedBillingAddressId" value="${billingAddress.id }"/>
                                              <a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="savedAddresses" data-bs-toggle="dropdown" aria-expanded="false">
                                                ${billingAddress.formattedAddress }
                                              </a>
                                            
                                             </c:forEach>
                                              <ul class="dropdown-menu selectSavedBillingAddress" aria-labelledby="savedAddresses">
                                              <c:forEach items="${billingAddresses}" var="billingAddress">
                                                <li><a class="dropdown-item" href="#" data-id="${billingAddress.id }" data-address="${billingAddress.formattedAddress }">${billingAddress.formattedAddress }</a></li>
                                              </c:forEach>
                                              </ul>
                                            </div>
                                            <a href="#" class="gray80" id="paymentAddNewAddress" data-bs-toggle="collapse" data-bs-target="#billing-address-form-expand" aria-controls="billing-address-form-expand">+ Add a new address</a>
                                            </c:if>
                                            <div class="collapse" id="billing-address-form-expand">
                                            	 <c:if test="${cartData.deliveryItemsQuantity > 0}">
                                        <div id="useDeliveryAddressData"
                                             data-firstname="${deliveryAddress.firstName}"
                                             data-lastname="${deliveryAddress.lastName}"
                                             data-line1="${deliveryAddress.line1}"
                                             data-line2="${deliveryAddress.line2}"
                                             data-town="${deliveryAddress.town}"
                                             data-postalcode="${deliveryAddress.postalCode}"
                                             data-countryisocode="${deliveryAddress.country.isocode}"
                                             data-regionisocode="deliveryAddress.region.isocode"
                                             data-email="${deliveryAddress.email}"
                                             data-address-id="${deliveryAddress.id}"></div>
                                        <formElement:formCheckbox path="useDeliveryAddress"
                                                                  idKey="ccUseDeliveryAddress"
                                                                  labelKey="checkout.multi.sop.useMyDeliveryAddress"
                                                                  tabindex="11"/>
                                    </c:if> 

															<input type="hidden" name="paypal_email"
																id="paypal_email" /> <%-- <input type="hidden"
																value="${silentOrderPageData.parameters['billTo_email']}"
																class="text" name="billTo_email" id="billTo_email"> --%>
															<%-- <address:billAddressFormSelector
                                            supportedCountries="${countries}" regions="${regions}"
                                            tabindex="12"/> --%>
                                            
                                            <%-- <formElement:formSelectBox idKey="address.country"
	                           labelKey=""
	                           path="billTo_country"
	                           mandatory="true"
	                           skipBlank="false"
	                           skipBlankMessageKey="address.selectCountry"
	                           items="${countries}"
	                           itemValue="isocode"
	                           tabindex="${tabindex}"
	                           selectCSSClass="form-control" /> --%>
	                           <input type="hidden" name="billTo_country" id="address.country" value="US">
	                           
                                            <div id="billingAddressForm" class="billingAddressForm">
                                            
                                            </div>
                                            </div>

															 
															<%-- <checkout:billingAddressForm
																supportedCountries="${countries}" regions="${regions}"
																cartData="${cartData}" tabindex="12" /> --%>
														</div>

														<div class="form-additionals"></div>

														

														<%--<div id="paypalButtonError"></div>
                                <div id="google-pay-button" class="google-pay-button-container google_pay_container"></div>
                                <div id="mark-paypal-button" class="paypal_button_container btn btn-block"></div>--%>
														<%--<div id="lpm-buttons" class="paypal_button_container">
                                    <c:forEach items="${localPaymentMethods}" var="localPaymentMethod">
                                        <div id="${localPaymentMethod.code}" class="btn-local-payment-methods btn btn-block">
                                            <div>
                                                <img class="payment-button" src="${localPaymentMethod.image.url}"
                                                     alt="${localPaymentMethod.image.altText}">
                                            </div>
                                            <label>${localPaymentMethod.name}</label>
                                        </div>
                                    </c:forEach>
                                </div>--%>
													</form:form>
												</ycommerce:testId>

											</div>
										</div>
									</div>
									<div id="validationMessage">
										
									</div>
									
														<cart:blGiftCard cartData="${cartData}"/>
                                                        
                                                        <div id="allFieldvalidationMessage">
										
									                   </div>
                                                        
                             
														

									<%--<c:if test="${not empty braintreePaymentInfos}">
                    <div id="savedpayments">
                        <div id="savedpaymentstitle">
                            <div class="headline">
                                    <span class="headline-text"><spring:theme
                                            code="checkout.multi.paymentMethod.addPaymentDetails.useSavedPaymentMethod"/></span>
                            </div>
                        </div>
                        <div id="savedpaymentsbody">
                            <c:forEach items="${braintreePaymentInfos}" var="paymentInfo"
                                       varStatus="status">
                                <input type="text" value="${paymentInfo.subscriptionId}" class=" paymentType hide">
                                <form id="savedPaymentInfoForm${paymentInfo.id}"
                                      action="${request.contextPath}/checkout/multi/payment-method/braintree/choose-cc"
                                      method="GET">
                                    <input type="hidden" name="selectedPaymentMethodId"
                                           value="${paymentInfo.id}"/>
                                    <input type="hidden" name="selectedPaymentMethodNonce"
                                           value="${paymentInfo.paymentMethodNonce}">
                                    <c:choose>
                                        <c:when
                                                test="${paymentInfo.subscriptionId eq 'BrainTreePayPalExpress' or paymentInfo.subscriptionId eq 'PayPalAccount'}">
                                            <spring:theme code="paymentMethod.type.PayPal"/><br/>
                                            <input type="hidden" name="selectedPaymentMethod" value="PayPal">
                                            ${fn:escapeXml(paymentInfo.payer)}<br/>
                                            <img
                                                    src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-medium.png"
                                                    alt="PayPal icon"/>
                                            <br/>
                                        </c:when>
                                        <c:when test="${paymentInfo.subscriptionId eq 'VenmoAccount'}">
                                            <br><spring:theme code="paymentMethod.type.Venmo" /><br/>
                                            <input type="hidden" name="selectedPaymentMethod" value="PayPal">
                                            ${fn:escapeXml(paymentInfo.payer)}<br>
                                            <img height="28" width="56"
                                                 src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/venmo_acceptance_mark.svg"
                                                 alt="Venmo icon"/>
                                            <br/>
                                        </c:when>
                                        <c:when test="${paymentInfo.subscriptionId eq 'AndroidPayCard'}">
                                            <br><spring:theme code="paymentMethod.type.GooglePay" />
                                            <br>${fn:escapeXml(paymentInfo.payer)}
                                            <br><img height="38" width="56"
                                                     src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/googlePay_mark.png"
                                                     alt="GooglePay icon"/>
                                            <br>${fn:escapeXml(paymentInfo.cardNumber)}
                                            <br>${fn:escapeXml(paymentInfo.cardType)}
                                            <br><spring:theme code="text.expires" text="Expires" />
                                            <c:if test="${not empty paymentInfo.expiryMonth}">
                                                ${fn:escapeXml(paymentInfo.expiryMonth)} /
                                                ${fn:escapeXml(paymentInfo.expiryYear)}
                                            </c:if>
                                        </c:when>
                                        <c:otherwise>
                                            <c:if test="${not empty paymentInfo.cardholderName}">
                                                ${fn:escapeXml(paymentInfo.cardholderName)}<br/>
                                            </c:if>
                                            ${fn:escapeXml(paymentInfo.cardType)}<br/>
                                            ${fn:escapeXml(paymentInfo.cardNumber)}<br/>
                                            <c:if test="${not empty paymentInfo.expiryMonth}">
                                                <spring:theme
                                                        code="checkout.multi.paymentMethod.paymentDetails.expires"
                                                        arguments="${fn:escapeXml(paymentInfo.expiryMonth)},${fn:escapeXml(paymentInfo.expiryYear)}"/>
                                                <br/>
                                            </c:if>
                                            <c:if test="${not empty paymentInfo.accountHolderName}">
                                                <img src="${fn:escapeXml(paymentInfo.accountHolderName)}"
                                                     alt="Card Type"/>
                                                <br/>
                                            </c:if>
                                        </c:otherwise>
                                    </c:choose>
                                    <strong>${fn:escapeXml(paymentInfo.billingAddress.firstName)}&nbsp; ${fn:escapeXml(paymentInfo.billingAddress.lastName)}</strong><br/>
                                        ${fn:escapeXml(paymentInfo.billingAddress.line1)}&nbsp;
                                    <c:if test="${not empty paymentInfo.billingAddress.line2}">
                                        ${fn:escapeXml(paymentInfo.billingAddress.line2)}
                                    </c:if><br/>
                                        ${fn:escapeXml(paymentInfo.billingAddress.town)}&nbsp; ${fn:escapeXml(paymentInfo.billingAddress.region.isocodeShort)}<br/>
                                        ${fn:escapeXml(paymentInfo.billingAddress.postalCode)}&nbsp; ${fn:escapeXml(paymentInfo.billingAddress.country.isocode)}<br/>
                                    <c:choose>
                                        <c:when test="${paymentInfo.defaultPaymentInfo}">
                                            <button type="submit" class="btn btn-primary btn-block default-payment-method"
                                                    tabindex="${fn:escapeXml((status.count * 2) - 1)}"><spring:theme
                                                    code="checkout.multi.paymentMethod.addPaymentDetails.useDefaultPaymentDetails"/></button>
                                        </c:when>
                                        <c:otherwise>
                                            <button type="submit" class="btn btn-primary btn-block"
                                                    tabindex="${fn:escapeXml((status.count * 2) - 1)}"><spring:theme
                                                    code="checkout.multi.paymentMethod.addPaymentDetails.useThesePaymentDetails"/></button>
                                        </c:otherwise>
                                    </c:choose>
                                </form>
                            </c:forEach>
                        </div>
                    </div>
                </c:if>--%>
								</div>
							</div>
							<div class="cart-actions">
                                <a href="#" class="gray80">Back to renting</a>
                                <a href="javascript:void(0)" class="btn btn-sm btn-primary float-end" id="submit_silentOrderPostForm">Continue</a>
                            </div> 
							
						</div>

						<div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">
							<cart:orderSummery cartData="${cartData}"
								emptyCart="${emptyCart}" />
							<%-- <div class="notification notification-warning">This is a cart warning.</div>
                            <div class="notification notification-tip truck">Free 2-day shipping on orders over $150.</div>
                            <div class="notification notification-tip check">Free changes or cancellation until Jan 28.</div> --%>
							<div class="order-actions my-4">
								<a href="#" alt="Print Order"><i class="icon-print"></i></a> <a
									href="#"><i class="icon-save" alt="Save Order"></i></a>
								<%--<a href="${emptyCart}" alt="Trash Order" class="clear-cart-page" disabled="disabled"><i class="icon-trash"></i></a>--%>
							</div>
						</div>
					</div>

				</div>
			</div>
		</div>
		</div>
	</section>
	
	<div class="modal fade" id="editWarning" tabindex="-1" aria-hidden="true">
<div class="modal-dialog modal-dialog-centered modal-sm">
<div class="modal-content">
<div class="modal-header">
<h5 class="modal-title"><spring:theme code="shipping.interception.change.date.warning.wait"/></h5>
<button type="button" class="btn-close" aria-label="Close" id="shippingCloseIconModal"></button>
</div>
<div class="modal-body">
<input type="hidden" value="" id="rentalStartDate">
<input type="hidden" value="" id="rentalEndDate">
<p class="body14"><spring:theme code="shipping.interception.change.date.warning.message"/></p>
<a href="#" class="btn btn-primary btn-block my-4" id="shippingChangeRentalDate"><spring:theme code="shipping.interception.change.date.warning.continue"/></a>
<p class="text-center mb-0"><a href="#" class="lightteal" aria-label="Close" id="shippingCloseModal"><spring:theme code="shipping.interception.change.date.warning.cancel"/></a></p>
</div>
</div>
</div>
</div>
	<spring:eval
		expression="@configurationService.configuration.getProperty('braintree.user.action')"
		var="userAction" />

	<%--<util:importBtSDK
            sdkVersion="3.69.0"
            enablePayPal="${payPalStandardEnabled}"
            enableHostedFields="${hostedFieldsEnable}"
            enableGooglePay="${googlePayEnable}"
            enableVenmo="${venmoEnabled}"
            enableLocalPayment="${localPaymentsEnabled}"
            enableApplePay="${payPalCheckoutData.applePayEnabled}"
            enableSecure3d="${payPalCheckoutData.secure3d}"/>--%>
	<script type="text/javascript"
		src="https://js.braintreegateway.com/web/3.69.0/js/client.min.js"></script>
	<script type="text/javascript"
		src="https://js.braintreegateway.com/web/3.69.0/js/hosted-fields.min.js"></script>
	<script type="text/javascript"
		src="https://js.braintreegateway.com/web/3.69.0/js/data-collector.min.js"></script>


	<script>
        var paymentMethodsPage = "paymentMethodsPage";

        var clientToken = "${client_token}";
        var isCreditCardSelect = "${is_credit_card_select}";
        var isSingleUseSelect = "${is_single_use_select}";

        var advancedFraudToolsEnabled = "${payPalCheckoutData.advancedFraudTools}";
        var environment = "${payPalCheckoutData.environment}";
        var secure3d = "${payPalCheckoutData.secure3d}";
        var skip3dSecureLiabilityResult = "${payPalCheckoutData.skip3dSecureLiabilityResult}";
        var dbaName = "${payPalCheckoutData.dbaName}";

        // only paypal specific configuration options
        var storeInVault = "${payPalCheckoutData.storeInVault}";
        var paypalIntent = "${payPalCheckoutData.intent}";
        var amount = "${payPalCheckoutData.amount}";
        var locale = "${payPalCheckoutData.locale}";
        var enableShippingAddress = "${payPalCheckoutData.enableShippingAddress}";
        var braintreeLocale = "${braintreeLocale}";
        var currency = "${payPalCheckoutData.currency}";
        var recipientName = "${payPalCheckoutData.shippingAddressOverride.recipientName}";
        var streetAddress = "${payPalCheckoutData.shippingAddressOverride.streetAddress}";
        var extendedAddress = "${payPalCheckoutData.shippingAddressOverride.extendedAddress}";
        var locality = "${payPalCheckoutData.shippingAddressOverride.locality}";
        var countryCodeAlpha2 = "${payPalCheckoutData.shippingAddressOverride.countryCodeAlpha2}";
        var postalCode = "${payPalCheckoutData.shippingAddressOverride.postalCode}";
        var region = "${payPalCheckoutData.shippingAddressOverride.region}";
        var phone = "${payPalCheckoutData.shippingAddressOverride.phone}";
        var billingAgreementDescription = "${billingAgreementDescription}";
        var userAction = "${userAction}";
        var payPalStandardEnabled =${payPalStandardEnabled};
        var applePayEnabled = ${payPalCheckoutData.applePayEnabled};
        var venmoEnabled = ${venmoEnabled};
        var creditEnabled = ${isCreditEnabled};
        var payPalButtonConfig = "${payPalMarkButtonConfig}";
        var localPaymentMethods = "${localPaymentMethods}";
        var lpmids = ${lpmids};
        var venmoProfileId = "${venmoProfileId}";
        var endpointURL = "${endpointURL}";
        var disableMarkFunding = "${disableMarkFunding}";
        var googleMerchantId = "${googleMerchantId}";
        var googlePayCountryCode = "${googlePayCountryCode}";
        var currencyMerchantAccountId = "${currencyMerchantAccount}";
        var googlePayEnabled = ${googlePayEnable};
    </script>
</template:page>
