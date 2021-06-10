<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="util" tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/util" %>

<jsp:include page="../../../messages/braintreeErrorMessages.jsp" />
<spring:eval expression="@configurationService.configuration.getProperty('braintree.store.in.vault')" var="storeInVault"/>
<div class="container">
	<div class="account-section-content	 account-section-content-small">
		<div class="account-addressbook">
			<c:if test="${payPalConfigurationData.storeInVault}">
			<c:if test="${empty selectedAddressCode}">
				<spring:theme code="account.payment.selectAddress" text="Please select Billing Address for Payment Method"/></div>
				<br/>
			</c:if>
			<c:if test="${empty deliveryAddresses}">
				<br/>
				<div class="emptyMessage"><spring:theme code="account.payment.noAddress" text="No Saved Addresses"/></div>
				<div class="col-xs-12 col-sm-6 col-md-5 accountAddAddress">
					<a href="add-address" class="btn btn-primary btn-block"><spring:theme code="text.account.addressBook.addAddress" text="Add New Address"/></a>
				</div>
			</c:if>
			<c:if test="${empty selectedAddressCode}">
				<c:if test="${not empty deliveryAddresses}">
					<button id="viewAddressBook" class="btn btn-default js-address-book" type="button"> Address Book </button>
					<div id="savedAddressListHolder" class="clear">
						<div id="addressbook">
							<c:forEach items="${deliveryAddresses}" var="deliveryAddress" varStatus="status">
								<div class="addressEntry">
									<form action="${request.contextPath}/my-account/add-payment-method" method="GET">
										<input type="hidden" name="selectedAddressCode" value="${deliveryAddress.id}" />
										<ul>
											<li>
												<strong>${fn:escapeXml(deliveryAddress.title)}&nbsp;
														${fn:escapeXml(deliveryAddress.firstName)}&nbsp;
														${fn:escapeXml(deliveryAddress.lastName)}</strong>
												<br>
													${fn:escapeXml(deliveryAddress.line1)}&nbsp;
													${fn:escapeXml(deliveryAddress.line2)}
												<br>
													${fn:escapeXml(deliveryAddress.town)}
												<c:if test="${not empty deliveryAddress.region.name}">
													&nbsp;${fn:escapeXml(deliveryAddress.region.name)}
												</c:if>
												<br>
													${fn:escapeXml(deliveryAddress.country.name)}&nbsp;
													${fn:escapeXml(deliveryAddress.postalCode)}
											</li>
										</ul>
										<button type="submit" class="btn btn-primary btn-block">
											<spring:theme code="account.payment.address.useThisAddress" text="Use this Address"/>
										</button>
									</form>
								</div>
							</c:forEach>
						</div>
					</div>
				</c:if>
			</c:if>

			<c:if test="${not empty selectedAddressCode}">
				<div class="no-payment-methods-message headline hidden"><spring:theme code="checkout.multi.paymentMethod.noPaymentMethods"/></div>
				<div class="account-section-header">
					<spring:theme code="account.payment.address.addPaymentMethod.title" text="Add Payment Method"/>
				</div>
				<br>
				<c:choose>
                    <c:when test="${googlePayEnable}">
                        <div style="overflow: auto;" id="googlepay" class="hide">
                            <input id="paymentMethodGooglePay" type="radio"
                                name="paymentMethodSelection" value="googlePay"
                                class="paypalselection"/>
                            <div id="google-pay-button-container">
                                <div class="cmsimage">
                                    <img class="google-pay-image"
                                        src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/googlePay_mark.png"
                                        alt="Buy with GooglePay" class="payment-logo"/>
                                </div>
                            </div>
                        </div>
                        <br>
                    </c:when>
                    <c:otherwise>
                        <div style="overflow: auto;"></div>
                    </c:otherwise>
                </c:choose>
		<c:if test="${payPalConfigurationData.intent ne 'order'}">
			<c:choose>
				<c:when test="${payPalStandardEnabled}">
					<div style="overflow: auto;" id="paypal-checkbox">
						<input id="paymentMethodPayPal" type="radio"
							   name="paymentMethodSelection" value="paypal"
							   class="paypalselection" checked="true" />
						<div class="paypalimage" id="paypal-container">
							<div class="cmsimage">
								<img
										src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-medium.png"
										alt="Buy now with PayPal" class="payment-logo"/>
							</div>
						</div>
						<div id="text" class="paypalurl">
							<a style="padding-left: 10px;"
							   href="https://www.paypal.com/webapps/mpp/paypal-popup"
							   title="How PayPal Works"
							   onclick="javascript:window.open('https://www.paypal.com/webapps/mpp/paypal-popup','WIPaypal','toolbar=no, location=no, directories=no, status=no, menubar=no, scrollbars=yes, resizable=yes, width=1060, height=700'); return false;"><spring:theme
									code="braintree.text.what.is.paypal" />?</a>
						</div>
					</div>
				</c:when>
				<c:otherwise>
					<div style="overflow: auto;"></div>
				</c:otherwise>
			</c:choose>
		</c:if>
				<br>
		<c:if test="${not payPalConfigurationData.secure3d}">
			<c:choose>
				<c:when test="${hostedFieldsEnable}">
					<div style="overflow: auto;" id="braintree-container">
						<input id="paymentMethodBT" type="radio" name="paymentMethodSelection" value="bt"
							   class="paypalselection" checked="true"/>
						<c:if test="${(not empty paymentsImagesURL)}">
							<c:forEach items="${paymentsImagesURL}" var="url">
								<img src="${url.value}" alt="${url.key}" />
							</c:forEach>
						</c:if>
						<input type="hidden" value="false" class="text" name="paypal_is_valid" id="paypal_is_valid">
					</div>
				</c:when>
				<c:otherwise>
					<div style="overflow: auto;"></div>
				</c:otherwise>
			</c:choose>
		</c:if>
		<c:choose>
			<c:when test="${venmoEnabled}">
				<div style="overflow: auto;" id="venmo" class="hide">
					<input id="paymentMethodVenmo" type="radio"
						   name="paymentMethodSelection" value="venmo"
						   class="paypalselection" checked="true" />
					<div id="venmo-mark-container">
						<div class="cmsimage">
							<img class="venmo-mark payment-logo"
								 src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/venmo_acceptance_mark.svg"
								 alt="Venmo"/>
						</div>
					</div>
				</div>
			</c:when>
			<c:otherwise>
				<div style="overflow: auto;"></div>
			</c:otherwise>
		</c:choose>
				<ycommerce:testId code="paymentDetailsForm">
					<div class="account-section-content">
						<form:form id="braintree-payment-form"
								   action="${request.contextPath}/my-account/add-payment-method" method="POST">
							<div class="hostedFields">
								<div class="account-section-header">
									<spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.paymentCard" text="Card Details"/>
								</div>
								<div class="description">
									<spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.enterYourCardDetails" text="Please enter your card details for payment"/></br>
								</div>
								<div class="control-group cardForm" style="dispaly: none;" id="cardForn">
									<label for="cardholderName" class="control-label ">
										<spring:theme code="braintree.text.cc.cardholder" />
									</label>
									<div class="controls" >
										<input id="cardholderName" value="" maxlength="175"/>
									</div>
									<label for="number" class="control-label ">
										<spring:theme code="braintree.text.cc.number" /></label>
									<div id="number" class="controls"></div>
									<label for="cvv" class="control-label ">
										<spring:theme code="braintree.text.cc.cvv" /></label>
									<div id="cvv" class="controls"></div>
									<label for="expiration-date" class="control-label ">
										<spring:theme code="braintree.text.cc.expiration.date" /></label>
									<div id="expiration-date" class="controls"></div>
								</div>
							</div>
							<br/>

							<input type="hidden" name="paypal_email" id="paypal_email"/>
							<input type="hidden" name="selectedAddressCode" id="selectedAddressCode" value="${selectedAddressCode}"/>
							<div class="form-additionals"/>
							<div class="form-actions">
									<div class="col-md-2 col-lg-3">
										<div id="venmo-button">
											<div  class="venmo-button-container">
												<img src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/venmo_button.svg"
													 class="venmo-button-style"/>
											</div>
										</div>
										<c:url value="/my-account/payment-details"  var="accountPaymentMethodUrl" />
										<a class="btn btn-block btn-default" href="${accountPaymentMethodUrl}">
											<spring:theme code="account.add.paymentMethod.cancel" text="Cancel" />
										</a>
									</div>
									<div class="col-md-2 col-lg-3">
										<button class="btn btn-primary btn-block" id="submit_silentOrderPostForm" type="submit">
											<spring:theme code="account.add.paymentMethod.save" text="Save" />
										</button>
										<div id="mark-paypal-button" class="paypal_button_container btn btn-block"></div>
										<div id="google-pay-button" class="google-pay-button-container google_pay_container"></div>
									</div>
							</div>
						</form:form>
					</div>
					</div>
				</ycommerce:testId>
			</c:if>
	</c:if>
		</div>
	</div>
</div>
<spring:eval expression="@configurationService.configuration.getProperty('braintree.user.action')" var="userAction"/>

<util:importBtSDK
		sdkVersion="3.69.0"
		enablePayPal="${payPalStandardEnabled}"
		enableHostedFields="${hostedFieldsEnable}"
		enableGooglePay="${googlePayEnable}"
		enableVenmo="${venmoEnabled}"
		enableSecure3d="${payPalCheckoutData.secure3d}"/>

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