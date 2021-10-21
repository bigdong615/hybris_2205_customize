<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="util"
	tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/util"%>

<jsp:include page="../../../messages/braintreeErrorMessages.jsp" />
<spring:eval
	expression="@configurationService.configuration.getProperty('braintree.store.in.vault')"
	var="storeInVault" />


<div id="accountContent" class="col-lg-8 offset-lg-1">
	<h1>Credit Card</h1>
	<hr>
	<div class="row">
		<div class="col-lg-7">
			<b>Add New <img
				src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-cc.png"
				style="max-width: 220px;"></b>
			<ycommerce:testId code="paymentDetailsForm">
				<div class="account-section-content">
					<form:form id="braintree-payment-form"
						modelAttribute="sopPaymentDetailsForm"
						action="${request.contextPath}/my-account/add-payment-method"
						method="POST">

						<input type="hidden" name="orderId" id="orderId" value="${orderCode}"/>
						<div class="hostedFields">

							<div class="control-group cardForm" style="dispaly: none;"
								id="cardForn">

								<div id="number" class="controls form-control secure testing"></div>
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
								<input type="checkbox" id="default-card" checked><label for="default-card"><span class="gray80">Default card</span></label> 
							</div>
						</div>
						<br />



						<input type="hidden" name="paypal_email" id="paypal_email" />
						<%-- <input type="hidden" name="selectedAddressCode" id="selectedAddressCode" value="${selectedAddressCode}"/> --%>

						<div class="form-additionals" />
						<input type="hidden" id="isAddressPresent" name="isAddressPresent"
							value="true" />
						<div id="billing-address-saved" class="collapse fixed-line-height"
							data-bs-parent="#billingDetails">

							<c:choose>
								<c:when
									test="${not empty paymentInfoBillingAddress and empty billingAddresses}">
									<div class="mt-5" id="billingAddress">
										<b>Saved Billing Addresses</b> <b class="mt-4">Saved
											Billing Addresses</b>
										<div class="dropdown my-2">

											<a
												class="btn btn-block btn-outline dropdown-toggle text-start"
												href="#" role="button" id="savedAddresses"
												data-bs-toggle="dropdown" aria-expanded="false">
												${paymentInfoBillingAddress.formattedAddress } </a>

										</div>

										<a href="#" class="gray80" id="paymentAddNewAddress"
											data-bs-toggle="collapse"
											data-bs-target="#billing-address-form-expand"
											aria-expanded="false"
											aria-controls="billing-address-form-expand">+ Add a new
											address</a>
								</c:when>
								<c:when
									test="${not empty defaultBillingAddress and empty billingAddresses}">
									<b class="mt-4">Saved Billing Addresses</b>
									<div class="dropdown my-2">

										<a
											class="btn btn-block btn-outline dropdown-toggle text-start"
											href="#" role="button" id="savedAddresses"
											data-bs-toggle="dropdown" aria-expanded="false">
											${defaultBillingAddress.formattedAddress } </a>

									</div>

									<a href="#" class="gray80" id="paymentAddNewAddress"
										data-bs-toggle="collapse"
										data-bs-target="#billing-address-form-expand"
										aria-expanded="false"
										aria-controls="billing-address-form-expand">+ Add a new
										address</a>
								</c:when>
								<c:otherwise>
									<c:if
										test="${not empty billingAddresses and billingAddresses.size() > 0 }">

										<b class="mt-4">Saved Billing Addresses</b>
										<div class="dropdown my-2">
											<a
												class="btn btn-block btn-outline dropdown-toggle text-start"
												href="#" role="button" id="savedAddresses"
												data-bs-toggle="dropdown" aria-expanded="false"> <c:choose>
													<c:when test="${not empty paymentInfoBillingAddress.formattedAddress }">
															${paymentInfoBillingAddress.formattedAddress }
												    </c:when>
													<c:otherwise>
															Select Saved Billing Address
													</c:otherwise>
												</c:choose>
											</a>
											<ul class="dropdown-menu selectSavedBillingAddress"
												aria-labelledby="savedAddresses">
												<c:if
													test="${not empty defaultBillingAddress.formattedAddress }">

													<li><a class="dropdown-item" href="#"
														data-id="${defaultBillingAddress.id }"
														data-address="${defaultBillingAddress.formattedAddress }">${defaultBillingAddress.formattedAddress }</a></li>
												</c:if>
												<c:forEach items="${billingAddresses}" var="billingAddress">
													<c:if
														test="${empty defaultBillingAddress or fn:containsIgnoreCase(billingAddress.id, defaultBillingAddress.id) == false}">
														<li><a class="dropdown-item" href="#"
															data-id="${billingAddress.id }"
															data-address="${billingAddress.formattedAddress }">${billingAddress.formattedAddress }</a></li>
													</c:if>
												</c:forEach>

											</ul>
										</div>
										<a href="#" class="gray80" id="paymentAddNewAddress"
											data-bs-toggle="collapse"
											data-bs-target="#billing-address-form-expand"
											aria-expanded="false"
											aria-controls="billing-address-form-expand">+ Add a new
											address</a>
									</c:if>
								</c:otherwise>
							</c:choose>
						</div>

						<div class="collapse" id="billing-address-form-expand">
							<div class="mb-5">

								<div id="useDeliveryAddressData" data-countryisocode="US"></div>
								<b class="mt-4 mb-3">Add Your Billing Address</b>


								<div id="billingAddressForm" class="billingAddressForm cart"></div>
							</div>
							<a href="#" class="gray80" id="showSavedAddresses"
								data-bs-toggle="collapse"
								data-bs-target="#billing-address-saved" aria-expanded="false"
								aria-controls="billing-address-saved">+ Use a saved billing
								address</a>
						</div>
				</div>


				</form:form>
				<input type="hidden" id="savedBillingAddressId"
					name="savedBillingAddressId" value="" />
				<div id="validationMessage"></div>
				<div id="allFieldvalidationMessage"></div>
				<div class="text-end mt-4">
					<c:url value="/my-account/payment-details"
						var="accountPaymentMethodUrl" />
					<a class="btn btn-outline" href="${accountPaymentMethodUrl}"> 
					<spring:theme code="account.add.paymentMethod.cancel"
							text="Cancel" /> </a>

					<button class="btn btn-primary" id="submit_silentOrderPostForm"
						type="submit">
						<spring:theme code="account.add.paymentMethod.save" text="Save" />
					</button>
				</div>
		</div>
	</div>
	</ycommerce:testId>
</div>
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
		enableSecure3d="${payPalCheckoutData.secure3d}"/>--%>
<script type="text/javascript"
	src="https://js.braintreegateway.com/web/3.69.0/js/client.min.js"></script>
<script type="text/javascript"
	src="https://js.braintreegateway.com/web/3.69.0/js/hosted-fields.min.js"></script>
<script type="text/javascript"
	src="https://js.braintreegateway.com/web/3.69.0/js/data-collector.min.js"></script>
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