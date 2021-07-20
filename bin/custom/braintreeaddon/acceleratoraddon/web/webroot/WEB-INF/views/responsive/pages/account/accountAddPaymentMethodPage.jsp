<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="util" tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/util" %>

<jsp:include page="../../../messages/braintreeErrorMessages.jsp" />
<spring:eval expression="@configurationService.configuration.getProperty('braintree.store.in.vault')" var="storeInVault"/>
<%-- <div class="container">
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
							   class="paypalselection" style="visibility: visible;position: static;" checked="true"/>
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
		</div> --%>
	<section id="myAccount">
        <div class="container">
            <div class="row justify-content-center">
                <div id="accountMenu" class="col-lg-3 sticky-lg-top">
                    <h6 class="mb-4">Hello, John!</h6>
                    <div id="accountMobileNav" class="d-block d-lg-none dropdown my-4">
                        <button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="accountMobile" data-bs-toggle="dropdown" aria-expanded="false">
                            Credit Card
                        </button>
                        <ul class="dropdown-menu" aria-labelledby="accountMobile">
                            <li><a href="#" class="dropdown-item">Orders</a></li>
                            <li><a href="#" class="dropdown-item">Addresses</a></li>
                            <li><a href="#" class="dropdown-item">Change Email</a></li>
                            <li><a href="#" class="dropdown-item">Change Password</a></li>
                            <li><a href="#" class="dropdown-item">Saved Carts</a></li>
                            <li><a href="#" class="dropdown-item">Bookmarks</a></li>
                            <li><a href="#" class="dropdown-item">Verification Documents</a></li>
                            <li><a href="#" class="dropdown-item cc-click"><b>Credit Cards</b></a></li>
                        </ul>
                    </div>
                    <div class="d-none d-lg-block">
                        <p><a href="#">Orders</a></p>
                        <hr>
                        <p><a href="#">Addresses</a></p>
                        <hr>
                        <p><a href="#">Change Email</a></p>
                        <hr>
                        <p><a href="#">Change Password</a></p>
                        <hr>
                        <p><a href="#">Saved Carts</a></p>
                        <hr>
                        <p><a href="#">Bookmarks</a></p>
                        <hr>
                        <p><a href="#">Verification Documents</a></p>
                        <hr>
                        <p><a href="#" class="active">Credit Cards</a></p>
                    </div>
                </div>
                <div id="accountContent" class="col-lg-8 offset-lg-1">
                    <h1>Credit Card</h1>
                    <hr>
                    <div class="row">
                        <div class="col-lg-7">
                            <b>Add New <img src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-cc.png" style="max-width: 220px; "></b>
                       <ycommerce:testId code="paymentDetailsForm">
					<div class="account-section-content">
						<form:form id="braintree-payment-form"
									modelAttribute="sopPaymentDetailsForm"
								   action="${request.contextPath}/my-account/add-payment-method" method="POST">
							<div class="hostedFields">
								<%-- <div class="account-section-header">
									<spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.paymentCard" text="Card Details"/>
								</div> --%>
								<%-- <div class="description">
									<spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.enterYourCardDetails" text="Please enter your card details for payment"/></br>
								</div> --%>
								<div class="control-group cardForm" style="dispaly: none;" id="cardForn">
									<%-- <label for="cardholderName" class="control-label ">
										<spring:theme code="braintree.text.cc.cardholder" />
									</label>
									<div class="controls" >
										<input id="cardholderName" value="" maxlength="175"/>
									</div> --%>
									<%-- <label for="number" class="controls form-control secure testing ">
										<spring:theme code="braintree.text.cc.number" /></label> --%>
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
											<input type="checkbox" id="default-card"><label for="default-card"><span class="gray80">Default card</span></label>
										</div>
							</div>
							<br/>
							
							

							<input type="hidden" name="paypal_email" id="paypal_email"/>
							<%-- <input type="hidden" name="selectedAddressCode" id="selectedAddressCode" value="${selectedAddressCode}"/> --%>
							
							<div class="form-additionals"/>
							
									<%-- <div class="col-md-2 col-lg-3">
										
										<c:url value="/my-account/payment-details"  var="accountPaymentMethodUrl" />
										<a class="btn btn-block btn-default" href="${accountPaymentMethodUrl}">
											<spring:theme code="account.add.paymentMethod.cancel" text="Cancel" />
										</a>
									</div> --%>
									<input type="hidden" id="isAddressPresent" name="isAddressPresent" value="true"/>
									<div id="billing-address-saved" class="collapse" data-bs-parent="#billingDetails">
								
									<c:choose>
											<c:when test="${not empty paymentInfoBillingAddress and empty billingAddresses}">
									<div class="mt-5" id="billingAddress">
                                    <b>Saved Billing Addresses</b>
                                   
										<b class="mt-4">Saved Billing Addresses</b>
											<div class="dropdown my-2">
																				
											<a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="savedAddresses" data-bs-toggle="dropdown" aria-expanded="false">
														${paymentInfoBillingAddress.formattedAddress }
											</a>
																				
									</div>
									
									<a href="#" class="gray80" id="paymentAddNewAddress" data-bs-toggle="collapse" data-bs-target="#billing-address-form-expand" aria-expanded="false" aria-controls="billing-address-form-expand">+ Add a new address</a>
									</c:when>
									<c:when test="${not empty defaultBillingAddress and empty billingAddresses}">
										<b class="mt-4">Saved Billing Addresses</b>
											<div class="dropdown my-2">
																				
											<a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="savedAddresses" data-bs-toggle="dropdown" aria-expanded="false">
															${defaultBillingAddress.formattedAddress }
										    </a>
																				
											</div>
											
												<a href="#" class="gray80" id="paymentAddNewAddress" data-bs-toggle="collapse" data-bs-target="#billing-address-form-expand" aria-expanded="false" aria-controls="billing-address-form-expand">+ Add a new address</a>
									</c:when>
									<c:otherwise>
																		<c:if test="${not empty billingAddresses and billingAddresses.size() > 0 }">
																		
																			<b class="mt-4">Saved Billing Addresses</b>
																	<div class="dropdown my-2">
																		<a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="savedAddresses" data-bs-toggle="dropdown" aria-expanded="false">
																			<c:choose>
																				<c:when test="${not empty paymentInfoBillingAddress.formattedAddress }">
																					${paymentInfoBillingAddress.formattedAddress }
																				</c:when>
																				<c:when test="${not empty defaultBillingAddress.formattedAddress }">
																					${defaultBillingAddress.formattedAddress }
																				</c:when>
																				<c:otherwise>
																					Select Saved Billing Address
																				</c:otherwise>
																			</c:choose>
																		</a>
																		<ul class="dropdown-menu selectSavedBillingAddress" aria-labelledby="savedAddresses">
																		<c:if test="${not empty defaultBillingAddress.formattedAddress }">
																			<li><a class="dropdown-item" href="#" data-id="${defaultBillingAddress.id }" data-address="${defaultBillingAddress.formattedAddress }">${defaultBillingAddress.formattedAddress }</a></li>
																		</c:if>																		
																			<c:forEach items="${billingAddresses}" var="billingAddress">
																			<c:if test="${empty defaultBillingAddress or fn:containsIgnoreCase(billingAddress.id, defaultBillingAddress.id) == false}">
																				<li><a class="dropdown-item" href="#" data-id="${billingAddress.id }" data-address="${billingAddress.formattedAddress }">${billingAddress.formattedAddress }</a></li>
																			</c:if>
																			</c:forEach>
																		
																		</ul>
																	</div>
																	<a href="#" class="gray80" id="paymentAddNewAddress" data-bs-toggle="collapse" data-bs-target="#billing-address-form-expand" aria-expanded="false" aria-controls="billing-address-form-expand">+ Add a new address</a>
																	</c:if>
																		</c:otherwise>
																	</c:choose>	
                                </div> 
                                
                                <div class="collapse" id="billing-address-form-expand">
														<div class="mb-5">
															
																 <div id="useDeliveryAddressData"
																	 <%-- data-firstname="${deliveryAddress.firstName}"
																	 data-lastname="${deliveryAddress.lastName}"
																	 data-line1="${deliveryAddress.line1}"
																	 data-line2="${deliveryAddress.line2}"
																	 data-town="${deliveryAddress.town}"
																	 data-postalcode="${deliveryAddress.postalCode}" --%>
																	 data-countryisocode="US"
																	 <%-- data-regionisocode="deliveryAddress.region.isocode"
																	 data-email="${deliveryAddress.email}"
																	 data-address-id="${deliveryAddress.id}" --%>></div> 
																   	<b class="mt-4 mb-3">Add Your Billing Address</b>
																   	<%-- <input type="checkbox" class="form-control ${hideUseShipping}" id="ccUseDeliveryAddress" name="useDeliveryAddress"/> --%>
																   	<%-- <label for="ccUseDeliveryAddress" class="${hideUseShipping}">
																   		<span class="gray80"><spring:theme code="checkout.multi.sop.useMyDeliveryAddress" /></span>
																   	</label> --%>     
															
															<!-- <input type="hidden" name="paypal_email" id="paypal_email" /> 
														    <input type="hidden" name="billTo_country" id="address.country" value="US"> -->
														    
															<div id="billingAddressForm" class="billingAddressForm"></div>
															</div>
															<a href="#" class="gray80" id="showSavedAddresses" data-bs-toggle="collapse" data-bs-target="#billing-address-saved" aria-expanded="false" aria-controls="billing-address-saved">+ Use a saved billing address</a>
														</div>
                                </div>
									<%-- <div class="text-end mt-4">
									    <c:url value="/my-account/payment-details"  var="accountPaymentMethodUrl" />
                                       <button class="btn btn-outline"><spring:theme code="account.add.paymentMethod.cancel" text="Cancel" /></button>
                                       
                                       <button class="btn btn-primary" id="submit_silentOrderPostForm" type="submit">
											<spring:theme code="account.add.paymentMethod.save" text="Save" />
										</button>
                                </div> --%>
							
						</form:form>
						<input type="hidden" id="savedBillingAddressId" name="savedBillingAddressId" value=""/>
						<div id="validationMessage"></div>
                        <div id="allFieldvalidationMessage"></div>
						<div class="text-end mt-4">
									    <c:url value="/my-account/payment-details"  var="accountPaymentMethodUrl" />
                                       <button class="btn btn-outline"><spring:theme code="account.add.paymentMethod.cancel" text="Cancel" /></button>
                                       
                                       <button class="btn btn-primary" id="submit_silentOrderPostForm" type="submit">
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
    </section> 
	

<spring:eval expression="@configurationService.configuration.getProperty('braintree.user.action')" var="userAction"/>

<%--<util:importBtSDK
		sdkVersion="3.69.0"
		enablePayPal="${payPalStandardEnabled}"
		enableHostedFields="${hostedFieldsEnable}"
		enableGooglePay="${googlePayEnable}"
		enableVenmo="${venmoEnabled}"
		enableSecure3d="${payPalCheckoutData.secure3d}"/>--%>
<script type="text/javascript" src="https://js.braintreegateway.com/web/3.69.0/js/client.min.js"></script>
<script type="text/javascript" src="https://js.braintreegateway.com/web/3.69.0/js/hosted-fields.min.js"></script>
<script type="text/javascript" src="https://js.braintreegateway.com/web/3.69.0/js/data-collector.min.js"></script>
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