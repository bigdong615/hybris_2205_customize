<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>
<%@ taglib prefix="multiCheckout" tagdir="/WEB-INF/tags/responsive/checkout/multi" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement" %>
<%@ taglib prefix="sec" uri="http://www.springframework.org/security/tags" %>
<%@ taglib prefix="address" tagdir="/WEB-INF/tags/responsive/address" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="util" tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/util" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement" %>

<%--<jsp:include page="../../../../messages/braintreeErrorMessages.jsp"/>--%>

<spring:eval expression="@configurationService.configuration.getProperty('braintree.store.in.vault')" var="storeInVault"/>
<c:url value="${currentStepUrl}" var="choosePaymentMethodUrl"/>
<c:url value="/cart" var="cart" />
<c:url value="/checkout/multi/delivery-method/chooseShipping" var="deliveryOrPickup" />
<template:page pageTitle="${pageTitle}" >

    <%--<div class="row">
        <div class="col-sm-12" id="globalMessages"></div>
        <div class="col-sm-6">
            <div class="checkout-headline">
                <span class="glyphicon glyphicon-lock"></span>
                <spring:theme code="checkout.multi.secure.checkout"/>
            </div>
            <multiCheckout:checkoutSteps checkoutSteps="${checkoutSteps}" progressBarId="${progressBarId}">
                <jsp:body>
                    <div class="checkout-paymentmethod">
                        <div class="checkout-indent">

                            <div class="headline"><spring:theme code="checkout.multi.paymentMethod"/></div>
                            <div class="no-payment-methods-message headline hidden"><spring:theme code="checkout.multi.paymentMethod.noPaymentMethods"/></div>

                            <c:choose>
                                <c:when test="${applePayEnabled}">
                                    <div style="overflow: auto;" id="applepay" class="hide">
                                        <input id="paymentMethodApplePay" type="radio"
                                               name="paymentMethodSelection" value="applePay"
                                               class="paypalselection"/>
                                        <div class="applepay-image-container" id="applepay-container">
                                            <div class="cmsimage">
                                                <img class="apple-pay-image"
                                                     src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/apple-pay.png"
                                                     alt="Buy with ApplePay" class="payment-logo"/>
                                            </div>
                                        </div>
                                    </div>
                                    <br>
                                </c:when>
                                <c:otherwise>
                                    <div style="overflow: auto;"></div>
                                </c:otherwise>
                            </c:choose>
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
                            <c:choose>
                                <c:when test="${payPalStandardEnabled}">
                                    <div style="overflow: auto;" id="paypal-checkbox">
                                        <input id="paymentMethodPayPal" type="radio"
                                               name="paymentMethodSelection" value="paypal"
                                               class="paypalselection" checked="true"/>
                                        <div class="paypalimage" id="paypal-container">
                                            <div class="cmsimage">
                                                <img src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-medium.png"
                                                     alt="Buy now with PayPal" class="payment-logo"/>
                                            </div>
                                        </div>
                                        <div id="text" class="paypalurl">
                                            <a style="padding-left: 10px;"
                                               href="https://www.paypal.com/webapps/mpp/paypal-popup"
                                               title="How PayPal Works"
                                               onclick="javascript:window.open('https://www.paypal.com/webapps/mpp/paypal-popup','WIPaypal','toolbar=no, location=no, directories=no, status=no, menubar=no, scrollbars=yes, resizable=yes, width=1060, height=700'); return false;"><spring:theme
                                                    code="braintree.text.what.is.paypal"/>?</a>
                                        </div>
                                    </div>
                                    <br>
                                </c:when>
                                <c:otherwise>
                                    <div style="overflow: auto;"></div>
                                </c:otherwise>
                            </c:choose>
                            <c:choose>
                                <c:when test="${hostedFieldsEnable}">
                                    <div style="overflow: auto;" id="braintree-container">
                                        <input id="paymentMethodBT" type="radio"
                                               name="paymentMethodSelection" value="bt"
                                               class="paypalselection"/>
                                        <c:if test="${not empty paymentsImagesURL}">
                                            <c:forEach items="${paymentsImagesURL}" var="url">
                                                <img src="${url.value}" alt="${url.key}"/>
                                            </c:forEach>
                                        </c:if>
                                        <input type="hidden" value="false" class="text"
                                               name="paypal_is_valid" id="paypal_is_valid">
                                    </div>
                                    <br>
                                </c:when>
                                <c:otherwise>
                                    <div style="overflow: auto;"></div>
                                </c:otherwise>
                            </c:choose>
                            <c:choose>
                                <c:when test="${localPaymentsEnabled && !empty localPaymentMethods}">
                                    <div style="overflow: auto;" id="localpayments">
                                        <input id="paymentMethodLpm" type="radio"
                                               name="paymentMethodSelection" value="lpm"
                                               class="paypalselection"/>
                                        <div class="lpm-radio">
                                             <label>Buy with Local Payment Method</label>
                                        </div>
                                    </div>
                                </c:when>
                                <c:otherwise>
                                    <div style="overflow: auto;"></div>
                                </c:otherwise>
                            </c:choose>
                            <c:choose>
                                <c:when test="${venmoEnabled}">
                                    <div style="overflow: auto;" id="venmo" class="hide">
                                        <input id="paymentMethodVenmo" type="radio"
                                               name="paymentMethodSelection" value="venmo"
                                               class="paypalselection"/>
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

                                <form:form id="braintree-payment-form" name="silentOrderPostForm"
                                           class="create_update_payment_form"

                                           modelAttribute="sopPaymentDetailsForm"
                                           action="${request.contextPath}/braintree/checkout/hop/response"
                                           method="POST">
                                    <div id="venmo-button" class="venmo-container hide">
                                        <img src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/venmo_button.svg"
                                             class="venmo-button-style"/>
                                    </div>
                                    <div class="hostedFields">
                                        <div class="headline">
                                            <spring:theme
                                                    code="checkout.multi.paymentMethod.addPaymentDetails.paymentCard"/>
                                        </div>
                                        <div class="description">
                                            <spring:theme
                                                    code="checkout.multi.paymentMethod.addPaymentDetails.enterYourCardDetails"/>
                                        </div>
                                        <div class="control-group cardForm" style="dispaly: none;" id="cardForn">
                                            <label for="cardholderName" class="control-label ">
                                                <spring:theme code="braintree.text.cc.cardholder"/>
                                            </label>
                                            <div class="controls">
                                                <input id="cardholderName" value="" maxlength="175"/>
                                            </div>
                                            <label for="number" class="control-label ">
                                                <spring:theme code="braintree.text.cc.number"/></label>
                                            <div id="number" class="controls"></div>
                                            <label for="cvv" class="control-label "><spring:theme
                                                    code="braintree.text.cc.cvv"/></label>
                                            <div id="cvv" class="controls"></div>
                                            <label for="expiration-date" class="control-label "><spring:theme
                                                    code="braintree.text.cc.expiration.date"/></label>
                                            <div id="expiration-date" class="controls"></div>
                                        </div>
                                    </div>
                                    <br/>
                                    <c:if test="${payPalCheckoutData.storeInVault}">
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
                                    </c:if>

                                    <div class="billingAddressData">

                                        <div class="headline clear">
                                            <spring:theme
                                                    code="checkout.multi.paymentMethod.addPaymentDetails.billingAddress"/>
                                        </div>

                                        <c:if test="${cartData.deliveryItemsQuantity > 0}">
                                            <div id="useDeliveryAddressData"
                                                 data-firstname="${deliveryAddress.firstName}"
                                                 data-lastname="${deliveryAddress.lastName}"
                                                 data-line1="${deliveryAddress.line1}"
                                                 data-line2="${deliveryAddress.line2}"
                                                 data-town="${deliveryAddress.town}"
                                                 data-postalcode="${deliveryAddress.postalCode}"
                                                 data-countryisocode="${deliveryAddress.country.isocode}"
                                                 data-regionisocode="${deliveryAddress.region.isocodeShort}"
                                                 data-address-id="${deliveryAddress.id}"></div>
                                            <formElement:formCheckbox path="useDeliveryAddress"
                                                                      idKey="useDeliveryAddress"
                                                                      labelKey="checkout.multi.sop.useMyDeliveryAddress"
                                                                      tabindex="11"/>
                                        </c:if>

                                        <input type="hidden" name="paypal_email" id="paypal_email"/>
                                        <input type="hidden"
                                               value="${silentOrderPageData.parameters['billTo_email']}"
                                               class="text" name="billTo_email" id="billTo_email">
                                        <address:billAddressFormSelector
                                                supportedCountries="${countries}" regions="${regions}"
                                                tabindex="12"/>
                                    </div>

                                    <div class="form-additionals"></div>

                                    <p>
                                        <spring:theme
                                                code="checkout.multi.paymentMethod.seeOrderSummaryForMoreInformation"/>
                                    </p>

                                    <button type="submit" id="submit_silentOrderPostForm"
                                            class="btn btn-primary btn-block checkout-next">
                                        <spring:theme code="checkout.multi.paymentMethod.continue"
                                                      text="Next"/>
                                    </button>

                                    <div id="paypalButtonError"></div>
                                    <div id="google-pay-button" class="google-pay-button-container google_pay_container"></div>
                                    <div id="mark-paypal-button" class="paypal_button_container btn btn-block"></div>
                                    <div id="lpm-buttons" class="paypal_button_container">
                                        <c:forEach items="${localPaymentMethods}" var="localPaymentMethod">
                                            <div id="${localPaymentMethod.code}" class="btn-local-payment-methods btn btn-block">
                                                <div>
                                                   <img class="payment-button" src="${localPaymentMethod.image.url}"
                                                   alt="${localPaymentMethod.image.altText}">
                                                </div>
                                                <label>${localPaymentMethod.name}</label>
                                            </div>
                                        </c:forEach>
                                    </div>
                                </form:form>
                            </ycommerce:testId>

                        </div>
                    </div>

                    <c:if test="${not empty braintreePaymentInfos}">
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
                    </c:if>

                </jsp:body>
            </multiCheckout:checkoutSteps>
        </div>

        <div class="col-sm-6 hidden-xs">
            <multiCheckout:checkoutOrderDetails cartData="${cartData}" showDeliveryAddress="true" showPaymentInfo="false"
                                                showTaxEstimate="false" showTax="true"/>
        </div>

        <div class="col-sm-12 col-lg-12">
            <cms:pageSlot position="SideContent" var="feature" element="div" class="checkout-help">
                <cms:component component="${feature}"/>
            </cms:pageSlot>
        </div>
    </div>

    <spring:eval expression="@configurationService.configuration.getProperty('braintree.user.action')" var="userAction"/>

    <util:importBtSDK
            sdkVersion="3.69.0"
            enablePayPal="${payPalStandardEnabled}"
            enableHostedFields="${hostedFieldsEnable}"
            enableGooglePay="${googlePayEnable}"
            enableVenmo="${venmoEnabled}"
            enableLocalPayment="${localPaymentsEnabled}"
            enableApplePay="${payPalCheckoutData.applePayEnabled}"
            enableSecure3d="${payPalCheckoutData.secure3d}"/>

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
    </script>--%>

    <section id="cartProcess">
        <div class="container">
            <div id="cartSteps" class="row justify-content-center">
                <div class="col-xl-10">
                    <a href="${cart}" class="text-decoration-none">
                        <span class="step1 complete"><i class="icon-check"></i> Your Rental</span>
                    </a>
                    <a href="${deliveryOrPickup}" class="text-decoration-none">
                        <span class="step2 complete"><i class="icon-check"></i> Delivery or Pickup</span>
                    </a>
                    <a href="#" class="window.location.reload(true)" class="text-decoration-none">
                        <span class="step3 active"><i class="number">3</i> Payment</span>
                    </a>
                    <span class="step4"><i class="number">4</i> Review</span>
                </div>
            </div>
            <div class="row justify-content-center">
                <div class="col-xl-10">
                    <div class="row">
                        <div id="order" class="col-lg-7">
                            <h1>Payment</h1>
                            <hr>
                            <p><b>Dates</b>&emsp;<input type="text" class="form-control cart-picker" id="litepicker" placeholder="Apr 20 - Apr 23" autocomplete="off"></p>
                            <cart:blGiftCard cartData="${cartData}"/>
                            <p class="overline">Pay With</p>
                            <ycommerce:testId code="paymentDetailsForm">

                            <form:form id="braintree-payment-form" name="silentOrderPostForm"
                                       class="create_update_payment_form"

                                       modelAttribute="sopPaymentDetailsForm"
                                       action="${request.contextPath}/braintree/checkout/hop/response"
                                       method="POST">
                            <div class="paymentOption">
                                <div class="row">
                                    <div class="col-1 pt-2">
                                        <input type="radio" id="payment-cc" name="payment"><label for="payment-cc"></label>
                                    </div>
                                    <div class="col-11">
                                        <%--<b>Credit Card <img src="assets/payment-cc.png" style="height: 44px; width: auto;"></b>--%>
                                        <formElement:formInputBox idKey="card_accountNumber" labelKey="payment.cardNumber" path="card_accountNumber" inputCSS="form-control" mandatory="true" tabindex="3" autocomplete="off" />
                                        <div class="row">
                                            <div class="col-4">
                                                <div class="dropdown">
                                                    <%--<a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="card-month" data-bs-toggle="dropdown" aria-expanded="false">
                                                        Month
                                                    </a>--%>
                                                   <%-- <ul class="dropdown-menu" aria-labelledby="card-month">
                                                        <li><a class="dropdown-item" href="#">01 Jan</a></li>
                                                        <li><a class="dropdown-item" href="#">02 Feb</a></li>
                                                        <li><a class="dropdown-item" href="#">03 Mar</a></li>
                                                        <li><a class="dropdown-item" href="#">04 Apr</a></li>
                                                        <li><a class="dropdown-item" href="#">05 May</a></li>
                                                        <li><a class="dropdown-item" href="#">06 Jun</a></li>
                                                        <li><a class="dropdown-item" href="#">07 Jul</a></li>
                                                        <li><a class="dropdown-item" href="#">08 Aug</a></li>
                                                        <li><a class="dropdown-item" href="#">09 Sep</a></li>
                                                        <li><a class="dropdown-item" href="#">10 Oct</a></li>
                                                        <li><a class="dropdown-item" href="#">11 Nov</a></li>
                                                        <li><a class="dropdown-item" href="#">12 Dec</a></li>
                                                    </ul>--%>
                                                    <formElement:formSelectBox idKey="StartMonth" selectCSSClass="form-control" labelKey="payment.month" path="card_startMonth" mandatory="true" skipBlank="false" skipBlankMessageKey="payment.month" items="${months}" tabindex="4"/>
                                                </div>
                                            </div>
                                            <div class="col-4">
                                                <div class="dropdown">
                                                   <%-- <a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="card-year" data-bs-toggle="dropdown" aria-expanded="false">
                                                        Year
                                                    </a>--%>
                                                    <%--<ul class="dropdown-menu" aria-labelledby="card-year">
                                                        <li><a class="dropdown-item" href="#">2021</a></li>
                                                        <li><a class="dropdown-item" href="#">2022</a></li>
                                                        <li><a class="dropdown-item" href="#">2023</a></li>
                                                        <li><a class="dropdown-item" href="#">2024</a></li>
                                                        <li><a class="dropdown-item" href="#">2025</a></li>
                                                        <li><a class="dropdown-item" href="#">2026</a></li>
                                                        <li><a class="dropdown-item" href="#">2027</a></li>
                                                    </ul>--%>

                                                    <formElement:formSelectBox idKey="StartYear" selectCSSClass="form-control" labelKey="payment.year" path="card_startYear" mandatory="true" skipBlank="false" skipBlankMessageKey="payment.year" items="${startYears}" tabindex="7"/>
                                                </div>
                                            </div>
                                            <div class="col-4">
                                                <div class="cvv-holder">
                                                    <%--<input type="text" class="form-control" placeholder="CVV">
                                                    <div class="cvv-tooltip"><img  data-bs-toggle="tooltip" data-bs-placement="top" title="Tooltip on top" src="assets/icon-support.svg"></div>--%>
                                                        <formElement:formInputBox idKey="card_cvNumber" labelKey="payment.cvn" path="card_cvNumber" inputCSS="form-control" mandatory="true" tabindex="8" />

                                                </div>
                                            </div>
                                        </div>
                                        </form:form>
                                        </ycommerce:testId>
                                        <hr>
                                        <b class="mt-4">Billing Address</b>
                                        <div class="mb-5">
                                            <form>
                                                <input type="text" class="form-control" id="first-name" placeholder="First Name">
                                                <input type="text" class="form-control" id="last-name" placeholder="Last Name">
                                                <input type="text" class="form-control" id="street" placeholder="Street Address">
                                                <input type="text" class="form-control" id="street-2" placeholder="Apt., Suite, Floor">
                                                <input type="text" class="form-control" id="city" placeholder="City">
                                                <input type="text" class="form-control float-start" id="zip" placeholder="Zip" style="width: calc(40% - 15px);">
                                                <div class="select-wrapper float-end" style="width: 60%;">
                                                    <select class="form-control" id="state" style="width:100%;">
                                                        <option value="" disabled selected>State</option>
                                                        <option value="AL">Alabama</option>
                                                        <option value="AK">Alaska</option>
                                                        <option value="AZ">Arizona</option>
                                                        <option value="AR">Arkansas</option>
                                                        <option value="CA">California</option>
                                                        <option value="CO">Colorado</option>
                                                        <option value="CT">Connecticut</option>
                                                        <option value="DE">Delaware</option>
                                                        <option value="DC">District Of Columbia</option>
                                                        <option value="FL">Florida</option>
                                                        <option value="GA">Georgia</option>
                                                        <option value="HI">Hawaii</option>
                                                        <option value="ID">Idaho</option>
                                                        <option value="IL">Illinois</option>
                                                        <option value="IN">Indiana</option>
                                                        <option value="IA">Iowa</option>
                                                        <option value="KS">Kansas</option>
                                                        <option value="KY">Kentucky</option>
                                                        <option value="LA">Louisiana</option>
                                                        <option value="ME">Maine</option>
                                                        <option value="MD">Maryland</option>
                                                        <option value="MA">Massachusetts</option>
                                                        <option value="MI">Michigan</option>
                                                        <option value="MN">Minnesota</option>
                                                        <option value="MS">Mississippi</option>
                                                        <option value="MO">Missouri</option>
                                                        <option value="MT">Montana</option>
                                                        <option value="NE">Nebraska</option>
                                                        <option value="NV">Nevada</option>
                                                        <option value="NH">New Hampshire</option>
                                                        <option value="NJ">New Jersey</option>
                                                        <option value="NM">New Mexico</option>
                                                        <option value="NY">New York</option>
                                                        <option value="NC">North Carolina</option>
                                                        <option value="ND">North Dakota</option>
                                                        <option value="OH">Ohio</option>
                                                        <option value="OK">Oklahoma</option>
                                                        <option value="OR">Oregon</option>
                                                        <option value="PA">Pennsylvania</option>
                                                        <option value="RI">Rhode Island</option>
                                                        <option value="SC">South Carolina</option>
                                                        <option value="SD">South Dakota</option>
                                                        <option value="TN">Tennessee</option>
                                                        <option value="TX">Texas</option>
                                                        <option value="UT">Utah</option>
                                                        <option value="VT">Vermont</option>
                                                        <option value="VA">Virginia</option>
                                                        <option value="WA">Washington</option>
                                                        <option value="WV">West Virginia</option>
                                                        <option value="WI">Wisconsin</option>
                                                        <option value="WY">Wyoming</option>
                                                    </select>
                                                </div>
                                                <input type="text" class="form-control" id="email" placeholder="Email">
                                                <input type="text" class="form-control mb-3" id="phone" placeholder="Phone Number">
                                                <input type="checkbox" class="form-control" id="save-address"><label for="save-address"><span class="gray80">Save address</span></label>
                                            </form>
                                        </div>
                                        <b class="mt-4">Saved Billing Addresses</b>
                                        <div class="dropdown my-2">
                                            <a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="savedAddresses" data-bs-toggle="dropdown" aria-expanded="false">
                                                123 Broadway, New York, NY 10012
                                            </a>
                                            <ul class="dropdown-menu" aria-labelledby="savedAddresses">
                                                <li><a class="dropdown-item" href="#">123 Broadway, New York, NY 10012</a></li>
                                            </ul>
                                        </div>
                                        <a href="#" class="gray80">+ Add a new address</a>
                                        <hr>
                                        <b class="mt-4">Order Notes</b>
                                        <input type="text" class="form-control" id="notes" placeholder="client name, job #, etc.">
                                        <input type="checkbox" class="form-control" id="newsletter"><label for="newsletter"><span class="gray80">Receive BL newsletter</span></label>
                                    </div>
                                </div>
                            </div>
                            <div class="paymentOption">
                                <div class="row">
                                    <div class="col-1 pt-2">
                                        <input type="radio" id="payment-paypal" name="payment"><label for="payment-paypal"></label>
                                    </div>
                                    <div class="col-11">
                                        <b>Paypal <img src="assets/payment-paypal.png" style="height: 44px; width: auto;"></b>
                                    </div>
                                </div>
                            </div>

                            <div class="cart-actions">
                                <a href="#" class="gray80">Back to renting</a>
                                <button type="submit" id="submit_silentOrderPostForm"
                                        class="btn btn-primary btn-block checkout-next">
                                    <spring:theme code="checkout.multi.paymentMethod.continue"
                                                  text="Next"/>
                                </button>
                            </div>
                            </form>

                        </div>
                        <div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">
                            <%--<div id="orderSummary" class="card">
                                <h5>Order Summary</h5>
                                <hr>
                                <p><b>Dates</b>&emsp;<input type="text" class="form-control cart-picker" id="summary-litepicker" placeholder="Apr 20 - Apr 23"></p>
                                <hr>
                                <table id="costSummary">
                                    <tbody>
                                    <tr>
                                        <td class="gray80">Rental Cost</td>
                                        <td class="text-end">$98.00</td>
                                    </tr>
                                    <tr>
                                        <td class="gray80">Damage Waiver <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></td>
                                        <td class="text-end">$98.00</td>
                                    </tr>
                                    <tr>
                                        <td class="gray80">Shipping*</td>
                                        <td class="text-end">–</td>
                                    </tr>
                                    <tr>
                                        <td class="gray80">Est. Taxes*</td>
                                        <td class="text-end">–</td>
                                    </tr>
                                    <tr class="discount">
                                        <td>Discount</td>
                                        <td class="text-end">–$25.00</td>
                                    </tr>
                                    <tr class="total">
                                        <td>Total</td>
                                        <td class="text-end">$160.23</td>
                                    </tr>
                                    </tbody>
                                </table>
                                <div class="input-group my-3">
                                    <input type="text" class="form-control success" value="BL123XYZ">
                                    <div class="input-group-append">
                                        <button class="btn btn-secondary disabled" type="button">Apply</button>
                                    </div>
                                </div>
                                <p class="body14"><span class="gray60">BL123XYZ</span> <a href="#">remove</a><span class="float-end">- $5.00</span></p>
                                <p class="body14"><span class="gray60">Gift Card</span> <a href="#">remove</a><span class="float-end">- $20.00</span></p>
                                <p class="body14"><span class="gray60">Holiday</span> <a href="#">remove</a><span class="float-end">2 days free</span></p>
                            </div>--%>
                                <cart:orderSummery cartData="${cartData}" emptyCart="${emptyCart}"/>
                                <c:if test ="${not empty fn:escapeXml(errorMsg)}">
                                                              <div class="notification notification-error">
                                                                      ${fn:escapeXml(errorMsg)}
                                                               </div>
                                                             </c:if>
                                                             <div class="notification notification-error d-none"id="errorMessages_voucher" />
                            <div class="order-actions my-4"><a href="#" alt="Print Order"><i class="icon-print"></i></a><a href="#"><i class="icon-save" alt="Save Order"></i></a><a href="#" class="disabled" alt="Trash Order"><i class="icon-trash"></i></a></div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </section>

</template:page>
