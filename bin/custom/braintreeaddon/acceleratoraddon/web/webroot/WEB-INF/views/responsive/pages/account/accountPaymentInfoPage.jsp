<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

        <script type="text/javascript" src="https://js.braintreegateway.com/web/3.63.0/js/venmo.min.js"></script>

<c:set var="noBorder" value=""/>
<c:if test="${not empty braintreePaymentInfos}">
    <c:set var="noBorder" value="no-border"/>
</c:if>

<div class="account-section-header ${noBorder}">
    <spring:theme code="text.account.paymentDetails" />
    <c:if test="${addPaymentMethodShow eq false}">
        <c:if test="${payPalConfigurationData.storeInVault}">
	        <div class="account-section-header-add pull-right">
	            <a href="add-payment-method" >
	                <spring:theme code="text.account.profile.paymentMethod.add" text="Add Payment Method"/>
	            </a>
	        </div>
        </c:if>
    </c:if>
</div>

<c:choose>
    <c:when test="${not empty braintreePaymentInfos}">
        <div class="account-paymentdetails account-list">
            <div class="account-cards card-select">
                <div class="row">
                    <c:forEach items="${braintreePaymentInfos}" var="paymentInfo">
                        <input type="text" value="${paymentInfo.subscriptionId}" class=" paymentType hide">
                        <div class="col-xs-12 col-sm-6 col-md-4 card hide">
                            <ul class="pull-left">
                                <c:choose>
                                    <c:when test="${paymentInfo.subscriptionId eq 'BrainTreePayPalExpress' or paymentInfo.subscriptionId eq 'PayPalAccount'}">
                                        <br><spring:theme code="paymentMethod.type.PayPal" />
                                        <br>${fn:escapeXml(paymentInfo.payer)}
                                        <br><img height="38" width="56"
                                            src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-small.png" alt="PayPal icon"/>
                                    </c:when>
                                    <c:when test="${paymentInfo.subscriptionId eq 'VenmoAccount'}">
                                        <br><spring:theme code="paymentMethod.type.Venmo" />
                                        <br>${fn:escapeXml(paymentInfo.payer)}
                                        <br><img height="38" width="56"
                                                 src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/venmo_acceptance_mark.svg" alt="Venmo icon"/>
                                    </c:when>
                                    <c:when test="${paymentInfo.subscriptionId eq 'AndroidPayCard'}">
                                        <br><spring:theme code="paymentMethod.type.GooglePay" />
                                        <br>${fn:escapeXml(paymentInfo.payer)}
                                        <br><img height="38" width="56"
                                                 src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/googlePay_mark.png" alt="GooglePay icon"/>
                                        <br>${fn:escapeXml(paymentInfo.cardNumber)}
                                        <br>${fn:escapeXml(paymentInfo.cardType)}
                                        <br><spring:theme code="text.expires" text="Expires" />
                                        <c:if test="${not empty paymentInfo.expiryMonth}">
                                            <span> ${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)}</span>
                                        </c:if>
                                    </c:when>
                                    <c:otherwise>
                                        <c:if test="${not empty paymentInfo.cardholderName}">
                                            <br>${fn:escapeXml(paymentInfo.cardholderName)} <c:if test="${paymentInfo.defaultPaymentInfo}" >&nbsp;(<spring:theme code="text.default" />)</c:if>
                                        </c:if>
                                        <br>${fn:escapeXml(paymentInfo.cardNumber)}
                                        <br>${fn:escapeXml(paymentInfo.cardType)}
                                        <br><spring:theme code="text.expires" text="Expires" />
                                        <c:if test="${not empty paymentInfo.expiryMonth}">
                                            <span> ${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)}</span>
                                        </c:if>
                                        <c:if test="${not empty paymentInfo.accountHolderName}">
                                            <br><img src="${fn:escapeXml(paymentInfo.accountHolderName)}" alt="Card Type" />
                                        </c:if>
                                    </c:otherwise>
                                </c:choose>
                                <c:if test="${paymentInfo.billingAddress ne null}">
                                    <li>${fn:escapeXml(paymentInfo.billingAddress.line1)}</li>
                                    <li>${fn:escapeXml(paymentInfo.billingAddress.town)}&nbsp;${fn:escapeXml(paymentInfo.billingAddress.region.isocodeShort)}</li>
                                    <li>${fn:escapeXml(paymentInfo.billingAddress.country.name)}&nbsp;${fn:escapeXml(paymentInfo.billingAddress.postalCode)}</li>
                                </c:if>
                            </ul>
                            <div class="account-cards-actions pull-left">
                                <ycommerce:testId code="paymentDetails_deletePayment_button" >
                                    <a class="action-links removePaymentDetailsButton" href="#" data-payment-id="${paymentInfo.id}" data-popup-title="<spring:theme code="text.account.paymentDetails.delete.popup.title"/>">
                                        <span class="glyphicon glyphicon-remove"></span>
                                    </a>
                                </ycommerce:testId>
                            </div>
                            <c:if test="${not paymentInfo.defaultPaymentInfo}" >
                                <c:url value="/my-account/set-default-payment-details" var="setDefaultPaymentActionUrl"/>
                                <form:form id="setDefaultPaymentDetails${paymentInfo.id}" action="${setDefaultPaymentActionUrl}" method="post">
                                    <input type="hidden" name="paymentInfoId" value="${paymentInfo.id}"/>
                                    <ycommerce:testId code="paymentDetails_setAsDefault_button" >
                                        <button type="submit" class="account-set-default-address">
                                            <spring:theme code="text.setDefault" text="Set as Default" />
                                        </button>
                                    </ycommerce:testId>
                                </form:form>
                            </c:if>
                        </div>
                    
                        <div class="display-none">
                            <div id="popup_confirm_payment_removal_${paymentInfo.id}" class="account-address-removal-popup">
                                <spring:theme code="text.account.paymentDetails.delete.following"/>
                                <div class="address">
                                    <c:choose>
                                        <c:when test="${paymentInfo.subscriptionId eq 'BrainTreePayPalExpress' or paymentInfo.subscriptionId eq 'PayPalAccount'}">
                                            <br>PayPal
                                            <br>${fn:escapeXml(paymentInfo.billingAddress.email)}
                                            <br><img height="38" width="56"
                                                     src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-small.png"
                                                     alt="PayPal icon"/>
                                        </c:when>
                                        <c:when test="${paymentInfo.subscriptionId eq 'VenmoAccount'}">
                                            <br><spring:theme code="paymentMethod.type.Venmo" />
                                            <br>${fn:escapeXml(paymentInfo.payer)}
                                            <br><img height="38" width="56"
                                                     src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/venmo_acceptance_mark.svg" alt="Venmo icon"/>
                                        </c:when>
                                        <c:when test="${paymentInfo.subscriptionId eq 'AndroidPayCard'}">
                                            <br><spring:theme code="paymentMethod.type.GooglePay" />
                                            <br>${fn:escapeXml(paymentInfo.payer)}
                                            <br><img height="38" width="56"
                                                     src="${contextPath}/_ui/addons/braintreeaddon/responsive/common/images/googlePay_mark.png" alt="GooglePay icon"/>
                                            <br>${fn:escapeXml(paymentInfo.cardNumber)}
                                            <br>${fn:escapeXml(paymentInfo.cardType)}
                                            <br><spring:theme code="text.expires" text="Expires" />
                                            <c:if test="${not empty paymentInfo.expiryMonth}">
                                                <span> ${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)}</span>
                                            </c:if>
                                        </c:when>
                                        <c:otherwise>
                                            <c:if test="${not empty paymentInfo.cardholderName}">
                                                <br>${fn:escapeXml(paymentInfo.cardholderName)}
                                            </c:if>
                                            <br>${fn:escapeXml(paymentInfo.cardNumber)}
                                            <br>${fn:escapeXml(paymentInfo.cardType)}
                                            <br><spring:theme code="text.expires" text="Expires" />
                                            <c:if test="${not empty paymentInfo.expiryMonth}">
                                                ${fn:escapeXml(paymentInfo.expiryMonth)} /
                                                ${fn:escapeXml(paymentInfo.expiryYear)}
                                            </c:if>
                                            <c:if test="${not empty paymentInfo.accountHolderName}">
                                                <br><img
                                                    src="${fn:escapeXml(paymentInfo.accountHolderName)}"
                                                    alt="Card Type" />
                                            </c:if>
                                        </c:otherwise>
                                    </c:choose>
                                    <c:if test="${paymentInfo.billingAddress ne null}">
                                        <br>${fn:escapeXml(paymentInfo.billingAddress.line1)}
                                        <br>${fn:escapeXml(paymentInfo.billingAddress.town)}&nbsp;${fn:escapeXml(paymentInfo.billingAddress.region.isocodeShort)}
                                        <br>${fn:escapeXml(paymentInfo.billingAddress.country.name)}&nbsp;${fn:escapeXml(paymentInfo.billingAddress.postalCode)}
                                    </c:if>
                                </div>
                                <c:url value="/my-account/remove-payment-method-bt" var="removePaymentActionUrl"/>
                                <form:form id="removePaymentDetails${paymentInfo.id}" action="${removePaymentActionUrl}" method="post">
                                    <input type="hidden" name="paymentInfoId" value="${paymentInfo.id}"/>
                                    <input type="hidden" name="paymentMethodToken" value="${paymentInfo.paymentMethodToken}"/>
                                    <br>
                                    <div class="modal-actions">
                                        <div class="row">
                                            <ycommerce:testId code="paymentDetailsDelete_delete_button" >
                                                <div class="col-xs-12 col-sm-6 col-sm-push-6">
                                                    <button type="submit" class="btn btn-default btn-primary btn-block paymentsDeleteBtn">
                                                        <spring:theme code="text.account.paymentDetails.delete"/>
                                                    </button>
                                                </div>
                                            </ycommerce:testId>
                                            <div class="col-xs-12 col-sm-6 col-sm-pull-6">
                                                <a class="btn btn-default closeColorBox paymentsDeleteBtn btn-block" data-payment-id="${paymentInfo.id}">
                                                    <spring:theme code="text.button.cancel" />
                                                </a>
                                            </div>
                                        </div>
                                    </div>
                                </form:form>
			                 </div>
                        </div>
                    </c:forEach>
                </div>
            </div>
        </div>
    </c:when>
    <c:otherwise>
        <div class="account-section-content content-empty">
            <spring:theme code="text.account.paymentDetails.noPaymentInformation" />
        </div>
    </c:otherwise>
</c:choose>

    <script>
    var venmoEnabled = ${venmoEnabled};
    var environment = "${payPalConfigurationData.environment}";
    var clientToken = "${client_token}";
    var googleMerchantId = "${googleMerchantId}";
    var googlePayCountryCode = "${googlePayCountryCode}";
    var googlePayEnabled = ${googlePayEnable};
    var accountPaymentInfoPage = "accountPaymentInfoPage";
    </script>

