<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<%-- <script type="text/javascript" src="https://js.braintreegateway.com/web/3.63.0/js/venmo.min.js"></script>

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
</c:choose> --%>

<section id="myAccount">
        <div class="container">
            <div class="row justify-content-center">
                <div id="accountMenu" class="col-lg-3 sticky-lg-top">
                    <h6 class="mb-4">Hello, John!</h6>
                    <div id="accountMobileNav" class="d-block d-lg-none dropdown my-4">
                        <button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="accountMobile" data-bs-toggle="dropdown" aria-expanded="false">
                            Credit Cards
                        </button>
                        <ul class="dropdown-menu" aria-labelledby="accountMobile">
                            <li><a href="#" class="dropdown-item">Orders</a></li>
                            <li><a href="#" class="dropdown-item">Addresses</a></li>
                            <li><a href="#" class="dropdown-item">Change Email</a></li>
                            <li><a href="#" class="dropdown-item">Change Password</a></li>
                            <li><a href="#" class="dropdown-item">Saved Carts</a></li>
                            <li><a href="#" class="dropdown-item">Bookmarks</a></li>
                            <li><a href="#" class="dropdown-item">Verification Documents</a></li>
                            <li><a href="#" class="dropdown-item"><b>Credit Cards</b></a></li>
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
                    <h1>Credit Cards</h1>
                    <hr>
                    <div class="notification notification-tip cc">
                        <p>Active and upcoming orders will charge the card used when you placed your rental. To change the card used, please <a href="#">contact us</a>.</p>
                    </div>
                    <div id="cardList" class="row mt-5">
                    <c:choose>
                    <c:when test="${not empty braintreePaymentInfos}">
                    <c:forEach items="${braintreePaymentInfos}" var="paymentInfo">
                  
                    <c:set var="cardName" value="${fn:escapeXml(paymentInfo.cardType)}"/>
                        <div class="col-md-6 saved-card">
                            <div class="card mb-4"> 
                            <c:choose>
                            <c:when test="${paymentInfo.isDefault eq true }">
                             <div class="badges">
                                    <span class="badge badge-default float-start">Default Card</span>
                              </div> 
                            </c:when>
                            <c:otherwise>
<!--                             	<div class="badges"> -->
<!--                                     <span class="badge badge-outline float-md-end">Set as Default Card</span> -->
<!--                               </div> -->
										<button class="badge badge-default float-start js-set-default-card" data-card-default="true" data-payment-default-id="${paymentInfo.id}"  data-payment-default-token="${paymentInfo.paymentMethodToken}">
												<spring:theme code="text.setDefault" />		
										</button>
							</c:otherwise>
                            </c:choose>
                                
                                <div class="row">
                                    <div class="col-9">
                                        
                                        <span class="gray60">${fn:escapeXml(paymentInfo.cardType)}<br>
                                        ${fn:escapeXml(paymentInfo.cardNumber)}<br>
                                        Exp: ${fn:escapeXml(paymentInfo.expiryMonth)} /
                                                ${fn:escapeXml(paymentInfo.expiryYear)}</span>
                                                 
                                        <p class="card-actions mb-0"><a href="#" class="edit-cc-form" data-id="${paymentInfo.id}">Edit</a><a href="#" class="delete-link"data-payment-id="${paymentInfo.id}"  data-tokan="${paymentInfo.paymentMethodToken}" data-bs-toggle="modal" data-bs-target="#clearCartWarning">Remove</a></p>
                                        
                                        
                                        
                                    </div>
                                    <div class="col-3 text-md-right">
                                        <img src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-${fn:replace(fn:toLowerCase(cardName),' ', '_')}.png" class="cc-image">
                                    </div>
                                </div>    
                            </div>
                        </div>
                        </c:forEach>
                        </c:when>
						 <c:otherwise>
						 </c:otherwise>
                    </c:choose>
                        <div class="col-md-6 saved-card">
                            <div class="card mb-4"> 
                                <div class="row">
                                    <div class="col-9">
                                        <b>PayPal makes it easy!</b>
                                        <span class="gray60">Select the PayPal option during checkout for a fast, secure checkout.</span>
                                    </div>
                                    <div class="col-3 text-md-right">
                                        <img src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/img-paypal.png" class="cc-image">
                                    </div>
                                </div>    
                            </div>
                        </div>
                        <div class="col-12">
                            <!-- <button class="btn btn-primary">Add Credit Card</button> -->
                            <a class="btn btn-primary" href="add-payment-method" >
	                          Add Credit Card
	           			    </a>
                        </div>
                    </div>    
                </div>
            </div>
        </div> 
        <%-- <form id="braintree-payment-default-form" action="${request.contextPath}/my-account/default-credit-card" method="GET">
              <input type="hidden" name="paymentInfoIdDefault" id="paymentInfoIdDefault" value=""/>
              <input type="hidden" name="paymentMethodTokenDefault" id="paymentMethodTokenDefault" value=""/>
              <input type="hidden" name="defaultCard" id="defaultCard"/>
         </form> --%>
         
         <form id="braintree-payment-default-form" action="${request.contextPath}/my-account/edit-payment-method" method="GET">
         
         </form>
         
    <c:url value="/my-account/remove-payment-method-bt" var="removePaymentActionUrl"/>
       <form:form id="removePaymentDetails${paymentInfo.id}" action="${removePaymentActionUrl}" method="post">
        <input type="hidden" id="paymentInfoIdRemove" name="paymentInfoIdRemove" value=""/>
         <input type="hidden" id="paymentMethodTokenRomove"  name="paymentMethodTokenRomove" value=""/>
                               
	<div class="modal fade" id="clearCartWarning" tabindex="-1"
		aria-hidden="true">
		<div class="modal-dialog modal-dialog-centered modal-sm">
			<div class="modal-content">
				<div class="modal-header">
					<h5 class="modal-title">
						<spring:theme
							code="text.account.paymentDetails.deleteLink.title" />
					</h5>
					<button type="button" class="btn-close" data-bs-dismiss="modal"
						aria-label="Close"></button>
				</div>
				<div class="modal-body">
					<p class="body14">
						<spring:theme code="text.account.paymentDetails.deleteLink.message" />
					</p>
					<ycommerce:testId code="paymentDetailsDelete_delete_button" >
					<button type="submit" data-payment-id="${paymentInfo.id}" class="btn btn-primary btn-block my-4 paymentsDeleteBtn">
                           <spring:theme code="text.account.paymentDetails.delete"/>
                    </button>		
					</ycommerce:testId>		
					<p class="text-center mb-0">
						<a href="#" class="lightteal" data-bs-dismiss="modal"
							aria-label="Close"><spring:theme code="text.button.cancel" /></a>
					</p>
				</div>
			</div>
		</div>
	</div>
	
	</form:form>
</section> 
    

    <script>
    var accountPaymentInfoPage = "accountPaymentInfoPage";
    </script>

