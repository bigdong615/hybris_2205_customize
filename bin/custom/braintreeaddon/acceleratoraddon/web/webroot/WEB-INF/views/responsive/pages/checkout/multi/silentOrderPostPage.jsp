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
<c:url value="/checkout/multi/delivery-method/chooseShipping" var="shippingPageUrl" />
<c:url value="/checkout/multi/payment-method/braintree/reviewSavedPayment" var="reviewSavedPaymentAction" />
<c:if test="${fn:containsIgnoreCase(cartData.deliveryMode.shippingGroup, 'SHIP_UPS_OFFICE') == true or fn:containsIgnoreCase(cartData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == true}">
<c:set var="hideUseShipping" value="hideUseShipping"/>
</c:if>
<c:url value="/checkout/multi/payment-method/reviewSavePoPayment" var="reviewSavePoPaymentAction" />
<c:url value="/cart" var="cartPageUrl" />
<spring:eval
	expression="@configurationService.configuration.getProperty('braintree.store.in.vault')"
	var="storeInVault" />
<c:url value="${currentStepUrl}" var="choosePaymentMethodUrl" />
<c:url value="/cart" var="cart" />
<c:url value="/checkout/multi/delivery-method/chooseShipping" var="deliveryOrPickup" />
<template:page pageTitle="${pageTitle}">
	<section id="cartProcess">
		<div class="container">
			<div id="cartSteps" class="row justify-content-center">
				<div class="col-xl-10">
				    <a href="${cart}" class="text-decoration-none">
                        <span class="step1 complete"><i class="icon-check"></i>
                            <c:choose>
                                <c:when test="${cartData.isRentalCart && cartData.isRetailGearOrder eq false}"><spring:theme code="text.checkout.multi.order.rental"/></c:when>
                                <c:otherwise><spring:theme code="text.checkout.multi.order.UsedGear"/></c:otherwise>
                            </c:choose>
                        </span>
                    </a>
                    <c:choose>
                    <c:when test="${cartData.hasGiftCart}">
                     <span class="step2 complete"><i class="icon-check"></i> Delivery or Pickup</span>
                    </c:when>
                    <c:otherwise>
                     <a href="${deliveryOrPickup}" class="text-decoration-none">
					  <span class="step2 complete"><i class="icon-check"></i> Delivery or Pickup</span>
					</a>
                    </c:otherwise>
                    </c:choose>
					<a href="#" onClick="window.location.reload(true)" class="text-decoration-none">
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
						 <c:if test="${!cartData.hasGiftCart}">
							<c:if test="${cartData.isRentalCart && cartData.isRetailGearOrder eq false}">
							  <p><b>Dates</b>&emsp;<input type="text"
									class="form-control cart-picker" id="litepicker"
									placeholder="<spring:theme code="text.rental.cart.select.date"/>">
							  </p>
							  </c:if>
							</c:if>
							<p class="overline">Pay With</p>
							<div class="accordion" id="paymentOptions">
								<div class="accordion-item payProduct">
									<div class="row">
										<c:if test="${not empty userSelectedPaymentInfo}">
											<input type="hidden" id="isCCPresent" name="isCCPresent" value="true"/>
										</c:if>
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
												
												<ycommerce:testId code="paymentDetailsForm">
													<form:form id="braintree-payment-form"
														name="silentOrderPostForm"
														class="create_update_payment_form"
														modelAttribute="sopPaymentDetailsForm"
														action="${request.contextPath}/braintree/checkout/hop/response"
														method="POST">
														<div id="cardDetails">
															<div id="credit-card-saved" class="collapse show" data-bs-parent="#cardDetails">
																<c:choose>
																	<c:when test="${not empty userSelectedPaymentInfo.cardNumber and empty braintreePaymentInfos}">
																	<b class="mt-4">Saved Credit Cards</b>
																	<div class="dropdown my-2">
																		<button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="savedCards" data-bs-toggle="dropdown" aria-expanded="false">
																			<img src="${userSelectedPaymentInfo.accountHolderName}" style="max-width: 33px; height: auto;"> ${userSelectedPaymentInfo.cardType} &nbsp ${fn:escapeXml(userSelectedPaymentInfo.cardNumber)} &nbsp exp ${fn:escapeXml(userSelectedPaymentInfo.expiryMonth)}/${fn:escapeXml(userSelectedPaymentInfo.expiryYear)}
																		</button>
																	</div>
																		<a href="#" id="addNewCardForm" class="gray80" data-bs-toggle="collapse" data-bs-target="#credit-card-form-expand" aria-controls="credit-card-form-expand">+ Add a new credit card</a>
																	</c:when>
																	<c:otherwise>
																	<c:if test="${not empty braintreePaymentInfos and braintreePaymentInfos.size() > 0}">
																	<b class="mt-4">Saved Credit Cards</b>
																		<div class="dropdown my-2">
																	<button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="savedCards" data-bs-toggle="dropdown" aria-expanded="false">
																		<c:choose>
																			<c:when test="${not empty userSelectedPaymentInfo}">
																				<img src="${userSelectedPaymentInfo.accountHolderName }" style="max-width: 33px; height: auto;"> ${userSelectedPaymentInfo.cardType} &nbsp ${fn:escapeXml(userSelectedPaymentInfo.cardNumber)} &nbsp exp ${fn:escapeXml(userSelectedPaymentInfo.expiryMonth)}/${fn:escapeXml(userSelectedPaymentInfo.expiryYear)}
																			</c:when>
																			<c:otherwise>
																				Select or Enter new card
																			</c:otherwise>
																		</c:choose>
																	</button>
																	<ul class="dropdown-menu savedPaymentList" aria-labelledby="savedCards" id="saved-payment-action">
																		<c:forEach items="${braintreePaymentInfos}" var="paymentInfo" varStatus="status">
																		<c:if test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'CreditCard')}">
																			<li>
																				<button class="dropdown-item" data-id="${paymentInfo.id}" data-nonce="${paymentInfo.paymentMethodNonce}">
																					<img src="${paymentInfo.accountHolderName }" style="max-width: 33px; height: auto;">
																					${paymentInfo.cardType}
																					&nbsp ${fn:escapeXml(paymentInfo.cardNumber)} &nbsp exp ${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)}
																				</button>
																			</li>
																		</c:if>
																		</c:forEach>
																	</ul>
																	
																</div>
																<a href="#" id="addNewCardForm" class="gray80" data-bs-toggle="collapse" data-bs-target="#credit-card-form-expand" aria-controls="credit-card-form-expand">+ Add a new credit card</a>
																</c:if>
																	</c:otherwise>
																</c:choose>
																
																
															</div>
															<div class="collapse" id="credit-card-form-expand" data-bs-parent="#cardDetails">
															<b class="mt-4">Add Your Credit Card</b>
															<div class="mb-5">
																<div class="hostedFields">
																	<div class="control-group cardForm" style="dispaly: none;" id="cardForn">
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
																		<%-- <c:if test="${not empty userSelectedPaymentInfo}">
																			 <input id="userSelectedPaymentInfo_number" type="hidden" value="${userSelectedPaymentInfo.cardNumber}"/>
																			 <input id="userSelectedPaymentInfo_month" type="hidden" value="${userSelectedPaymentInfo.expiryMonth}"/>
																			 <input id="userSelectedPaymentInfo_year" type="hidden" value="${userSelectedPaymentInfo.expiryYear}"/>
																		</c:if> --%>  
																		<%-- <c:if test="${payPalCheckoutData.storeInVault}"> --%>
																		<input type="checkbox" checked class="form-control" id="savePaymentInfo" name="savePaymentInfo"/>
																			<label for="savePaymentInfo">
																				<span class="gray80"><spring:theme code="checkout.multi.sop.savePaymentInfo" /></span>
																			</label> 
																	</div>
																</div>
															</div>
															<a href="#" class="gray80" id="showSavedCard" data-bs-toggle="collapse" data-bs-target="#credit-card-saved" aria-expanded="false" aria-controls="credit-card-saved">+ Use a saved credit card</a>
															</div>
														</div>
														<hr/>
														<div id="billingDetails">	
															
															<input type="hidden" id="isAddressPresent" name="isAddressPresent" value="true"/>
																<div id="billing-address-saved" class="collapse" data-bs-parent="#billingDetails">
																	<c:choose>
																		<c:when test="${not empty paymentInfoBillingAddress and empty billingAddresses}">
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
															<c:if test="${cartData.deliveryItemsQuantity > 0}">
																<div id="useDeliveryAddressData"
																	 data-firstname="${deliveryAddress.firstName}"
																	 data-lastname="${deliveryAddress.lastName}"
																	 data-line1="${deliveryAddress.line1}"
																	 data-line2="${deliveryAddress.line2}"
																	 data-town="${deliveryAddress.town}"
																	 data-postalcode="${deliveryAddress.postalCode}"
																	 data-countryisocode="${empty deliveryAddress.country.isocode ? 'US' : deliveryAddress.country.isocode}"
																	 data-regionisocode="deliveryAddress.region.isocode"
																	 data-email="${deliveryAddress.email}"
																	 data-address-id="${deliveryAddress.id}"></div>
																   	<b class="mt-4 mb-3">Add Your Billing Address</b>
																   	<c:if test="${!cartData.hasGiftCart}">
																   	<input type="checkbox" class="form-control ${hideUseShipping}" id="ccUseDeliveryAddress" name="useDeliveryAddress"/>
																   	<label for="ccUseDeliveryAddress" class="${hideUseShipping}">
																   		<span class="gray80"><spring:theme code="checkout.multi.sop.useMyDeliveryAddress" /></span>
																   	</label>    
																   	</c:if> 
															</c:if> 
															<input type="hidden" name="paypal_email" id="paypal_email" /> 
														    <input type="hidden" name="billTo_country" id="address.country" value="US">
														    
															<div id="billingAddressForm" class="billingAddressForm"></div>
															</div>
															<a href="#" class="gray80" id="showSavedAddresses" data-bs-toggle="collapse" data-bs-target="#billing-address-saved" aria-expanded="false" aria-controls="billing-address-saved">+ Use a saved billing address</a>
														</div>
														<div class="form-additionals"></div>
													</form:form>
												</ycommerce:testId>
												<c:set var="selectedAddressId" value="${defaultBillingAddress.id }"/>
												<c:if test="${not empty paymentInfoBillingAddress}">
													<c:set var="selectedAddressId" value="${paymentInfoBillingAddress.id }"/>
												</c:if>
												<input type="hidden" id="savedBillingAddressId" name="savedBillingAddressId" value="${selectedAddressId}"/>
												<form:form name="submitSavedCardForm" method="POST" id="submitSavedCardForm" action="${reviewSavedPaymentAction}">
													<input type="hidden" id="savedCCCardId" name="savedCCCardId" value="${userSelectedPaymentInfo.id}"/>
													<input type="hidden" id="savedCCCardNonce" name="savedCCCardNonce" value="${userSelectedPaymentInfo.paymentMethodNonce}"/>
												</form:form>
												<form:form name="selectSavedCardForm" method="POST" id="selectSavedCardForm" action="${savedPaymentInfoFormURL}">
													<input type="hidden" id="selectedPaymentMethodId" name="selectedPaymentMethodId" value=""/>
													<input type="hidden" id="selectedPaymentMethodNonce" name="selectedPaymentMethodNonce" value=""/>
												</form:form>
											</div>
											
										</div>
										</div>
									</div>
								</div>
								<!-- Paypal section -->
							<%--	<div class="accordion-item payProduct">
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
								</div> --%>
								<!--BL-623 PO section -->
			<c:if test="${!cartData.hasGiftCart}">
                <c:if test="${cartData.isPOEnabled}">
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
                					    <input type="radio" class="paypalselection" id="paymentMethodPo" name="paymentMethodSelection" value="bt"><label
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
                					<input type="text" class="form-control po-number" name="poNumber" id="poNumber" min="1" max="30" maxlength="30" value="${selectedPoNumber}"
                						placeholder="<spring:theme code="text.payment.page.po.number.placeholder"/>">
                					<input type="text" class="form-control po-number" name="poNotes" id="poNotes" min="1" max="1000" maxlength="1000" value="${selectedPoNotes}"
                						placeholder="<spring:theme code="text.payment.page.po.notes.placeholder"/>">
                					<input type="hidden" id="selectedPoNumber" name="selectedPoNumber" value=""/>
                          <input type="hidden" id="selectedPoNotes" name="selectedPoNotes" value=""/>
                          <input type="hidden" id="poSelected" name="poSelected" value=""/>
                				</form:form>
                				</div>
                			</div>
                		</div>
                	</div>
                </c:if>
			</c:if>				</div>
							<%-- Error message secion --%>
							<c:if test="${cartData.hasGiftCart}">
							<div class="page-loader-new-layout">
							    <img src="${themeResourcePath}/assets/bl-loader.gif" alt="Loading.." title="Loading.." id="new_loading_Img">
							</div>
							</c:if>
							 <c:if test="${!cartData.hasGiftCart}">
							<cart:blGiftCard cartData="${cartData}"/>
							</c:if>
							<!-- Uncomment hr tag to add more sections after hr tag on payment page -->
							<!-- <hr class="my-5"> -->
							<div id="validationMessage"></div>
                            <div id="allFieldvalidationMessage"></div>
							<!-- <hr class="mt-5"> -->
							
							
							<div class="cart-actions">
                               <c:choose>
							    <c:when test="${cartData.hasGiftCart}">
							         <a href="${cartPageUrl}" class="gray80">Back</a>
							         <a href="javascript:void(0)" class="btn btn-sm btn-primary float-end" id="submit_silentOrderPostForm">Continue</a>
							         <a href="#" class="btn btn-sm btn-primary float-end" id="submit_silentOrderSavedForm">Continue</a>
							    </c:when>
							    <c:otherwise>
							         <a href="${shippingPageUrl}" class="gray80"><c:choose> <c:when test="${cartData.isRetailGearOrder eq true}"><spring:theme code="text.newgear.cart.back" /></c:when><c:when test="${cartData.isRentalCart}"><spring:theme code="text.rental.cart.back" /></c:when><c:otherwise><spring:theme code="text.usedGear.cart.back.plp" /></c:otherwise></c:choose></a>
                                     <a href="javascript:void(0)" class="btn btn-sm btn-primary float-end" id="submit_silentOrderPostForm">Continue</a>
                                      <a href="#" class="btn btn-sm btn-primary float-end" id="submit_silentOrderSavedForm">Continue</a>
                               </c:otherwise>
							</c:choose>
							
                            </div>
                        </div>
						<div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">
							<c:choose>
						   <c:when test="${cartData.hasGiftCart}">
						         <cart:blGiftCartPurchaseOrderSummery cartData="${cartData}" emptyCart="${emptyCart}" />
						    </c:when>
						    <c:otherwise>
						             <cart:orderSummery cartData="${cartData}" emptyCart="${emptyCart}" />
						    </c:otherwise>
						</c:choose>
							<c:if test ="${not empty fn:escapeXml(errorMsg)}">
                      <div class="notification notification-error js-promo-error">
                           ${fn:escapeXml(errorMsg)}
                      </div>
              </c:if>
							<%-- <div class="notification notification-warning">This is a cart warning.</div> --%>
							  <c:if test="${not empty cartData.potentialOrderPromotions}">
                   <c:forEach items="${cartData.potentialOrderPromotions}" var="promotion">
                   <c:if test="${fn:containsIgnoreCase(promotion.promotionData.code, 'free_shipping')}">
                      <div class="notification notification-tip truck"><spring:theme code="text.free.shipping.promo.applied.message"/></div>
                   </c:if>
                   </c:forEach>
               </c:if>
                              <div class="notification notification-error d-none"id="errorMessages_voucher" ></div>

               <c:if test="${cartData.isRetailGearOrder eq false && cartData.isRentalCart eq true}">
                     <div class="notification notification-tip check"><spring:theme code="text.shipping.change.or.cancellation.message"/></div>
               </c:if>
            </div>
					</div>
				</div>	
			</div>
		</div>	
	</section>
	<cart:damageWaiverInfo/>
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
    
    <script type="text/javascript"
		src="https://js.braintreegateway.com/web/3.69.0/js/client.min.js"></script>
	<script type="text/javascript"
		src="https://js.braintreegateway.com/web/3.69.0/js/hosted-fields.min.js"></script>
	<script type="text/javascript"
		src="https://js.braintreegateway.com/web/3.69.0/js/data-collector.min.js"></script>
	<script type="text/javascript" src="https://js.braintreegateway.com/web/3.69.0/js/paypal.min.js"></script>
    <script type="text/javascript" src="https://js.braintreegateway.com/web/3.69.0/js/paypal-checkout.min.js"></script>
</template:page>
