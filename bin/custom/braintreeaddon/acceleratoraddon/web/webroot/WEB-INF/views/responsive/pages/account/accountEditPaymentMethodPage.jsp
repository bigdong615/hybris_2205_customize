<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="theme" tagdir="/WEB-INF/tags/shared/theme" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/desktop/nav" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="common" tagdir="/WEB-INF/tags/desktop/common" %>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/desktop/formElement" %>
<%-- <div class="span-20 append-1">

<div class="account-section-content">
   <div class="back-link border">
   <c:url value="/my-account/payment-details"  var="accountPaymentMethodUrl" />
      <a class="addressBackBtn" href="${accountPaymentMethodUrl}">
            <span class="glyphicon glyphicon-chevron-left"></span>
         </a>
      <span class="label"><spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.editPaymentMethod" /></span>
   </div>
</div>
</div> --%>

   <c:if test="${not empty ccPaymentInfo}">
      <%-- <div class="account-section">
         <form:form id="braintree-payment-form" action="${request.contextPath}/my-account/edit-payment-method" method="POST">
            <input type="hidden" name="paymentInfoId" id="paymentInfoId" value="${selectedPaymentMethodId}"/>
            <c:if test="${ccPaymentInfo.cardType!=null}">
               <div style="float: right;">
                  <spring:theme code="form.required" text="Fields marked * are required"/>
               </div>
               <br>
               <div class="account-section-content  account-section-content-small">
                  <div class="control-group cardForm" style="dispaly: none;" id="braintree-payment-edit-form">
                     <label for="cardholderName" class="control-label ">
                        <spring:theme code="braintree.text.cc.cardholder" />
                     </label>
                     <div class="controls" >
                        <c:choose>
                           <c:when test="${not empty cardholder}">
                              <input id="cardholderName" name="cardholder"  value="${cardholder}" placeholder="Cardholder Name" maxlength="175"/>
                           </c:when>
                           <c:otherwise>
                              <input id="cardholderName" name="cardholder"  value="${ccPaymentInfo.cardholderName}" placeholder="Cardholder Name" maxlength="175"/>
                           </c:otherwise>
                        </c:choose>
                     </div>
                     <label for="number" class="control-label ">
                        <spring:theme code="braintree.text.cc.number"/>
                     </label>
                     <div class="controls" >
                        <input id="number" value="${ccPaymentInfo.cardNumber}" readonly/>
                     </div>
                     <label for="expiration-date" class="control-label ">
                        <spring:theme code="braintree.text.cc.expiration.date"/>
                        *
                     </label>
                     <div class="controls" >
                        <c:choose>
                           <c:when test="${not empty expirationDate}">
                              <input id="expiration-date"  name="expirationDate" value="${expirationDate}" placeholder="MM/YYYY" />
                           </c:when>
                           <c:otherwise>
                              <input id="expiration-date"  name="expirationDate" value="${ccPaymentInfo.expiryMonth}/${ccPaymentInfo.expiryYear}" placeholder="MM/YYYY" />
                           </c:otherwise>
                        </c:choose>
                     </div>
                     <label for="cvv" class="control-label ">
                        <spring:theme code="braintree.text.cc.cvv" />
                     </label>
                     <div class="" >
                        <input id="cvv" name="cvv" value="" placeholder="3 or 4 digit" type="password" maxlength="4"/>
                     </div>
                  </div>
               </div>
            </c:if >
            <br>
            <div class="account-section-header">
               <spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.billingAddress" />
            </div>
            <div id="address-item">
               <c:if test="${ccPaymentInfo.billingAddress!=null}">
                  <input type="hidden" name="billingAddressId" id="billingAddressId" value="${paymentInfo.billingAddress.id}"/>
                  <br/><span id="title">${fn:escapeXml(ccPaymentInfo.billingAddress.title)}</span>&nbsp;<span id="firstName">${fn:escapeXml(ccPaymentInfo.billingAddress.firstName)}</span>&nbsp;<span id="lastName">${fn:escapeXml(ccPaymentInfo.billingAddress.lastName)}</span>
                  <br/><span id="line1">${fn:escapeXml(ccPaymentInfo.billingAddress.line1)}</span>
                  <br/><span id="line2">${fn:escapeXml(ccPaymentInfo.billingAddress.line2)}</span>
                  <br/><span id="town">${fn:escapeXml(ccPaymentInfo.billingAddress.town)}&nbsp;</span> <span id="postalCode">${fn:escapeXml(ccPaymentInfo.billingAddress.postalCode)}</span>
                  <br/><span id="country-name">${fn:escapeXml(ccPaymentInfo.billingAddress.country.name)}</span>
                  <br/><span id="region-name">${fn:escapeXml(ccPaymentInfo.billingAddress.region.name)}</span>
               </c:if>
            </div>
            <br/>
            <c:if test="${not empty deliveryAddresses}">
               <button id="viewAddressBook" class="btn btn-primary js-address-book" type="button">
                  <spring:theme code="account.payment.selectOther" text="Select new Address"/>
               </button>
            </c:if>
            <br/>
            <div class="account-section-content">
               <div class="col-xs-12 col-sm-5 col-md-4 col-lg-3 col-sm-push-5 col-sm-offset-2 col-md-push-4 col-lg-push-3 col-md-offset-4 col-lg-offset-6">
                  <button class="btn btn-primary btn-block change_address_button show_processing_message" type="submit">
                     <spring:theme code="account.add.paymentMethod.save" text="Save" />
                  </button>
               </div>
               <div class="col-xs-12 col-sm-5 col-md-4 col-lg-3 col-sm-pull-5 col-md-pull-4 col-lg-pull-3">
                  <c:url value="/my-account/payment-details"  var="accountPaymentMethodUrl" />
                  <a class="btn btn-block btn-default" href="${accountPaymentMethodUrl}">
                     <spring:theme code="account.add.paymentMethod.cancel" text="Cancel" />
                  </a>
               </div>
            </div>
         </form:form>
         <div id="savedAddressListHolder" class="clear">
               <div id="addressbook">
                  <div class="headline">
                     <spring:theme code="account.payment.addressBook" text="Address Book"/>
                  </div>
                  <div class="addressList">
                     <c:forEach items="${deliveryAddresses}" var="deliveryAddress">
                        <div class="addressEntry">
                           <input type="hidden" name="selectedAddressCode" id="selectedAddressCode" value="${deliveryAddress.id}"/>
                           <br>${fn:escapeXml(deliveryAddress.title)}&nbsp; ${fn:escapeXml(deliveryAddress.firstName)}&nbsp; ${fn:escapeXml(deliveryAddress.lastName)}
                           <br>${fn:escapeXml(deliveryAddress.line1)}
                           <br>${fn:escapeXml(deliveryAddress.line2)}
                           <br>${fn:escapeXml(deliveryAddress.town)}&nbsp; ${fn:escapeXml(deliveryAddress.postalCode)}
                           <br>${fn:escapeXml(deliveryAddress.country.name)}
                           <c:if test="${not empty deliveryAddress.region.name}">&nbsp; ${fn:escapeXml(deliveryAddress.region.name)}</c:if>
                           <br>
                           <button id="resolveAddressButton" class="btn btn-primary btn-block" onclick="receiveNewAddressData(${deliveryAddress.id})">
                              <spring:theme code="account.payment.address.useThisAddress" text="Use this address"/>
                           </button>
                        </div>
                     </c:forEach>
                  </div>
               </div>
         </div>
      </div> --%>
      
                
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
								   action="${request.contextPath}/my-account/edit-payment-method" method="POST">
								   <input type="hidden" name="paymentInfoId" id="paymentInfoId" value="${selectedPaymentMethodId}"/>
							<c:if test="${ccPaymentInfo.cardType!=null}">
							   
							
								<%-- <div class="account-section-header">
									<spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.paymentCard" text="Card Details"/>
								</div> --%>
								<%-- <div class="description">
									<spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.enterYourCardDetails" text="Please enter your card details for payment"/></br>
								</div> --%>
								<div class="control-group cardForm" style="dispaly: none;" id="braintree-payment-edit-form">
									<%-- <label for="cardholderName" class="control-label ">
										<spring:theme code="braintree.text.cc.cardholder" />
									</label>
									<div class="controls" >
										<input id="cardholderName" value="" maxlength="175"/>
									</div> --%>
									<%-- <label for="number" class="controls form-control secure testing ">
										<spring:theme code="braintree.text.cc.number" /></label> --%>
									<div class="controls" >
									<input id="number" class="form-control secure testing" value="${ccPaymentInfo.cardNumber}" readonly/>
									</div>
											<div class="row">
												<div class="col-4">
													<div class="controls">
													<input id="expirationMonth" class="form-control"  value="${ccPaymentInfo.expiryMonth}" />
													</div>
												</div>
												<div class="col-4">
													<div  class="controls">
													<input id="expirationYear" class="form-control"  value="${ccPaymentInfo.expiryYear}" />
													</div>
												</div>
												<div class="col-4">
													<div class="">
													<input id="cvv"class="form-control" name="cvv" value="" placeholder="3 or 4 digit" type="password" maxlength="4"/>
													</div>
												</div>
											</div>
											<input type="checkbox" id="default-card" checked><label for="default-card"><span class="gray80">Default card</span></label>
										</div>
							
							<br/>
							
							 <input type="hidden" name="expirationDate" id="expirationDate" value="${ccPaymentInfo.expiryMonth}/${ccPaymentInfo.expiryYear}"/>

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
									<c:when test="${ccPaymentInfo.billingAddress != null}">
									 <input type="hidden" name="billingAddressId" id="billingAddressId" value="${paymentInfo.billingAddress.id}"/>
									<div class="mt-5" id="billingAddress">
                                    <b>Saved Billing Addresses</b>
                                   
										<b class="mt-4">Saved Billing Addresses</b>
											<div class="dropdown my-2">
																				
											<a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="savedAddresses" data-bs-toggle="dropdown" aria-expanded="false">
														${ccPaymentInfo.billingAddress.formattedAddress }
											</a>
																				
									</div>
									
									<a href="#" class="gray80" id="paymentAddNewAddress" data-bs-toggle="collapse" data-bs-target="#billing-address-form-expand" aria-expanded="false" aria-controls="billing-address-form-expand">+ Add a new address</a>
									</c:when>
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
						</c:if>	
						</form:form>
						 <input type="hidden" name="billingAddressId" id="billingAddressId" value=""/>
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
 
      
   </c:if>
</div>

<script>
   var editPaymentMethodsPage = "editPaymentMethodsPage";
</script>