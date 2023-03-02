<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="theme" tagdir="/WEB-INF/tags/shared/theme"%>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/desktop/nav"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="common" tagdir="/WEB-INF/tags/desktop/common"%>
<%@ taglib prefix="formElement"
	tagdir="/WEB-INF/tags/desktop/formElement"%>

<c:if test="${not empty ccPaymentInfo}">
	<div id="accountContent" class="col-lg-8 offset-lg-1">
		<h1>Credit Card</h1>
		<hr>
		<div class="row">
			<div class="col-lg-7">
				<b>Edit <img
					src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-cc.png"
					style="max-width: 220px;"></b>
				<ycommerce:testId code="paymentDetailsForm">
					<div class="account-section-content">
						<form:form id="braintree-payment-form"
							modelAttribute="sopPaymentDetailsForm"
							action="${request.contextPath}/my-account/edit-payment-method"
							method="POST">
							<input type="hidden" name="paymentInfoId" id="paymentInfoId"
								value="${selectedPaymentMethodId}" />
							<c:if test="${ccPaymentInfo.cardType!=null}">
								<div class="control-group cardForm" style="dispaly: none;"
									id="braintree-payment-edit-form">
									<!-- Commented card holder related code as of now -->
									<%-- <label for="cardholderName" class="control-label ">
										<spring:theme code="braintree.text.cc.cardholder" />
									</label>
									<div class="controls" >
										<input id="cardholderName" value="" maxlength="175"/>
									</div> --%>
									<div class="controls">
										<input id="number" class="form-control secure testing"
											value="${ccPaymentInfo.cardNumber}" readonly />
									</div>
									<div class="row">
										<div class="col-4">
											<div class="controls">
												<input id="expirationMonth" class="form-control"
													value="${ccPaymentInfo.expiryMonth}" />
											</div>
										</div>
										<div class="col-4">
											<div class="controls">
												<input id="expirationYear" class="form-control"
													value="${ccPaymentInfo.expiryYear}" />
											</div>
										</div>
										<div class="col-4">
											<div class="">
												<input id="cvv" class="form-control" name="cvv" value=""
													placeholder="CVV" type="password" maxlength="4" />
											</div>
										</div>
									</div>
									<c:choose>
										<c:when test="${ccPaymentInfo.defaultPaymentInfo eq true}">
											<input type="checkbox" id="default-card" checked>
											<label for="default-card"><span class="gray80">Default
													card</span></label>
										</c:when>
										<c:otherwise>
											<input type="checkbox" id="default-card">
											<label for="default-card"><span class="gray80">Default
													card</span></label>
										</c:otherwise>
									</c:choose>
								</div>

								<br />

								<input type="hidden" name="expirationDate" id="expirationDate"
									value="" />

								<input type="hidden" name="paypal_email" id="paypal_email" />


								<div class="form-additionals" />


								<input type="hidden" id="isAddressPresent"
									name="isAddressPresent" value="true" />
								<div id="billing-address-saved" class="collapse"
									data-bs-parent="#billingDetails">

									<c:choose>
										<c:when test="${ccPaymentInfo.billingAddress != null}">
											<input type="hidden" name="billingAddressId"
												id="billingAddressId"
												value="${paymentInfo.billingAddress.id}" />

											<!--  Commented billing address related code -->
											<%-- <div class="mt-5" id="billingAddress">
                                    <b>Saved Billing Addresses</b>
                                   
										<b class="mt-4">Saved Billing Addresses</b>
											<div class="dropdown my-2">
																				
											<a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="savedAddresses" data-bs-toggle="dropdown" aria-expanded="false">
														${ccPaymentInfo.billingAddress.formattedAddress }
											</a>
																				
									</div> --%>

											<!-- <a href="#" class="gray80" id="paymentAddNewAddress" data-bs-toggle="collapse" data-bs-target="#billing-address-form-expand" aria-expanded="false" aria-controls="billing-address-form-expand">+ Add a new address</a> -->
										</c:when>
										<%-- <c:when test="${not empty paymentInfoBillingAddress and empty billingAddresses}">
									<div class="mt-5" id="billingAddress">
                                    <b>Saved Billing Addresses</b>
                                   
										<b class="mt-4">Saved Billing Addresses</b>
											<div class="dropdown my-2">
																				
											<a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="savedAddresses" data-bs-toggle="dropdown" aria-expanded="false">
														${paymentInfoBillingAddress.formattedAddress }
											</a>
																				
									</div>
									
									<a href="#" class="gray80" id="paymentAddNewAddress" data-bs-toggle="collapse" data-bs-target="#billing-address-form-expand" aria-expanded="false" aria-controls="billing-address-form-expand">+ Add a new address</a>
									</c:when> --%>
										<%-- <c:when test="${not empty defaultBillingAddress and empty billingAddresses}">
										<b class="mt-4">Saved Billing Addresses</b>
											<div class="dropdown my-2">
																				
											<a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="savedAddresses" data-bs-toggle="dropdown" aria-expanded="false">
															${defaultBillingAddress.formattedAddress }
										    </a>
																				
											</div>
											
												<a href="#" class="gray80" id="paymentAddNewAddress" data-bs-toggle="collapse" data-bs-target="#billing-address-form-expand" aria-expanded="false" aria-controls="billing-address-form-expand">+ Add a new address</a>
									</c:when> --%>
										<%-- <c:otherwise>
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
																		</c:otherwise> --%>
									</c:choose>
								</div>
					</div>
</c:if>
</form:form>
<input type="hidden" name="billingAddressId" id="billingAddressId"
	value="" />
<div id="validationMessage"></div>
<div id="allFieldvalidationMessage"></div>
<div class="text-end mt-4">
	<c:url value="/my-account/payment-details"
		var="accountPaymentMethodUrl" />
	<a class="btn btn-outline" href="${accountPaymentMethodUrl}"> <spring:theme
			code="account.add.paymentMethod.cancel" text="Cancel" />
	</a>

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


</c:if>
</div>

<script>
   var editPaymentMethodsPage = "editPaymentMethodsPage";
</script>