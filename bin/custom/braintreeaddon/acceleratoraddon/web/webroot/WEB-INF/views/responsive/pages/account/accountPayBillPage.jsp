<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="account" tagdir="/WEB-INF/tags/addons/blassistedservicestorefront/order" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<div id="accountContent" class="col-lg-5 offset-lg-1">
                    <h1>Bill Pay</h1>
                    <div class="extend-order">
                        <div class="row">
                            <div class="col-12 mb-3">
                                <h5>Past Order</h5>
                            </div>
                        </div>
                        <div class="row">
                        <c:if test="${orderData.isRentalCart}">
                            <div class="col-5 col-md-5">
                             <c:if test="${orderData.isRentalActive eq true}">
                                               <p class="lightteal mb-0">
                                                        <b>${orderData.rentalStartDate}</b>
                                               </p>
                                           </c:if>
                                           <c:if test="${orderData.isRentalActive eq false}">
                                                  <p class="mb-0">
                                                    <b>${orderData.rentalStartDate}</b>
                                                  </p>
                                           </c:if>
                                <p class="body14">Rental Starts</p>
                            </div>
                            <div class="col-2 col-md-1 text-center">
                                <img class="rental-arrow" src="${themeResourcePath}/assets/icon-arrow.svg">
                            </div>
                            <div class="col-5 col-md-5">
                                <c:if test="${orderData.isRentalActive eq true}">
                                        <p class="lightteal mb-0"><b>${orderData.rentalEndDate}</b></p>
                                        </c:if>
                                        <c:if test="${orderData.isRentalActive eq false}">
                                          <p class="mb-0"><b>${orderData.rentalEndDate}</b></p>
                                        </c:if>
                                <p class="body14">Rental Ends</p>
                            </div>
                         </c:if>   
                            <div class="col-12 mt-4">
                                <div class="row">
                                    <div class="col-5 col-md-4">
                                        <p class="body14">Order #<br>
                                        Date Placed<br>
                                        Rental Days<br>    
                                        Cost</p>
                                    </div>
                                    <div class="col-7 col-md-8">
                                        <p class="body14 gray60">#${fn:escapeXml(orderData.code)}<br>
                                        ${orderData.orderedFormatDate}<br>
                                       ${orderData.totalRentalDays} days<br>    
                                       <format:price priceData="${orderData.totalPriceWithTax}"/></p>
                                    </div>
                                </div>    
                            </div>
                            <c:if test="${not empty orderData.extendOrderEntrie}">
                               <div class="col-12 mt-4">
                                 <b><spring:theme code="text.myaccount.extend.order.details"/></b>
                                   <c:forEach items="${orderData.extendOrderEntrie}" var="extendOrder">
                                        <div class="row">
                                           <div class="col-5 col-md-4">
                                              <p class="body14"><spring:theme code="text.order.extend.text"/><br>
                                                    <spring:theme code="text.myaccount.extend.order.added.time"/><br>
                                                    <spring:theme code="text.myaccount.extend.order.cost"/><br>
                                                    <spring:theme code="text.myaccount.order.rental.damege.waiver"/></p>
                                           </div>
                                           <div class="col-7 col-md-8">
                                               <p class="body14 gray60">
                                                 ${extendOrder.extendOrderEndDate}<br>
                                                 ${extendOrder.extendOrderDaysWithoutPrevOrder}<br>
                                                 <format:blPrice priceData="${extendOrder.extendOrderCost}"/></br>
                                                <c:choose>
                                                <c:when test="${extendOrder.extendOrderDamageWaiverCost.value > 0}">
                                                 <format:blPrice priceData="${extendOrder.extendOrderDamageWaiverCost}"/>
                                                </c:when>
                                                <c:otherwise>
                                                $0.00
                                                </c:otherwise>
                                                </c:choose>
                                               </p>
                                           </div>
                                        </div>
                                   </c:forEach>
                               </div>
                             </c:if>
                            <h5 class="mt-5 mb-4">Billed Items</h5>
                            <div class="col-12">
                            <c:forEach items="${orderData.entries}" var="cartEntry" >
                                <div class="row mb-4 product-block">
                                <c:url var="productUrl" value="/rent/product/${cartEntry.product.code}"/>
                                              <c:if test="${!orderData.isRentalCart}">
                                                <c:url var="productUrl" value="/buy/product/${cartEntry.product.code}"/>
                                              </c:if>
                                    <div class="col-3 text-center"><a href="${productUrl}"> <product:productPrimaryImage product="${cartEntry.product}" format="thumbnail"/> </a></div>
                                    <div class="col-9 mt-3">
                                        <p class="gray80 body14">
                                            <b class="gray100"><a href="${productUrl}" style="text-decoration: none"> ${cartEntry.product.name}</b></a></b>
                                            <spring:theme code="text.myaccount.order.your.rental.qty"/> ${cartEntry.quantity}<br>
                                           <c:if test="${orderData.isRentalCart}">
                               		        <c:choose>
                               								<c:when test="${cartEntry.gearGuardProFullWaiverSelected}">
                               									 <spring:theme code="text.myaccount.order.damage.waiver.gear.plus"/><br>
                               							  </c:when>
                               								<c:when test="${cartEntry.gearGuardWaiverSelected}">
                               								   <spring:theme code="text.myaccount.order.damage.waiver.gear"/><br>
                               								</c:when>
                               								<c:otherwise>
                               										 <spring:theme code="text.myaccount.order.damage.waiver.gear.no"/><br>
                               								</c:otherwise>
                               						</c:choose>
                               						</c:if>
                                            Total <format:price priceData="${cartEntry.totalPrice}" displayFreeForZero="true" /></p>
                                        </p>    
                                    </div>
                                    <div class="col-12 ">
                                    <c:forEach items="${cartEntry.messages}" var="message" >
                                    <c:if test="${not empty message.messageCode and  message.messageCode ne ''}">
                                        <div class="notification notification-warning">${message.messageCode}</div>
                                    </c:if>    
                                        </c:forEach>
                                    </div>
                                </div>
                                 </c:forEach>
 
                            </div>    
                            <b>Pay with</b>
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
												
																	 <c:if test="${not empty braintreePaymentInfos and braintreePaymentInfos.size() > 0}">
																	<!-- <b class="mt-4">Saved Credit Cards</b> -->
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
																	<ul class="dropdown-menu savedPaymentList" aria-labelledby="savedCards" id="saved-payment-action-payBill">
																		<c:forEach items="${braintreePaymentInfos}" var="paymentInfo" varStatus="status">
																		<c:if test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'CreditCard')}">
																			<li>
																				<button class="dropdown-item" data-id="${paymentInfo.id}" data-nonce="${paymentInfo.paymentMethodNonce}">
																					<img src="${paymentInfo.accountHolderName}" style="max-width: 33px; height: auto;">
																					${paymentInfo.cardType} &nbsp ${fn:escapeXml(paymentInfo.cardNumber)} &nbsp exp ${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)}
																				</button>
																			</li>
																		</c:if>
																		</c:forEach>
																	</ul>
																	
																</div>
																
																</c:if>
																	</br>
                                                 <a href="#" data-bs-toggle="modal"  data-order="${orderData.code}:payBill" data-bs-target="#addCrediCard" class="gray80"><spring:theme code="text.myaccount.extend.order.new.card"/></a>
												
											</div>
											
										</div>
										</div>
									</div>
								
								<!-- Paypal section -->
								<div class="accordion-item payProduct">
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
														<input type="radio" class="paypalselection js-enable-extend-button" id="paymentMethodPayPal" name="paymentMethodSelection" value="bt">
														<label for="paymentMethodPayPal"></label>
													</button>
												</c:otherwise>
											</c:choose>
										</div>
										<div class="col-11">
											<b>PayPal <img src="https://www.paypalobjects.com/webstatic/en_US/i/buttons/pp-acceptance-medium.png" style="height: 44px; width: auto;"></b>
											<div class="collapse" id="paypal-expand" data-bs-parent="#paymentOptions">
												<br/>
							<div id="mark-paypal-button"
								class="paypal_button_container btn btn-block"></div>
							<div id="text" class="paypalurl">
								<a style="padding-left: 10px;"
									href="https://www.paypal.com/webapps/mpp/paypal-popup"
									title="<spring:theme code="text.order.extend.po"/>"
									onclick="javascript:window.open('https://www.paypal.com/webapps/mpp/paypal-popup','WIPaypal','toolbar=no, location=no, directories=no, status=no, menubar=no, scrollbars=yes, resizable=yes, width=1060, height=700'); return false;">
									<spring:theme code="text.order.extend..what.is.paypal" />
								</a>
							</div>
							<div id="payPalErrorMessage"></div>
											</div>
										</div>
									</div>
								</div>
                            <c:if test="${orderData.isPOEnabled}">
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
                					    <input type="radio" class="paypalselection js-enable-extend-button" id="paymentMethodPo" name="paymentMethodSelection" value="bt"><label
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
                					<input type="text" class="form-control po-number mt-3" name="extendPoNumberInput" id="extendPoNumberInput" min="1" max="30" maxlength="30" value="${selectedPoNumber}"
                						placeholder="<spring:theme code="text.payment.page.po.number.placeholder"/>">
                					<input type="text" class="form-control po-number mt-3" name="extendPoNotesInput" id="extendPoNotesInput" min="1" max="1000" maxlength="1000" value="${selectedPoNotes}"
                						placeholder="<spring:theme code="text.payment.page.po.notes.placeholder"/>">
                          <input type="hidden" id="poSelected" name="poSelected" value=""/>
                				</form:form>
                				</div>
                			</div>
                		</div>
                	</div>
                </c:if>
                   </div>      
                            <div class="cart-actions">
                            <c:url value="/my-account/payBillSuccess" var="payBillUrl"></c:url>
                        <form action="${payBillUrl}" method="post" id="payBillForm">
                        <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
                       <%-- <c:set var="total" value="" /> --%>
                       
                        <input type="hidden" id="payBillTotal" name="payBillTotal" value="${orderData.orderTotalWithTaxForPayBill.value}">
                        <input type="hidden" id="orderCode" name="orderCode" value="${orderData.code}"/>
                        <input type="hidden" id="paymentId" name="paymentId" value=""/>
                        <input type="hidden" id="paymentNonce" name="paymentNonce" value=""/>
                        <input type="hidden" id="extendPoNumber" name="extendPoNumber" value=""/>
                        <input type="hidden" id="extendPoNotes" name="extendPoNotes" value=""/>
                        
                        </form>
                        		<div id="validationMessage"></div>
                        		<div id="allFieldvalidationMessage"></div>
                              <button class="btn btn-sm btn-primary float-end js-po-extend-order" type="submit">Pay Bill</button>   
                            </div>  
                            
                        </div>
                    </div>
                </div>
                <div class="col-lg-3 d-lg-block sticky-lg-top">
                    <div id="orderSummary" class="card">
                        <h5>Bill Summary</h5>
                        <hr>
                        
                        <table id="costSummary">
                            <tbody>
                            <!--Commented below code as of now  -->
                                <tr>
                                    <td class="gray80">Bill Cost</td>
                                    <td class="text-end"><format:price priceData="${orderData.extensionBillingCost}" displayFreeForZero="false"/></td>
                                </tr>
                             
                                <tr>
                                    <td class="gray80">Taxes</td>
                                    <td class="text-end"><format:price priceData="${orderData.totalPayBillTax}" displayFreeForZero="false"/></td>
                                </tr> 
                               
                                <tr class="total">
                                    <td>Total</td>
                                    <td class="text-end"><format:price priceData="${orderData.orderTotalWithTaxForPayBill}" displayFreeForZero="false"/></td>
                                    
						                            
                                </tr>
                            </tbody>
                        </table>
                        
                        <c:url value="/my-account/payBillSuccess" var="payBillUrl"></c:url>
                        <form action="${payBillUrl}" method="post" id="payBillForm">
                        <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
                       <%-- <c:set var="total" value="" /> --%>
                       
                        <input type="hidden" id="payBillTotal" name="payBillTotal" value="${orderData.orderTotalWithTaxForPayBill.value}">
                        <input type="hidden" id="orderCode" name="orderCode" value="${orderData.code}"/>
                        <input type="hidden" id="paymentId" name="paymentId" value=""/>
                        <input type="hidden" id="paymentNonce" name="paymentNonce" value=""/>
                        <input type="hidden" id="extendPoNumber" name="extendPoNumber" value=""/>
                        <input type="hidden" id="extendPoNotes" name="extendPoNotes" value=""/>
                        
                       </form>
                       
                        <button class="btn btn-block btn-primary mt-4 js-po-extend-order" type="submit">Pay Bill</button> 
                    </div>
                 </div>
                 <div id="validationMessage"></div>
                 <div id="allFieldvalidationMessage"></div>

                
              
                
     <!-- Modals -->
    <div class="modal fade" id="addCrediCard" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-sm">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title"><spring:theme code="text.extend.order.credi.card"/></h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
          
          <div class="modal-body">
          <form id="payment-add-form"
	                 action="${request.contextPath}/my-account/add-payment-method"
	              method="GET">
          <input type="hidden" id="orderId" name="orderId" value="" />
          
           <spring:theme code="text.billpay.order.credi.card.message" />
              
                 <c:url value="/my-account/add-payment-method" var="addPaymentUrl" />
                <a href="" class="btn btn-block btn-primary mt-4 add-cc-form" data-order="${orderData.code}:payBill"><spring:theme code="text.extend.order.credi.continue"/></a>
               <br>
            <p class="text-center mb-0"><a href="#" class="lightteal" aria-label="Close" data-bs-dismiss="modal" aria-label="Close">
                 <spring:theme code="basket.save.cart.action.cancel"/> </a></p>
          
          </form>
          </div>
        </div>
      </div>
    </div>
                
                
                
 <script>
	var addPaymentMethodsPage = "addPaymentMethodsPage";
	var deliveryAddressId = "${selectedAddressCode}";
	var enableShippingAddress = "false";
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

<script type="text/javascript"
   src="https://js.braintreegateway.com/web/3.69.0/js/client.min.js"></script>
<script type="text/javascript"
   src="https://js.braintreegateway.com/web/3.69.0/js/hosted-fields.min.js"></script>
<script type="text/javascript"
   src="https://js.braintreegateway.com/web/3.69.0/js/data-collector.min.js"></script>
<script type="text/javascript" src="https://js.braintreegateway.com/web/3.69.0/js/paypal.min.js"></script>
<script type="text/javascript" src="https://js.braintreegateway.com/web/3.69.0/js/paypal-checkout.min.js"></script>

