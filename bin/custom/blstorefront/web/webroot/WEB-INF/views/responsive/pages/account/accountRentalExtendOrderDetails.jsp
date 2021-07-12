<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<spring:htmlEscape defaultHtmlEscape="true" />

<div id="accountContent" class="col-lg-5 offset-lg-1">
	<h1><spring:theme code="text.myaccount.extend.order"/></h1>
	<div class="extend-order">
		<div class="row">
			<div class="col-12 mb-3">
				<h5><spring:theme code="text.myaccount.extend.order.details.active"/></h5> </div>
		</div>
		<div class="row">
			<div class="col-5 col-md-5">
				<p class="lightteal mb-0"><b>${orderData.rentalStartDate}</b></p>
				<p class="body14">
					<spring:theme code="text.myaccount.order.rental.Starts" /> </p>
			</div>
			<div class="col-2 col-md-1 text-center"> <img class="rental-arrow" src="${themeResourcePath}/assets/icon-arrow.svg"> </div>
			<div class="col-5 col-md-5">
				<p class="lightteal mb-0"><b>${orderData.rentalEndDate}</b></p>
				<p class="body14">
					<spring:theme code="text.myaccount.order.rental.ends" /> </p>
			</div>
			<div class="col-12 mt-4">
				<div class="row">
					<div class="col-5 col-md-4">
						<p class="body14">
							<spring:theme code="text.myaccount.order" />
							<br>
							<spring:theme code="text.myaccount.extend.order.date.placed" />
							<br>
							<spring:theme code="text.myaccount.extend.order.rentalDays" />
							<br>
							<spring:theme code="text.myaccount.extend.order.cost" /> </p>
					</div>
					<div class="col-7 col-md-8">
						<p class="body14 gray60"> ${fn:escapeXml(orderData.code)}
							<br> ${orderData.orderedFormatDateForExtendRental}
							<br>${orderData.totalRentalDays} Days
							<br>
							<format:price priceData="${orderData.totalPriceWithTax}" /> </p>
					</div>
				</div>
			</div>
			<div class="rental-images row mt-3">
				<c:forEach items="${orderData.entries}" var="cartEntry">
					<div class="col-4 col-md-3 text-center">
						<product:productPrimaryImage product="${cartEntry.product}" format="thumbnail" /> </div>
				</c:forEach>
			</div>
		</div>
	</div>
	<div class="extend-order">
		<div class="row mb-5">
			<div class="col-12">
				<div class="notification notification-tip info">
					<spring:theme code="text.myaccount.extend.order.info" /> </div>
			</div>
		</div>
		<div class="row">
			<div class="col-12 mb-3">
				<h5 class="mb-4"><spring:theme code="text.myaccount.extend.order.details"/></h5><b><spring:theme code="text.myaccount.extend.order.ship.it.back"/></b>
				<form class="mt-3">
					<input type="text" id="rental-litepicker" class="form-control d-none d-md-block" placeholder="Select date">
					<input type="text" id="rental-mobile-litepicker" class="form-control d-block d-md-none" placeholder="Select date"> </form>
				<div class="col-12 mt-4 mb-5">
					<div class="row">
						<div class="col-5 col-md-4">
							<p class="body14">
								<spring:theme code="text.myaccount.extend.order.added.time" />
								<br>
								<spring:theme code="text.myaccount.extend.order.cost"/>
								<br>
								<spring:theme code="text.myaccount.order.rental.damege.waiver" /> </p>
						</div>
						<div class="col-7 col-md-8">
							<p class="body14 gray60">	<div id="js-totaldays-update"> ${orderData.addedTimeForExtendRental} Day </div><br>
							<div id="js-totalCost-update"><format:price priceData="${orderData.totalCostForExtendRental}"/></div><br>
							<div id="js-totalDamegeWaiverCost-update"><format:price priceData="${orderData.totalDamageWaiverCostForExtendRental}"/></div> </p>
						</div>
					</div>
				</div> <b><spring:theme code="text.myaccount.extend.order.pay"/></b>
				<div class="col-11">
						<ycommerce:testId code="paymentDetailsForm">
							<form:form id="braintree-payment-form" name="silentOrderPostForm" class="create_update_payment_form" modelAttribute="sopPaymentDetailsForm"
							action="${request.contextPath}/braintree/checkout/hop/response" method="POST">
								<div id="cardDetails">
									<div id="credit-card-saved" class="collapse show" data-bs-parent="#cardDetails">
										<c:choose>
											<c:when test="${empty userSelectedPaymentInfo.cardNumber and empty braintreePaymentInfos}"> <b class="mt-4">Saved Credit Cards</b>
												<div class="dropdown my-2">
													<button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="savedCards" data-bs-toggle="dropdown" aria-expanded="false"> <img src="${userSelectedPaymentInfo.accountHolderName}" style="max-width: 33px; height: auto;"> &nbsp ${fn:escapeXml(userSelectedPaymentInfo.cardNumber)} &nbsp exp ${fn:escapeXml(userSelectedPaymentInfo.expiryMonth)}/${fn:escapeXml(userSelectedPaymentInfo.expiryYear)} </button>
												</div> <a href="#" id="addNewCardForm" class="gray80" data-bs-toggle="collapse" data-bs-target="#credit-card-form-expand" aria-controls="credit-card-form-expand">+ Add a new credit card</a> </c:when>
											<c:otherwise>
												<c:if test="${empty braintreePaymentInfos}"> <b class="mt-4">Saved Credit Cards</b>
													<div class="dropdown my-2">
														<button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="savedCards" data-bs-toggle="dropdown" aria-expanded="false">
															<c:choose>
																<c:when test="${not empty userSelectedPaymentInfo}"> <img src="${userSelectedPaymentInfo.accountHolderName }" style="max-width: 33px; height: auto;"> &nbsp ${fn:escapeXml(userSelectedPaymentInfo.cardNumber)} &nbsp exp ${fn:escapeXml(userSelectedPaymentInfo.expiryMonth)}/${fn:escapeXml(userSelectedPaymentInfo.expiryYear)} </c:when>
																<c:otherwise> Select or Enter new card </c:otherwise>
															</c:choose>
														</button>
														<ul class="dropdown-menu savedPaymentList" aria-labelledby="savedCards" id="saved-payment-action">
															<c:forEach items="${braintreePaymentInfos}" var="paymentInfo" varStatus="status">
																<c:if test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'CreditCard')}">
																	<li>
																		<button class="dropdown-item" data-id="${paymentInfo.id}" data-nonce="${paymentInfo.paymentMethodNonce}"> <img src="${paymentInfo.accountHolderName }" style="max-width: 33px; height: auto;"> &nbsp ${fn:escapeXml(paymentInfo.cardNumber)} &nbsp exp ${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)} </button>
																	</li>
																</c:if>
															</c:forEach>
														</ul>
													</div> <a href="#" id="addNewCardForm" class="gray80" data-bs-toggle="collapse" data-bs-target="#credit-card-form-expand" aria-controls="credit-card-form-expand">+ Add a new credit card</a> </c:if>
											</c:otherwise>
										</c:choose>
									</div>
									<div class="collapse" id="credit-card-form-expand" data-bs-parent="#cardDetails"> <b class="mt-4">Add Your Credit Card</b>
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
															<input type="checkbox" checked class="form-control" id="savePaymentInfo" name="savePaymentInfo" />
															<label for="savePaymentInfo"> <span class="gray80"><spring:theme code="checkout.multi.sop.savePaymentInfo" /></span> </label>
												</div>
											</div>
										</div> <a href="#" class="gray80" id="showSavedCard" data-bs-toggle="collapse" data-bs-target="#credit-card-saved" aria-expanded="false" aria-controls="credit-card-saved">+ Use a saved credit card</a> </div>
								</div>
								<hr/>
							</form:form>
						</ycommerce:testId>
						<c:set var="selectedAddressId" value="${defaultBillingAddress.id }" />
						<c:if test="${not empty paymentInfoBillingAddress}">
							<c:set var="selectedAddressId" value="${paymentInfoBillingAddress.id }" /> </c:if>
						<input type="hidden" id="savedBillingAddressId" name="savedBillingAddressId" value="${selectedAddressId}" />

						<form:form name="submitSavedCardForm" method="POST" id="submitSavedCardForm" action="${reviewSavedPaymentAction}">
							<input type="hidden" id="savedCCCardId" name="savedCCCardId" value="${userSelectedPaymentInfo.id}" />
							<input type="hidden" id="savedCCCardNonce" name="savedCCCardNonce" value="${userSelectedPaymentInfo.paymentMethodNonce}" /> </form:form>
						<form:form name="selectSavedCardForm" method="POST" id="selectSavedCardForm" action="${savedPaymentInfoFormURL}">
							<input type="hidden" id="selectedPaymentMethodId" name="selectedPaymentMethodId" value="" />
							<input type="hidden" id="selectedPaymentMethodNonce" name="selectedPaymentMethodNonce" value="" /> </form:form>
						</div>
				</div>
				<hr class="mt-4">
				<div class="cart-actions">
					<a href="#" class="btn btn-sm btn-primary float-end">
						<spring:theme code="text.myaccount.order.extend.rent" /> </a>
				</div>
			</div>
		</div>
	</div>




<!--Order Extension Summary -->
<div class="col-lg-3 d-lg-block sticky-lg-top">
	<div id="orderSummary" class="card">
		<h5><spring:theme code="text.myaccount.extend.order.extension"/></h5>
		<hr>
		<table id="costSummary">
			<tbody>
				<tr>
					<td class="gray80">
						<spring:theme code="text.myaccount.extend.order.extension.cost" /> </td>
					<td class="text-end" id="js-totalExtendCost"><format:price priceData="${orderData.totalCostForExtendRental}" /></td>
				</tr>
				<tr>
					<td class="gray80">
						<spring:theme code="text.myaccount.order.rental.damege.waiver" /> <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></td>
					<td class="text-end"><format:price priceData="${orderData.totalDamageWaiverCostForExtendRental}"/></td>
				</tr>
				<tr>
					<td class="gray80">
						<spring:theme code="text.myaccount.extend.order.extension.taxes" /> </td>
					<td class="text-end"><format:price priceData="${orderData.totalTaxForExtendRental}"/></td>
				</tr>
				<tr class="total">
					<td>
						<spring:theme code="text.account.order.total"/> </td>
					<td class="text-end"><format:price priceData="${orderData.orderTotalWithTaxForExtendRental}"/></td>
				</tr>
			</tbody>
		</table>
		<div class="input-group my-3">
			<input type="text" class="form-control" value="Promo code">
			<div class="input-group-append">
				<button class="btn btn-secondary" type="button">
					<spring:theme code="text.myaccount.extend.order.extension.voucher.apply" /> </button>
			</div>
		</div>
		<button class="btn btn-block btn-primary mt-4">
			<spring:theme code="text.myaccount.order.extend.rent" /> </button>
	</div>
</div>

<!-- DamageWaivers Modal -->
<div class="modal fade" id="damageWaivers" tabindex="-1" aria-hidden="true">
	<div class="modal-dialog modal-dialog-centered modal-lg">
		<div class="modal-content">
			<div class="modal-header">
				<h5 class="modal-title"><spring:theme code="text.damage.Waiver.model.title"/></h5>
				<button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
			</div>
			<div class="modal-body">
				<div class="row mb-4">
					<div class="text-center col-md-3 col-lg-2"> <img src="${themeResourcePath}/assets/gear-guard-plus.png"> </div>
					<div class="col-md-9 col-lg-10">
						<p><b><spring:theme code="text.damage.Waiver.model.option.pro"/></b></p>
						<p class="body14">
							<spring:theme code="text.damage.Waiver.model.option.pro.description" /> </p>
						<hr> </div>
				</div>
				<div class="row mb-4">
					<div class="text-center col-md-3 col-lg-2"> <img src="${themeResourcePath}/assets/gear-guard.png"> </div>
					<div class="col-md-9 col-lg-10">
						<p><b><spring:theme code="text.damage.Waiver.model.option.gear"/></b></p>
						<p class="body14">
							<spring:theme code="text.damage.Waiver.model.option.gear.description" /> </p>
						<hr> </div>
				</div>
				<div class="row">
					<div class="text-center col-md-3 col-lg-2"> <img src="${themeResourcePath}/assets/gear-guard-none.png"> </div>
					<div class="col-md-9 col-lg-10">
						<p><b><spring:theme code="text.damage.Waiver.model.option"/></b></p>
						<p class="body14">
							<spring:theme code="text.damage.Waiver.model.option.description" /> </p>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>