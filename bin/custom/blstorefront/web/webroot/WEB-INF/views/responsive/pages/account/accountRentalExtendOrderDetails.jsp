<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:url value="/" var="homePageUrl" />
 <input type="hidden" id="js-extend-order-page" name="jsExtendOrderPage" value="true"/>
 <input type="hidden" id="js-extend-order-code" name="extendOrderCode" value="${fn:escapeXml(orderData.code)}"/>
<div id="accountContent" class="col-lg-5 offset-lg-1">
	<h3><spring:theme code="text.myaccount.extend.order"/></h3>
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

			</div>
			<div class="rental-images row mt-3">
				<c:forEach items="${orderData.entries}" var="cartEntry">
				  <c:url var="productUrl" value="/rent/product/${cartEntry.product.code}"/>
					<div class="col-4 col-md-3 text-center">
						<a href="${productUrl}" style="text-decoration: none"><product:productPrimaryImage product="${cartEntry.product}" format="product" /></a> </div>
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
						<div class="extend-price">	
							<p class="body14 gray60" id="js-totaldays-update"> ${orderData.addedTimeForExtendRental} Days  </p>
							<p class="body14 gray60" id="js-totalCost-update"> <format:blPrice priceData="${orderData.subTotalTaxForExtendRental}"/> </p>
							<p class="body14 gray60" id="js-totalDamegeWaiverCost-update"> <format:blPrice priceData="${orderData.totalDamageWaiverCostForExtendRental}"/> </p>
						</div>
						</div>
					</div>
             <div class="productNotifications row d-none" id="add-error-message">
                    <div class="col-12">
                       <div class="notification notification-error">
                        <spring:theme code="text.order.extend.stock.validation"/>
                         <a href="${homePageUrl}" class="gray80"><spring:theme code="text.order.extend.stock.contact.us"/></a>
                          <spring:theme code="text.order.extend.stock.contact.warning"/>
                       </div>
                    </div>
             </div>

						<div > </div>
				</div> <b><spring:theme code="text.myaccount.extend.order.pay"/></b>
                <order:accountExtendOrderPayment order="${orderData}"/>
				</div>
				<hr class="mt-4">

	<div class="notification notification-error d-none"id="js-extendOrderPaymentError-update">
	<spring:theme code="text.order.extend.payment.error"/>
	</div>

     <div class="cart-actions">
      <c:url value="/my-account/extendOrder/${fn:escapeXml(orderData.code)}" var="payExtendOrderUrl"/>
                                            <form action="${payExtendOrderUrl}" method="post" id="payExtendOrderForm">
                                                 <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
                                                 <input type="hidden" id="paymentId" name="paymentId" value=""/>
                                                 <input type="hidden" id="paymentNonce" name="paymentNonce" value=""/>
                                                 <input type="hidden" id="extendPoNumber" name="extendPoNumber" value=""/>
                                                 <input type="hidden" id="extendPoNotes" name="extendPoNotes" value=""/>
                                                 </br>
                                                 <button class="btn btn-sm btn-primary float-end js-enable-extend-order js-po-extend-order js-extend-button-enable" type="submit" disabled>
                                                 <spring:theme code="text.myaccount.order.extend.rent"/></button>
                                            </form>

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
					<td class="text-end" id="js-totalExtendCost"><format:blPrice priceData="${orderData.subTotalTaxForExtendRental}" /></td>
				</tr>
				<tr>
					<td class="gray80">
						<spring:theme code="text.myaccount.order.rental.damege.waiver" /> <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></td>
					<td class="text-end" id="js-totalDamageWaiver"><format:blPrice priceData="${orderData.totalDamageWaiverCostForExtendRental}"/></td>
				</tr>
				<tr>
					<td class="gray80">
						<spring:theme code="text.myaccount.extend.order.extension.taxes" /> </td>
					<td class="text-end"  id="js-totalExtendTax"><format:blPrice priceData="${orderData.totalTaxForExtendRental}"/></td>
				</tr>
				<c:if test="${orderData.extendOrderDiscount.value > 0}">
        				<tr>
                	<td class="discount">
                						<spring:theme code="Discount" /> </td>
                					<td class="text-end" id="js-extendDiscount"> - <format:blPrice priceData="${orderData.extendOrderDiscount}"/></td>
                	</tr>
        </c:if>
				<tr class="total">
					<td>
						<spring:theme code="text.account.order.total"/> </td>
					<td class="text-end" id="js-extendOrderTotal"><format:blPrice priceData="${orderData.orderTotalWithTaxForExtendRental}"/></td>
				</tr>
			</tbody>
		</table>
		<c:url value="/my-account/voucher/apply" var="voucherUrl" />
		<form:form action="${voucherUrl}" modelAttribute="voucherForm" method="POST" id="applyVoucherForm">
    			<spring:theme
    				code="text.checkout.multi.order.summary.promocode.placeholder"
    				var="voucherplaceholder" />
    			<div class="input-group my-3">
    				<form:input type="text"
    					class="form-control ${errormsgvalid} js-voucher-code-text-account"
    					path="voucherCode" placeholder="${voucherplaceholder}"
    					name="voucherCode" />
    				<div class="input-group-append">

                                  	<button type="submit" class="btn btn-secondary js-voucher-apply-account-btn" disabled="disabled">
                                       <spring:theme code="text.voucher.apply.button.label" />
                                    </button>

    				</div>
    			</div>
    		</form:form>
            <button class="btn btn-block btn-primary mt-4" disabled>Extend Rental</button>
	</div>


	<div class="notification notification-error d-none"id="js-extendOrderError-update"></div>

 <div class="col-12">
<div class="notification notification-error d-none"id="errorMessages_account_voucher"></div>
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