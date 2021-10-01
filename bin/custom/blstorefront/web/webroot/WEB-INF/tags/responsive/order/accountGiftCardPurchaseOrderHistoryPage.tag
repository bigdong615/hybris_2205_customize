<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order"%>
<%@ taglib prefix="account"
	tagdir="/WEB-INF/tags/addons/blassistedservicestorefront/order"%>
<%@ attribute name="OrderData"
	type="de.hybris.platform.commercefacades.order.data.OrderHistoryData"%>
<spring:htmlEscape defaultHtmlEscape="true" />

<div class="col-xl-10">
	<div class="row">
		<div id="accountContent" class="col-lg-7">
			<h1>
				<spring:theme code="text.account.order.title.details" />
			</h1>
			<hr>

			<div class="reviewCart">
				<div class="row">
					<div class="col-6">
						<p class="body14">
							<spring:theme code="text.account.orderHistory.orderStatus" />
							<br>
							<spring:theme code="text.account.orderHistory.datePlaced" />
							<br>
							<spring:theme code="text.myaccount.order" />
							<br>
							<spring:theme code="text.myaccount.order.tracking" />
						</p>
					</div>
					<c:choose>
					<c:when test="${orderData.status eq 'RECEIVED_IN_VERIFICATION'}">
					<spring:theme code="type.orderstatus.${orderData.status}.name" var="orderStatus"/>
					</c:when>
					<c:otherwise>
					<c:set var="orderStatus" value="${orderData.status}"/>"
					</c:otherwise>
					</c:choose>
					<div class="col-6">
						<p class="gray80 body14">
							${orderStatus}<br> ${orderData.orderedFormatDate}<br>
							${fn:escapeXml(orderData.code)}<br><spring:theme code="order.gift.card.myaccount.review.page.sent.email" />
						</p>
					</div>
				</div>

			</div>
			<div class="reviewCart">
				<h5 class="mb-4">Your Order</h5>
				<c:forEach items="${orderData.entries}" var="cartEntry">
					<div class="row">
						<c:url var="productUrl"
							value="/rent/product/${cartEntry.product.code}" />
						<c:if test="${!orderData.isRentalCart}">
							<c:url var="productUrl"
								value="/buy/product/${cartEntry.product.code}" />
						</c:if>


						<div class="col8">

							<a href="${productUrl}" style="text-decoration: none"> <format:price
									priceData="${cartEntry.totalPrice}" displayFreeForZero="true" />
								<spring:theme code="order.gift.card.myaccount.review.page.gift.certificate" />
							</a> <br /> 
							<spring:theme code="order.gift.card.myaccount.review.page.To" /> :
						   ${cartEntry.recipientName} (${cartEntry.recipientEmail})

						</div>
					</div>
				</c:forEach>
			</div>
			<div class="reviewCart">
				<h5 class="mb-4">
					<spring:theme code="text.review.page.delivery.mode" />
				</h5>
				<div class="row">
					<div class="col8"><spring:theme code="order.gift.card.myaccount.review.page.delivered.email" /></div>
				</div>
			</div>

			<div class="reviewCart">
				<c:choose>
					<c:when test="${not empty orderData.paymentInfo}">
						<h5 class="mb-4">
							<spring:theme code="text.myaccount.order.payment.title" />
						</h5>
						<div class="row mb-4">
							<order:accountPaymentDetails orderData="${orderData}"
								paymentInfo="${orderData.paymentInfo}" />
						</div>
					</c:when>
					<c:otherwise>
						<div class="row">
							<div class="col-2 text-center">
								<img
									src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-po.png"
									style="width: 50px;">
							</div>
							<div class="col-10 col-md-5">
								<b class="body14 gray100"><spring:theme
										code="text.review.page.payment.po" /></b>
								<div class="row">
									<div class="col-6">
										<p class="body14">
											<spring:theme code="text.review.page.payment.amount" />
										</p>
									</div>
									<div class="col-6">
										<p class="body14 gray80">
											<format:price priceData="${orderData.totalPriceWithTax}" />
										</p>
									</div>
								</div>
							</div>
							<div class="col-12 col-md-5">
								<div class="po-order-notes">
									<p class="gray80 body14">
										<b class="gray100"><spring:theme
												code="text.order.confirmation.print.page.po.notes" /></b>
										<c:choose>
											<c:when test="${orderData.poNotes == ''}">
												<spring:theme code="text.review.page.payment.notes.na" />
											</c:when>
											<c:otherwise>
                                               ${orderData.poNotes}
                                   				  </c:otherwise>
										</c:choose>
									</p>
								</div>
							</div>
						</div>
					</c:otherwise>
				</c:choose>
			</div>


		</div>
		<div
			class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top mt-5 mt-md-0">
			<div id="orderSummary" class="card">
				<h5>
					<spring:theme code="text.myaccount.order.summary" />
				</h5>
				<hr>
				<table id="costSummary">
					<tbody>
						<tr>
							<td class="gray80"><spring:theme code="order.gift.card.myaccount.review.page.gift.certificate.cost" /></td>
							<td class="text-end"><format:price
									priceData="${orderData.subTotal}" /></td>
						</tr>

						
						<c:if test="${orderData.totalDiscounts.value > 0}">
							<tr class="discount">
								<td><spring:theme code="text.myaccount.order.discount" /></td>
								<td class="text-end">-<format:blPrice
										priceData="${orderData.totalDiscounts}" /></td>
							</tr>
						</c:if>
						<tr class="total">
							<td><spring:theme code="text.myaccount.order.total" /></td>
							<td class="text-end"><format:price
									priceData="${orderData.totalPriceWithTax}" /></td>
						</tr>
					</tbody>
				</table>

			</div>
		</div>
	</div>
</div>

