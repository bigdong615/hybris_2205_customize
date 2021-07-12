<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<spring:htmlEscape defaultHtmlEscape="true" />

<!--Order Extension Summary -->
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
					<td class="text-end">$4.00</td>
				</tr>
				<tr>
					<td class="gray80">
						<spring:theme code="text.myaccount.extend.order.extension.taxes" /> </td>
					<td class="text-end">$8.00</td>
				</tr>
				<tr class="total">
					<td>
						<spring:theme code="text.account.order.total" /> </td>
					<td class="text-end">$68.00</td>
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