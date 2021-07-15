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

<!--Order Extension Summary -->
<h5><spring:theme code="text.myaccount.extend.order.extension"/></h5>
<input type="hidden" id="js-totalExtendDays" value="${orderData.addedTimeForExtendRental}">
<input type="hidden" id="js-isAllProductExtendabe" value="${orderData.extendErrorMessage}">
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
				<c:if test="${orderData.totalDiscounts.value > 0}">
				<tr>
        	<td class="discount">
        						<spring:theme code="Discount" /> </td>
        					<td class="text-end" id="js-extendDiscount"> - <format:blPrice priceData="${orderData.totalDiscounts}"/></td>
        	</tr>
        </c:if>
				<tr class="total">
					<td>
						<spring:theme code="text.account.order.total" /> </td>
					<td class="text-end"  id="js-extendOrderTotal"><format:blPrice priceData="${orderData.orderTotalWithTaxForExtendRental}"/></td>
				</tr>
			</tbody>
		</table>
		<c:if test="${not empty fn:escapeXml(errorMsg)}">
    		<c:set var="errormsgvalid" value="error" />
    	</c:if>

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
                    	<button type="submit" class="btn btn-secondary js-voucher-apply-account-btn">
                         <spring:theme code="text.voucher.apply.button.label" />
                      </button>

        				</div>
        			</div>
        		</form:form>

        		<small class="gray60"><spring:theme
            			code="text.checkout.multi.order.summary.msg" /></small>
            	<c:url value="/cart/voucher/remove" var="voucherRemoveUrl" />
            	<c:forEach items="${cartData.appliedVouchers}" var="voucher"
            		varStatus="loop">
            		<form:form action="${voucherRemoveUrl}" modelAttribute="voucherForm"
            			method="POST" id="removeVoucherForm${loop.index}">
            			<p class="body14">
            				<c:if test="${cartData.totalDiscounts.value > 0 || cartData.productDiscounts.value > 0}">
            					<span class="gray60">${fn:escapeXml(voucher)}</span>

            					<form:input hidden="hidden" value="${fn:escapeXml(voucher)}"
            						path="voucherCode" name="voucherCode" />
            					<a href="#" class="js-cart-release-voucher-remove-btn"
            						id="removeVoucherForm${loop.index}"><small><spring:theme code="text.remove"/></small></a>
            					<c:forEach items="${cartData.promotionAmountMap}" var="amountMap">
            						<c:if test="${amountMap.key eq voucher}">
            							<span class="float-end">-${amountMap.value}</span>
            						</c:if>
            					</c:forEach>
            				</c:if>
            			</p>
            		</form:form>
            	</c:forEach>

		<button class="btn btn-block btn-primary mt-4">
			<spring:theme code="text.myaccount.order.extend.rent" /> </button>

			<div class="notification notification-error d-none"id="errorMessages_voucher"></div>