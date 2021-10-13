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
<input type="hidden" id="js-extendOrderError" value="${extendOrderError}">
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
            	<c:url value="/my-account/voucher/remove" var="voucherRemoveUrl" />
            	<c:forEach items="${orderData.appliedVouchers}" var="voucher"
            		varStatus="loop">
            		<form:form action="${voucherRemoveUrl}" modelAttribute="voucherForm"
            			method="POST" id="removeVoucherForm${loop.index}">
            			<p class="body14">
            				<c:if test="${orderData.extendOrderDiscount.value > 0 || orderData.productDiscounts.value > 0}">
            					<span class="gray60">${fn:escapeXml(voucher)}</span>

            					<form:input hidden="hidden" value="${fn:escapeXml(voucher)}"
            						path="voucherCode" name="voucherCode" />
            					<a href="${voucherRemoveUrl}" class="js-cart-release-voucher-remove-btn"
            						id="removeVoucherForm${loop.index}"><small><spring:theme code="text.remove"/></small></a>
            					<c:forEach items="${orderData.promotionAmountMap}" var="amountMap">
            						<c:if test="${amountMap.key eq voucher}">
            							<span class="float-end">-${amountMap.value}</span>
            						</c:if>
            					</c:forEach>
            				</c:if>
            			</p>
            		</form:form>
            	</c:forEach>

                       <c:url value="/my-account/extendOrder/${fn:escapeXml(orderData.code)}" var="payExtendOrderUrl"/>
                                       <form action="${payExtendOrderUrl}" method="post" id="payExtendOrderForm">
                                            <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
                                            <input type="hidden" id="extPaymentId" name="paymentId" value=""/>
                                            <input type="hidden" id="extPaymentNonce" name="paymentNonce" value=""/>
                                            <input type="hidden" id="extendOrderPoNumber" name="extendPoNumber" value=""/>
                                            <input type="hidden" id="extendOrderPoNotes" name="extendPoNotes" value=""/>
                                            <button class="btn btn-block btn-primary mt-4 js-enable-extend-order js-po-extend-order" type="submit" disabled>
                                            <spring:theme code="text.myaccount.order.extend.rent"/></button>
                                       </form>




<script>
$(".js-voucher-apply-account-btn").on("click", function(e) {
   			e.preventDefault();
   			var voucherCode = $.trim($(".js-voucher-code-text-account").val());
   			if (voucherCode != '' && voucherCode.length > 0) {
         var formValues = $('#applyVoucherForm').serialize();
   				$.ajax({
   			url: ACC.config.encodedContextPath + '/my-account/voucher/apply',
             type: "POST",
             data: formValues,
             success: function (data) {
              $('#orderSummary').html(data);
              if($('#js-extendOrderError').val() != ""){
               $('#js-extendOrderError-update').removeClass("d-none");
               $('#js-extendOrderError-update').html($('#js-extendOrderError').val());
               $(".js-voucher-code-text-account").addClass("error");
               	 $('#errorMessages_account_voucher').addClass("d-none");

               }
               else {
               	$(".js-extendOrderError-update").addClass("error");
               	 $('#js-extendOrderError-update').addClass("d-none");
               	 $('#errorMessages_account_voucher').addClass("d-none");
               	if(	$(".js-voucher-code-text-account").hasClass("error")) {
               	$(".js-voucher-code-text-account").removeClass("error");
               	}
               }
             },
             error: function (xhr, textStatus, error) {

             }
   				});
   			} else {
   				$("#errorMessages_account_voucher").removeClass("d-none");
   				$("#errorMessages_account_voucher").html("Please enter your coupon code and click apply.");
   				$(".js-voucher-code-text-account").addClass("error");
   				$(".js-extendOrderError-update").addClass("error");
          $('#js-extendOrderError-update').addClass("d-none");
   			}

   	});

$(".js-cart-release-voucher-remove-btn").on("click", function(e) {
   			e.preventDefault();
   			 var entryNumber = $(this).attr('id');
         var form = $('#'+ entryNumber);
         var formValues = $(form).serialize();
   				$.ajax({
   			url: ACC.config.encodedContextPath + '/my-account/voucher/remove',
             type: "POST",
             data: formValues,
             success: function (data) {
              $('#orderSummary').html(data);
               $('#errorMessages_account_voucher').addClass("d-none");
             },
             error: function (xhr, textStatus, error) {

             }
   				});
   	});


   	$('#saved-payment-action-ExtendBill').on('change',function(e){
    					 var optionSelected = $("option:selected", this);
    					 var extendPaymentId = optionSelected.data("id");
    						var extendPaymentnonce = optionSelected.data("nonce");
    						$("#paymentId").val(extendPaymentId);
    						$("#paymentNonce").val(extendPaymentnonce);
    						$("#extPaymentId").val(extendPaymentId);
                $("#extPaymentNonce").val(extendPaymentnonce);
    				 });


    	$('.js-po-extend-order').on('click',function(e){
                    e.preventDefault();
        						var extendPoNumber1 = $("#extendPoNumberInput").val();
                    var extendPoNotes1 = $("#extendPoNotesInput").val();
                    if(extendPoNumber1 !== '' && extendPoNotes1 !== '') {

        						$("#extendPoNumber").val(extendPoNumber1);
        						$("#extendPoNotes").val(extendPoNotes1);

        							$("#extendOrderPoNumber").val(extendPoNumber1);
                      $("#extendOrderPoNotes").val(extendPoNotes1);
        						}
        						$("#payExtendOrderForm").submit();
        				 });

</script>