<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.CartData" %>
<%@ attribute name="paymentInfo" required="true" type="de.hybris.platform.commercefacades.order.data.CCPaymentInfoData" %>
<%@ attribute name="showPaymentInfo" required="false" type="java.lang.Boolean" %>
<%@ attribute name="brainTreePaymentInfo" required="false"
	type="com.braintree.hybris.data.BrainTreePaymentInfoData"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<c:choose>
  <c:when test="${not empty paymentInfo}">
    <c:set var="cardName" value="${fn:escapeXml(paymentInfo.cardType)}"/>
    <c:if test="${empty cardName }">
	    <c:choose>
		    <c:when test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'BrainTreePayPalExpress')}">
			    <c:set var="cardName" value="PayPal"/>
		    </c:when>
	    </c:choose>
    </c:if>
	 <div class="row">
		<div class="col-2 text-center">
			<img src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-${fn:replace(fn:toLowerCase(cardName),' ', '_')}.png" style="width: 50px;">
		</div>
		<div class="col-10 col-md-5">
			<b class="body14 gray100">${cardName}</b>
			<div class="row">
				<div class="col-6">
					<p class="body14">
						<c:if test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'BrainTreePayPalExpress') == false }">
						<spring:theme code="text.review.page.payment.ending" /><br> 
						<spring:theme code="text.review.page.payment.exp" /><br> 
						</c:if>
						<spring:theme code="text.review.page.payment.amount" />
					</p>
				</div>
				<div class="col-6">
					<p class="body14 gray80">
						<c:if test="${fn:containsIgnoreCase(paymentInfo.subscriptionId, 'BrainTreePayPalExpress') == false }">
						${fn:escapeXml(paymentInfo.cardNumber)}<br> 
						${fn:escapeXml(paymentInfo.expiryMonth)}/${fn:escapeXml(paymentInfo.expiryYear)}<br> 
						</c:if>
						<format:price priceData="${cartData.totalPriceWithTax}"/>
					</p>
				</div>
			</div>
		</div>
		<!-- Uncomment below section when order note is implemented on payments page -->
		<!-- <div class="col-12 col-md-5">
			<p class="gray80 body14">
				<b class="gray100">Order Notes</b> JayZ Superbowl Shoot
			</p>
		</div> -->
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
    		<b class="body14 gray100"><spring:theme code="text.review.page.payment.po" /></b>
    		<div class="row">
    			<div class="col-6">
    				<p class="body14">
    					<spring:theme code="text.review.page.payment.amount" />
    				</p>
    			</div>
    			<div class="col-6">
    				<p class="body14 gray80">
    					<format:price priceData="${cartData.totalPriceWithTax}" />
    				</p>
    			</div>
    		</div>
    	</div>
    	<div class="col-12 col-md-5">
    	  <div class="po-order-notes">
    		  <p class="gray80 body14">
    			  <b class="gray100"><spring:theme code="text.order.confirmation.print.page.po.notes"/></b>
    			  <c:choose>
    				  <c:when test="${cartData.poNotes == ''}">
                 <spring:theme code="text.review.page.payment.notes.na"/>
    				  </c:when>
    				  <c:otherwise>
                ${cartData.poNotes}
    				  </c:otherwise>
    			  </c:choose>
    		  </p>
    	  </div>
    	</div>
    </div>
  </c:otherwise>
</c:choose>

