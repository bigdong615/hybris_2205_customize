<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.CartData" %>
<%@ attribute name="showTax" required="false" type="java.lang.Boolean" %>
<%@ attribute name="showTaxEstimate" required="false" type="java.lang.Boolean" %>
<%@ attribute name="subtotalsCssClasses" required="false" type="java.lang.String" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix = "fmt" uri = "http://java.sun.com/jsp/jstl/fmt" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<div class="subtotals ${fn:escapeXml(subtotalsCssClasses)}">
	<div class="subtotal">
		<spring:theme code="basket.page.totals.subtotal"/>
		<span>
			<ycommerce:testId code="Order_Totals_Subtotal">
				<format:price priceData="${cartData.subTotal}"/>
			</ycommerce:testId>
		</span>
	</div>
	<c:if test="${cartData.totalDiscounts.value > 0}">
		<div class="subtotals__item--state-discount">
			<spring:theme code="text.account.order.discount"/>
			<span>
				<ycommerce:testId code="Order_Totals_Savings">
					<format:price priceData="${cartData.totalDiscounts}" displayNegationForDiscount="true" />
				</ycommerce:testId>
			</span>
		</div>
	</c:if>
	<c:if test="${cartData.quoteDiscounts.value > 0}">
		<div class="subtotals__item--state-discount">
			<spring:theme code="basket.page.quote.discounts" />
			<span>
				<ycommerce:testId code="Quote_Totals_Savings">
					<format:price priceData="${cartData.quoteDiscounts}" displayNegationForDiscount="true" />
				</ycommerce:testId>
			</span>
		</div>
	</c:if>
	<c:if test="${not empty cartData.deliveryCost}">
		<div class="shipping">
			<spring:theme code="basket.page.totals.delivery"/>
			<span>
				<ycommerce:testId code="Order_Totals_Delivery">
					<format:price priceData="${cartData.deliveryCost}" displayFreeForZero="TRUE"/>
				</ycommerce:testId>
			</span>
		</div>
	</c:if>
	<c:if test="${cartData.net && cartData.totalTax.value > 0 && showTax}">
		<div class="tax">
			<spring:theme code="basket.page.totals.netTax"/>
			<span>
				<format:price priceData="${cartData.totalTax}"/>
			</span>
		</div>
	</c:if>



	<%-- <c:if test="${cartData1.grandTotal.value > 0}">
    		<div class="totals">
    		<spring:theme code="basket.page.totals.total" text="Order Total"/>
    			<span>
    			<ycommerce:testId code="cart_totalPrice_label">
    		<c:set var="extractCurrencySymbol" value = "${cartData.totalPrice.formattedValue}"/>
    		<c:set var="firstCharacter" value ="${fn:substring(extractCurrencySymbol, 0, 1)}"/>

    		<c:choose>
    			<c:when test = "${progressBarId == 'deliveryAddress' && (cartData.store ne 'corsair-us') && cartData.net && cartData.totalTax.value > 0 && showTax && (cartData.totalPrice.value eq '0.0' ||(cartData.totalPrice.value - (cartData.rawSubTotal.value - cartData.orderDiscounts.value + cartData.totalDeliveryWithFreeShipping.value - cartData.discountDelivery.value  + cartData.totalTax.value - cartData.giftCardDiscount.value)) eq '0.00')}">
    				<c:choose>
    					<c:when test="${firstCharacter.matches('[0-9]+')}">
    						<fmt:formatNumber value="${otherGrandTotal + cartData.totalTax.value}" maxFractionDigits="2" minFractionDigits="2"/>${fn:substring(extractCurrencySymbol, fn:length(extractCurrencySymbol)-1, fn:length(extractCurrencySymbol))}

    					</c:when>

    					<c:otherwise>
    						${fn:substring(extractCurrencySymbol,0, 1)}<fmt:formatNumber value="${otherGrandTotal + cartData.totalTax.value}" maxFractionDigits="2" minFractionDigits="2"/>

    					</c:otherwise>
    				</c:choose>
    			</c:when>

    			<c:when test = "${progressBarId == 'deliveryAddress' && (cartData.store ne 'corsair-us') && !(cartData.net && cartData.totalTax.value > 0 && showTax) && (cartData.totalPrice.value eq '0.0' || (cartData.totalPrice.value - (cartData.rawSubTotal.value - cartData.orderDiscounts.value + cartData.totalDeliveryWithFreeShipping.value - cartData.discountDelivery.value - cartData.giftCardDiscount.value)) eq '0.00')}">
    				<c:choose>
    					<c:when test="${firstCharacter.matches('[0-9]+')}">
    							<fmt:formatNumber value="${otherGrandTotal}" maxFractionDigits="2" minFractionDigits="2"/>${fn:substring(extractCurrencySymbol, fn:length(extractCurrencySymbol)-1, fn:length(extractCurrencySymbol))}

    					</c:when>

    					<c:otherwise>
    							${fn:substring(extractCurrencySymbol,0, 1)}<fmt:formatNumber value="${otherGrandTotal}" maxFractionDigits="2" minFractionDigits="2"/>

    					</c:otherwise>
    				</c:choose>
    			</c:when>

    			<c:when test = "${progressBarId == 'deliveryAddress' && (cartData.store eq 'corsair-us') && cartData.net && cartData.totalTax.value > 0 && showTax}">
    			 	${fn:substring(extractCurrencySymbol,0, 1)} <fmt:formatNumber value="${usGrandTotal + cartData.totalTax.value}" maxFractionDigits="2" minFractionDigits="2"/>

    			</c:when>

    			<c:when test = "${progressBarId == 'deliveryAddress' && (cartData.store eq 'corsair-us') && !(cartData.net && cartData.totalTax.value > 0 && showTax)}">
    				${fn:substring(extractCurrencySymbol,0, 1)}<fmt:formatNumber value="${usGrandTotal}" maxFractionDigits="2" minFractionDigits="2"/>

    			</c:when>

    			<c:otherwise>
    				<format:price priceData="${cartData.grandTotal}"/>
    			</c:otherwise>
    			</c:choose>
    			</ycommerce:testId>
    		</span>
    		</div>
    	</c:if>  --%>
     	<c:if test="${cartData1.giftCardDiscount.value > 0}">
    		<div class="shipping">
    		<c:set var = "giftValueForStepFirst" value ="${cartData1.giftCardDiscount.value-(cartData.giftCardDiscount.value)}"/>
    		<spring:theme code="basket.page.totals.gcamount" text="Gift Card Amount :"/>
    			<span>
    			<ycommerce:testId code="Order_Totals_Delivery">
    			<%-- <c:choose>
    				 <c:when test = "${progressBarId == 'deliveryAddress' && (cartData.store ne 'corsair-us') && !(cartData.net && cartData.totalTax.value > 0 && showTax) && cartData.giftCardDiscount.value gt (cartData.rawSubTotal.value - cartData.orderDiscounts.value)}">
    			 		<c:choose>
    						<c:when test="${firstCharacter.matches('[0-9]+')}">
    							 -<fmt:formatNumber value="${cartData1.giftCardDiscount.value - (cartData1.giftCardDiscount.value -(cartData.rawSubTotal.value - cartData.orderDiscounts.value))}" maxFractionDigits="2" minFractionDigits="2"/>${fn:substring(extractCurrencySymbol, fn:length(extractCurrencySymbol)-1, fn:length(extractCurrencySymbol))}

    						</c:when>

    						<c:otherwise>
    							-${fn:substring(extractCurrencySymbol,0, 1)}<fmt:formatNumber value="${cartData1.giftCardDiscount.value - (cartData1.giftCardDiscount.value -(cartData1.rawSubTotal.value - cartData1.orderDiscounts.value))}" maxFractionDigits="2" minFractionDigits="2"/>

    						</c:otherwise>
    					</c:choose>
    			 	</c:when>

    			  	<c:when test = "${progressBarId == 'deliveryAddress' && (cartData.store ne 'corsair-us') && !(cartData.net && cartData.totalTax.value > 0 && showTax) && cartData.giftCardDiscount.value le (cartData.rawSubTotal.value - cartData.orderDiscounts.value)}">
    			 			-${cartData.giftCardDiscount.formattedValue}
    			 	</c:when>

    			 	<c:when test = "${progressBarId == 'deliveryAddress' && (cartData.store ne 'corsair-us') && cartData.net && cartData.totalTax.value > 0 && showTax && cartData.giftCardDiscount.value gt (otherGrandTotalWithTax)}">
    			 		<c:choose>
    						<c:when test="${firstCharacter.matches('[0-9]+')}">
    						 	-<fmt:formatNumber value="${(giftValueForStepFirst)}" maxFractionDigits="2" minFractionDigits="2"/>${fn:substring(extractCurrencySymbol, fn:length(extractCurrencySymbol)-1, fn:length(extractCurrencySymbol))}

    						</c:when>

    						<c:otherwise>
    						-${fn:substring(extractCurrencySymbol,0, 1)}<fmt:formatNumber value="${giftValueForStepFirst}" maxFractionDigits="2" minFractionDigits="2"/>

    						</c:otherwise>
    					</c:choose>
    			 	</c:when>

    			  	<c:when test = "${progressBarId == 'deliveryAddress' && (cartData.store ne 'corsair-us') && cartData.net && cartData.totalTax.value > 0 && showTax && cartData.giftCardDiscount.value le (otherGrandTotalWithTax)}">
    			 		-<format:price priceData="${cartData1.giftCardDiscount}"/>
    			 	</c:when>
    			<spring:theme code="basket.page.totals.delivery"/>
    			 <c:otherwise>
    					-<format:price priceData="${cartData1.giftCardDiscount}"/>
    			</c:otherwise>
    		</c:choose> --%>
    			-<format:price priceData="${cartData1.giftCardDiscount}"/>
    				</ycommerce:testId>
    			</span>
    		</div>
    </c:if>




	<div class="totals">
		<spring:theme code="basket.page.totals.total"/>
		<span>
			<ycommerce:testId code="cart_totalPrice_label">
				<c:choose>
					<c:when test="${showTax}">
						<format:price priceData="${cartData.totalPriceWithTax}"/>
					</c:when>
					<c:otherwise>
						<format:price priceData="${cartData.totalPrice}"/>
					</c:otherwise>
				</c:choose>

			</ycommerce:testId>
		</span>
	</div>
	<c:if test="${not cartData.net}">
		<div class="realTotals">
			<ycommerce:testId code="cart_taxes_label">
				<p>
					<spring:theme code="basket.page.totals.grossTax" arguments="${cartData.totalTax.formattedValue}" argumentSeparator="!!!!"/>
				</p>
			</ycommerce:testId>
		</div>
	</c:if>
	<c:if test="${cartData.net && not showTax }">
		<div class="realTotals">
			<ycommerce:testId code="cart_taxes_label">
				<p>
					<spring:theme code="basket.page.totals.noNetTax"/>
				</p>
			</ycommerce:testId>
		</div>
	</c:if>
</div>
	
