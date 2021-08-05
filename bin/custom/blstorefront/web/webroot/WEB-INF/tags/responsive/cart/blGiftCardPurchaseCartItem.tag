<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="cartData" required="true"
	type="de.hybris.platform.commercefacades.order.data.CartData"%>
<%@ attribute name="entry" required="true"
	type="de.hybris.platform.commercefacades.order.data.OrderEntryData"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>

<%-- ToDo - Need to update html, as of now used rental cart html here for used cart --%>
<spring:htmlEscape defaultHtmlEscape="true" />
<c:url value="/rent/product/${entry.product.code}" var="productUrl" />
<c:url value="/cart/update" var="cartUpdateFormAction" />

<div class="cartProduct">
	<div class="row">
		<div class="col-md-2 text-center mt-3">
		<a href="#"><product:productPrimaryImage
					product="${entry.product}" format="thumbnail" /></a>
		</div>
		<div class="col-md-6 mt-3">
			<b>${entry.product.name}</b>
			<form:form id="removeCartForm${entry.entryNumber}"
				action="${cartUpdateFormAction}" method="post"
				modelAttribute="updateQuantityForm${entry.entryNumber}"
				class="js-qty-form${entry.entryNumber}">
				<input type="hidden" name="entryNumber" value="${entry.entryNumber}" />
				<input type="hidden" name="productCode"
					value="${entry.product.code}" />
				<input type="hidden" name="initialQuantity"
					value="${entry.quantity}" />
				<input type="hidden" name="quantity" value=0 />
				<input type="hidden" name="removeEntry" value="true" />
				<a href="" class="shopping-cart__item-remove"
					id="removeEntry_${entry.entryNumber}"><small>Remove
						Item</small></a>
			</form:form>
		</div>
		<div class="col-md-4 mt-3">
			<p class="text-md-end">
				<spring:theme code="text.gift.cart.purchase" />
				<b><format:price priceData="${entry.totalPrice}"
						displayFreeForZero="true" /></span></b>
			</p>
		</div>
	</div>
	<div class="row mt-3">
		<div class="col-md-10 offset-md-2">
			<form:form method="POST" modelAttribute="giftCardPurchaseForm"
				id="giftCardPurchaseForm">
				<div class="gc-pdp-form">
					<input type="text" class="form-control"
						id="first-name"
						placeholder="<spring:theme code='giftcard.PurchaseForm.name.placeholder' />"
						name="name" value="${entry.recipientName}"> <input type="text" class="form-control"
						id="email" value="${entry.recipientEmail}"
						placeholder="<spring:theme code='giftcard.PurchaseForm.email.placeholder' />"
						name="email">
					<textarea class="form-control mt-2 mb-4" 
						placeholder="<spring:theme code='giftcard.PurchaseForm.message.placeholder' />"
						name="message">${entry.recipientMessage}</textarea>
				</div>
			</form:form>
		</div>
	</div>
</div>