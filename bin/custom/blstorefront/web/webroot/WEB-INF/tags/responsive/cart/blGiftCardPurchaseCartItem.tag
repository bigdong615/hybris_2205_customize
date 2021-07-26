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
			<%-- <input type="text" class="form-control" id="amount"
				placeholder="<spring:theme code='giftcard.PurchaseForm.amount.placeholder' />"
				name="amount">  --%>
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
					<textarea class="form-control mt-2 mb-4" value="${entry.recipientMessage}"
						placeholder="<spring:theme code='giftcard.PurchaseForm.message.placeholder' />"
						name="message"></textarea>
				</div>
			</form:form>
			<%-- <input type="text" class="form-control"
				id="first-name"
				placeholder="<spring:theme code='giftcard.PurchaseForm.name.placeholder' />"
				name="name"> <input type="text" class="form-control"
				id="email"
				placeholder="<spring:theme code='giftcard.PurchaseForm.email.placeholder' />"
				name="email">
			<textarea class="form-control mt-2 mb-4"
				placeholder="<spring:theme code='giftcard.PurchaseForm.message.placeholder' />"
				name="message"></textarea> --%>
		</div>
	</div>
	<div class="productNotifications row hide">
		<div class="col-12">
			<div class="notification notification-warning">
				<spring:theme code="text.giftcard.PurchaseForm.product.warning" />
			</div>
		</div>
	</div>
	<%-- It will be handled in BL-463 --%>
	<%-- <div id="productOptions" class="row mt-3">
         <div class="col-md-10 offset-md-2">
             <p class="body14 mb-1"><spring:theme code="text.cart.options"/></p>
             <div class="dropdown">
               <a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="coverageOptions1" data-bs-toggle="dropdown" aria-expanded="false">
                 <img src="https://d2ieyhi8galmxj.cloudfront.net/product/MDe5966894-1fa0-4dbb-89be-30c74da8f3a5.jpg">EF/EF-S Lens to EOS R Camera Adapter <span class="float-end">$XX</span>
               </a>
               <ul class="dropdown-menu" aria-labelledby="coverageOptions1">
                 <li><a class="dropdown-item" href="#"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MDe5966894-1fa0-4dbb-89be-30c74da8f3a5.jpg">EF/EF-S Lens to EOS R Camera Adapter <span class="float-end">$XX</span></a></li>
                 <li><a class="dropdown-item" href="#"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MDe5966894-1fa0-4dbb-89be-30c74da8f3a5.jpg">EF/EF-S Lens to EOS R Camera Adapter <span class="float-end">$XX</span></a></li>
                 <li><a class="dropdown-item" href="#"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MDe5966894-1fa0-4dbb-89be-30c74da8f3a5.jpg">EF/EF-S Lens to EOS R Camera Adapter <span class="float-end">$XX</span></a></li>
               </ul>
             </div>
         </div>
     </div> --%>

	<%--<div class="productNotifications row">
         <div class="col-12">
             <div class="notification notification-warning">This is a product warning.</div>
             <div class="notification notification-error">This item is no longer available for your selected date range. Change your dates or select a comparable item.</div>
         </div>
     </div>--%>
</div>