<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="sec"
	uri="http://www.springframework.org/security/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>

<c:url value="/cart/usedgearadd" var="addToCartUrl" />


<thead>
	<tr>
		<th><spring:theme code="pdp.serial.table.rating.text" /></th>
		<th class="d-none d-md-table-cell"><spring:theme
				code="pdp.serial.table.price.text" /></th>
		<th><spring:theme code="pdp.serial.table.incentivized.price.text" /></th>
		<th class="d-none d-md-table-cell"><spring:theme
				code="pdp.serial.table.number.text" /></th>
		<th></th>
	</tr>
</thead>
<tbody>
	<input type="hidden" id="doReload" name="doReload" value="false" />
	<div class="modal fade" id="addToCart" tabindex="-1" aria-hidden="true">
		<div class="modal-dialog modal-dialog-centered modal-sm"
			id="addToCartModalDialog"></div>
	</div>

	<form:form id="serialSubmitForm" action="${addToCartUrl}" method="get">
		<c:forEach items="${product.serialproducts}" var="serialProduct"
			varStatus="loop">
   <c:if test="${serialProduct.serialSoftAssignedOrHardAssigned eq 'true'}">
		<c:if test="${serialProduct.serialStatus ne 'SOLD' &&  serialProduct.isSerialNotAssignedToRentalOrder eq true }">
			<c:set value="${serialProduct.ugPromotionMessage ne null && serialProduct.serialPromotionPrice.value > 0 && product.onSale eq true && serialProduct.onSale eq true}" var="hasPromotion"/>
			<tr class=" ${loop.index >= 3 ? 'hide-product-row ' : ''} <c:if test="${hasPromotion}"> noborder</c:if>">
				<td><a href="#" data-bs-toggle="modal"
					data-bs-target="#sku52678"
					data-cosmetic="${serialProduct.cosmeticRating}"
					data-functional="${serialProduct.functionalRating}"
					data-condition-rating="${serialProduct.conditionRating}"
					data-serial-id="${serialProduct.serialId}"
					class="js-conditional-rating-popup">${serialProduct.conditionRating}</a></td>
				<c:choose>
					<c:when test="${not empty serialProduct.finalIncentivizedPrice}">
						<td class="d-none d-md-table-cell"><strike><format:price
									priceData="${serialProduct.finalSalePrice}" /></strike></td>
						<td><format:price
								priceData="${serialProduct.finalIncentivizedPrice}" /></td>
					</c:when>
					<c:otherwise>
						<td class="d-none d-md-table-cell"><format:price
								priceData="${serialProduct.finalSalePrice}" /></td>
						<td>-</td>
					</c:otherwise>
				</c:choose>

				<td class="d-none d-md-table-cell">#${serialProduct.serialId}</td>
				<td class="text-end">
					<!-- BL-537 : Added  class js-usedProduct-button --> <sec:authorize
						access="hasAnyRole('ROLE_ANONYMOUS')">
						<c:set value=" hidebutton" var="hidebutton" />
					</sec:authorize> <c:choose>
						<c:when test="${serialProduct.serialStatus eq 'ACTIVE' || serialProduct.serialStatus eq 'RECEIVED_OR_RETURNED' || serialProduct.serialStatus eq 'IN_HOUSE'}">
							<button type="button"
								data-link="<c:url value='/login/loginpopup'/>"
								class="btn btn-primary  js-login-popup hide-after-login"
								data-bs-toggle="modal" data-bs-target="#signIn"
								data-click="serial_entry_${loop.index }">
								<spring:theme code="basket.add.to.basket" />
							</button>
							<button type="button"
								class="btn btn-primary js-add-to-used-cart serial_entry_${loop.index }  ${hidebutton}"
								data-product-code="${product.code}" data-product-name="${product.name}" data-product-brand="${product.manufacturer}"
								data-bs-toggle="modal" data-bs-target="#addToCart" data-product-category="${product.categories[0].name}"
								data-serial="${serialProduct.serialId}">
								<spring:theme code="basket.add.to.basket" />
							</button>
						</c:when>
						<c:when test="${serialProduct.serialStatus eq 'ADDED_TO_CART' }">
							<button type="button"
								class="btn btn-primary js-add-to-cart js-disable-btn"
								aria-disabled="true" disabled="disabled">
								<spring:theme code="text.used.Gear.cart.button.name" />
							</button>
						</c:when>
						<c:otherwise>
						</c:otherwise>
					</c:choose>
				</td>
			</tr>
        <c:if test="${serialProduct.ugPromotionMessage ne null && serialProduct.serialPromotionPrice.value > 0 && product.onSale eq true && serialProduct.onSale eq 'true'}">
          	<tr class=" ${loop.index >= 3 ? 'hide-product-row ' : ''}">
            <td colspan="2">
               <span class="badge badge-new"><spring:theme code="text.serial.product.on.Sale"/></span>
            </td>
            <td colspan="3" class="text-start textGold">
                <strong><format:price	priceData="${serialProduct.serialPromotionPrice}"/>&nbsp;&nbsp;${fn:escapeXml(serialProduct.ugPromotionMessage)} </strong>
            </td>
          </tr>
        </c:if>
		</c:if>
	</c:if>
		</c:forEach>
	</form:form>
</tbody>