<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>

<thead>
	<tr>
		<th><spring:theme code="pdp.serial.table.rating.text" /></th>
		<th class="d-none d-md-table-cell"><spring:theme code="pdp.serial.table.price.text" /></th>
		<th><spring:theme code="pdp.serial.table.incentivized.price.text" /></th>
		<th class="d-none d-md-table-cell"><spring:theme code="pdp.serial.table.number.text" /></th>
		<th></th>
	</tr>
</thead>
<tbody>
	<c:forEach items="${product.serialproducts}" var="serialProduct"
		varStatus="loop">
		<tr class=" ${loop.index >= 3 ? 'hide-product-row' : ''}">
			<td><a href="#" data-bs-toggle="modal"
				data-bs-target="#sku52678" data-cosmetic="${serialProduct.cosmetic}" data-functional="${serialProduct.functional}"
				data-condition-rating="${serialProduct.conditionRating}" data-serial-id = "${serialProduct.serialId}" class="js-conditional-rating-popup">${serialProduct.conditionRating}</a></td>
				<c:choose>
					<c:when test="${not empty serialProduct.finalIncentivizedPrice}">
						<td class="d-none d-md-table-cell"><strike><format:price priceData="${serialProduct.finalSalePrice}" /></strike></td>
						<td><format:price priceData="${serialProduct.finalIncentivizedPrice}" /></td>
					</c:when>
					<c:otherwise>
						<td class="d-none d-md-table-cell"><format:price priceData="${serialProduct.finalSalePrice}" /></td>
						<td>-</td>
					</c:otherwise>
				</c:choose>
			
			<td class="d-none d-md-table-cell"># ${serialProduct.serialId}</td>
			<td class="text-end"><a href="#" class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#addToCart"><spring:theme code="basket.add.to.basket" /></a></td>
		</tr>
	</c:forEach>
</tbody>