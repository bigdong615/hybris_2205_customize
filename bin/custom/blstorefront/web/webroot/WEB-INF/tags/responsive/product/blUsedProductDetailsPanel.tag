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
<div class="page-loader-new-layout">
	<img src="${themeResourcePath}/assets/bl-loader.gif" alt="Loading.."
		title="Loading.." id="new_loading_Img" />
</div>
<div class="screen"></div>
<cms:pageSlot position="SearchBoxBl" var="component">
	<cms:component component="${component}" />
</cms:pageSlot>
<section id="theProduct">
	<div class="container">
		<div class="row justify-content-center">
			<div class="col-12">
				<cms:pageSlot position="BreadcrumbSection" var="feature">
					<cms:component component="${feature}" />
				</cms:pageSlot>
			</div>
		</div>
		<div class="row justify-content-center">
			<div class="col-12 col-lg-11 col-xl-10">
				<div class="row">
					<div id="productImage" class="col-lg-5 text-center">
						<product:productImagePanel galleryImages="${galleryImages}" />
					</div>
					<div id="productInfo" class="col-lg-6 offset-lg-1">
						<p class="overline">${fn:toUpperCase(product.manufacturer)}</p>
						<h1 class="mb-4">${product.displayName}</h1>
						<table id="usedProductList">
							<c:choose>
								<c:when
									test="${not empty product.hasIncentivizedPrice and product.hasIncentivizedPrice }">
									<product:blSerialIncentivizedPriceDataPanel />
								</c:when>
								<c:otherwise>
									<thead>
										<tr>
											<th><spring:theme code="pdp.serial.table.rating.text" /></th>
											<th><spring:theme code="pdp.serial.table.price.text" /></th>
											<th class="d-none d-md-table-cell"><spring:theme
													code="pdp.serial.table.number.text" /></th>
											<th></th>
										</tr>
									</thead>
									<tbody>
										<c:if
											test="${(allowAddToCart ne 'true') && (isRentalCart eq 'true') && (isUsedGearCart ne 'true')}">
											<div class="modal fade" id="addToCart" tabindex="-1"
												aria-hidden="true">
												<div class="modal-dialog modal-dialog-centered modal-sm"
													id="addToCartModalDialog"></div>
											</div>
										</c:if>
										<form:form id="serialSubmitForm" action="${addToCartUrl}"
											method="get">
											<c:forEach items="${product.serialproducts}"
												var="serialProduct" varStatus="loop">
												
												<c:if test="${serialProduct.serialStatus ne 'SOLD' or (product.forRent eq true and serialProduct.isSerialNotAssignedToRentalOrder eq true) }">
													<tr class=" ${loop.index >= 3 ? 'hide-product-row' : ''}">
														<td><a href="#" data-bs-toggle="modal"
															data-bs-target="#sku52678"
															data-cosmetic="${serialProduct.cosmeticRating}"
															data-functional="${serialProduct.functionalRating}"
															data-condition-rating="${serialProduct.conditionRating}"
															data-serial-id="${serialProduct.serialId}"
															class="js-conditional-rating-popup">${serialProduct.conditionRating}</a></td>
														<td><format:price
																priceData="${serialProduct.finalSalePrice}" /></td>
														<td class="d-none d-md-table-cell">#
															${serialProduct.serialId}</td>
														<td class="text-end">
															<!-- BL-537 : Added  class js-usedProduct-button -->
															 <sec:authorize
																access="hasAnyRole('ROLE_ANONYMOUS')">
																<c:set value=" hidebutton" var="hidebutton" />
															</sec:authorize> 
															<c:choose>
																<c:when test="${serialProduct.serialStatus eq 'ACTIVE' }">
																	<button type="button"
																		data-link="<c:url value='/login/loginpopup'/>"
																		class="btn btn-primary  js-login-popup hide-after-login"
																		data-bs-toggle="modal" data-bs-target="#signIn"
																		data-click="serial_entry_${loop.index }">
																		<spring:theme code="basket.add.to.basket" />
																	</button>
																	<button type="button"
																		class="btn btn-primary bl-serial-add  serial_entry_${loop.index }  ${hidebutton}"
																		data-product-code="${product.code}"
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
												</c:if>
											</c:forEach>
										</form:form>
									</tbody>
								</c:otherwise>
							</c:choose>
						</table>
						<c:if test="${product.serialproducts.size() >3}">
							<!--BL-573  and BL-572  added class showmore-margintop and mt-2 -->
							<p class="mt-2">
								<a href="#" id="showmore" class="showmore-margintop"><spring:theme
										code="pdp.show.more.button.text" /></a>
						</c:if>
						<c:if test="${product.forRent}">
							<c:url var="rentUrl" value="/rent/product/${product.code}" />
							<!--  BL:573 and  572 mt-4 added -->
							<a href="${rentUrl}"
								class="btn btn-sm btn-secondary float-end mt-4"><spring:theme
									code="pdp.product.rent.instead.button.text" /></a>
							</p>
						</c:if>
					</div>
				</div>
			</div>
		</div>
	</div>
</section>

<section id="theUsedProcess">
	<div class="container">
		<div class="row justify-content-center">
			<div class="col-lg-11 col-xl-9">
				<cms:pageSlot position="buyingUsedGearEasySection" var="feature">
					<cms:component component="${feature}" />
				</cms:pageSlot>
			</div>
		</div>
	</div>
</section>

<section id="productExtras">
	<div class="container">
		<div class="row justify-content-center">
			<div class="col-lg-11 col-xl-9">
				<hr>
				<!-- Product overview -->
				<a class="filter-expand" data-bs-toggle="collapse" href="#overview"
					role="button" aria-expanded="true" aria-controls="overview">
					<h5>
						<spring:theme code="pdp.overview.section.text" />
					</h5>
				</a>
				<div class="collapse show" id="overview">
					<p>${ycommerce:sanitizeHTML(product.usedDescription)}</p>
					<product:productVideo productVideos="${product.usedGearVideosLink}" />
				</div>
				<hr>
				<!--Product specification -->
				<product:specification />
				<!-- Product include -->
				<product:productInclude />
				<!-- Product notes -->
				<product:productNotes />
				<!-- product resource  -->
				<product:resource />
				<div id="gear-slider" class="splide mt-4"></div>
			</div>
		</div>
	</div>
</section>



