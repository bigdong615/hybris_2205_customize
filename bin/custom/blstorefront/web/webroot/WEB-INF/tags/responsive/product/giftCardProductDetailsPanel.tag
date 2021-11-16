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

<!-- BL-927: Created new file for gift card product details --> 
<c:url value="/cart/usedgearadd" var="addToCartUrl" />
<div class="page-loader-new-layout">
	<img src="${themeResourcePath}/assets/bl-loader.gif" alt="<spring:theme code='giftcard.pdp.load.img.alt' />"
		title="<spring:theme code='giftcard.pdp.load.img.alt' />" id="new_loading_Img" />
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
			<div class="col-12 col-lg-11 col-xl-9">
				<div class="row">
				<div class="hide-on-desktop" id="productInfo">
					<h1 class="mb-4">${product.displayName}</h1>
				</div>
					<div id="productImage" class="col-lg-6 text-center">
						<product:GiftCardProductImagePanel galleryImages="${galleryImages}" />
					</div>
					<div id="productInfo" class="col-lg-5 offset-lg-1">
					<div class="hide-on-mobile">
					<h1 class="mb-4">${product.displayName}</h1>
					</div>
                      <c:choose>
                                        <c:when test="${allowAddToCart}">
                                              <div class="modal fade gift-card-add-t-cart-popup" id="addToCart" tabindex="-1" aria-hidden="true">
                                                   <div class="modal-dialog modal-dialog-centered modal-lg" id="addToCartModalDialog"></div>
                                              </div>
                                        </c:when>
                                        <c:otherwise>
                                              <div class="modal fade" id="addToCart" tabindex="-1" aria-hidden="true">
                                                   <div class="modal-dialog modal-dialog-centered modal-sm" id="addToCartModalDialog"></div>
                                              </div>
                                        </c:otherwise>
                                      </c:choose>     
						<form:form method="POST" modelAttribute="giftCardPurchaseForm"
							id="giftCardPurchaseForm">
							
							<%-- <c:forEach items="${product.serialproducts}"
												var="serialProduct" varStatus="loop">
												 --%>
												
							<div class="gc-pdp-form">
								<input type="text" class="form-control" id="amount"
									placeholder="<spring:theme code='giftcard.PurchaseForm.amount.placeholder' />" name="amount"> <input
									type="text" class="form-control" id="first-name"
									placeholder="<spring:theme code='giftcard.PurchaseForm.name.placeholder' />" name="name"> <input
									type="text" class="form-control" id="email"
									placeholder="<spring:theme code='giftcard.PurchaseForm.email.placeholder' />" name="email">
								<textarea class="form-control mt-2 mb-4"
									placeholder="<spring:theme code='giftcard.PurchaseForm.message.placeholder' />" name="message"></textarea>
							</div>

							<div class="notification notification-warning mb-2 gc-error-message"
								style="display: none;"></div>

							<sec:authorize access="hasAnyRole('ROLE_ANONYMOUS')">
						       <c:set value="hidebutton" var="hidebutton" />
							</sec:authorize>
							<button type="button" id="add-to-gc"
								data-link="<c:url value='/login/loginpopup'/>" 
								class="btn btn-primary btn-block mt-4 mb-0 mb-md-5  js-login-popup hide-after-login"
								data-bs-toggle="modal" data-bs-target=""
								data-click="serial_entry_">
								<spring:theme code="basket.add.to.basket" />
							</button>
							<button type="button" data-bs-toggle="modal" data-bs-target="#addToCart"
								class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 btn-gift-card-pdp js-add-to-used-cart   ${hidebutton} serial_entry_${loop.index } "
								data-product-code="${product.code}" >
								<spring:theme code="basket.add.to.basket" />
							</button>
							<%-- 	<button class="btn btn-primary btn-block mt-4 mb-0 mb-md-5"
								type="submit" id="add-to-gc"><spring:theme
							code="giftcard.pdp.addtocart" /></button> --%>
<%-- </c:forEach> --%>
						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</section>
