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
			<div class="col-12 col-lg-11 col-xl-9">
				<div class="row">
					<div id="productImage" class="col-lg-6 text-center">
						<product:GiftCardProductImagePanel galleryImages="${galleryImages}" />
					</div>
					<div id="productInfo" class="col-lg-5 offset-lg-1">

						<h1 class="mb-4">${product.displayName}</h1>

						<form:form method="POST" modelAttribute="giftCardPurchaseForm"
							id="giftCardPurchaseForm">
							<div class="gc-pdp-form">
								<input type="text" class="form-control" id="amount"
									placeholder="$25 - $500" name="amount"> <input
									type="text" class="form-control" id="first-name"
									placeholder="Recipient's name" name="name"> <input
									type="text" class="form-control" id="email"
									placeholder="Recipient's email" name="email">
								<textarea class="form-control mt-2 mb-4"
									placeholder="Recipient's message" name="message"></textarea>
							</div>
							<div class="notification notification-warning mb-2"
								style="display: none;"><spring:theme
							code="giftcard.pdp.amount.limit" /></div>
							<button class="btn btn-primary btn-block mt-4 mb-0 mb-md-5"
								type="submit" id="add-to-gc"><spring:theme
							code="giftcard.pdp.addtocart" /></button>

						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</section>
