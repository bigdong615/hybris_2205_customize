<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<c:url var="homepageUrl" value="/" />
<c:url value="/my-account/order/"
	var="reviewOrderUrl" />
<c:url value="/checkout/printOrderConfirmation"
	var="printOrderConfirmationUrl" />
<div class="screen"></div>
<section id="confirmationWindow">
	<div class="container">
		<div class="row justify-content-center">
			<div class="col-12 col-lg-11 col-xl-10">
				<div id="orderConfirmation" class="text-center">
					<h5><spring:theme code="order.gift.card.confirmation.page.thanks.message" /></h5>

					<p><spring:theme code="order.gift.card.confirmation.page.email.message"/><br>
          <b>${orderData.user.uid}</b></p>
					<div class="confirmation-actions my-5">
						<a href="${reviewOrderUrl}${orderCode}" class="btn btn-primary mx-3 mb-4 mb-sm-0"><spring:theme
								code="order.confirmation.page.review.order.button" /></a> <a
							href="${homepageUrl}" class="btn btn-outline mx-3 mb-4 mb-sm-0"><spring:theme
								code="order.confirmation.page.continue.shopping.button" /></a>
					</div>
				</div>
			</div>
		</div>
	</div>
</section>
<!-- Featured Stories Section -->
<div id="stories" class="row justify-content-center mt-5">
	<div class="col-lg-11 col-xl-9">
		<h5>
			<cms:pageSlot
				position="OrderConfirmationPageFeaturedStoriesTitleSlot"
				var="feature">
				<cms:component component="${feature}" />
			</cms:pageSlot>
		</h5>
		<div id="blog-slider" class="splide mt-4">
			<div class="splide__track">
				<ul class="splide__list">
					<cms:pageSlot
						position="OrderConfirmationPageFeaturedStoriesSectionSlot"
						var="feature">
						<cms:component component="${feature}" />
					</cms:pageSlot>
				</ul>
			</div>
		</div>
	</div>
</div>
