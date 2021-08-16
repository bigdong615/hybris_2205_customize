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
					<h1>
						<spring:theme code="order.confirmation.page.thanks.message" />
					</h1>
					<h5 class="mb-5">
						<spring:theme code="order.confirmation.page.order.number"
							arguments="${orderCode}" /><br/>
						<c:if test="${cartData.isNewGearOrder  eq false}">
						(${orderData.rentalDates.selectedFromDate} - ${orderData.rentalDates.selectedToDate})
						</c:if>
					</h5>
					<c:if test="${not empty orderData.giftCardData}">
						<div
							class="notification notification-tip check d-inline-block mb-5">
							<c:forEach items="${orderData.giftCardData}" var="gift"
								varStatus="loop">
								<b><spring:theme code="order.confirmation.page.gift.card"
										arguments="${fn:escapeXml(gift.code)}" /></b>
								<spring:theme code="order.confirmation.page.remaining.balance" />&nbsp;<format:price
									priceData="${gift.balanceamount}" />
								</br>
							</c:forEach>
						</div>
					</c:if>
					<p>
						<spring:theme code="order.confirmation.page.email.to" />
						<br> <b>${orderData.user.uid}</b>
					</p>
					<div class="confirmation-actions my-5">
						<a href="${reviewOrderUrl}${orderCode}" class="btn btn-primary mx-3 mb-4 mb-sm-0"><spring:theme
								code="order.confirmation.page.review.order.button" /></a> <a
							href="${homepageUrl}" class="btn btn-outline mx-3 mb-4 mb-sm-0"><spring:theme
								code="order.confirmation.page.continue.shopping.button" /></a>
					</div>
        <c:if test="${cartData.isNewGearOrder  eq false}">
					<div class="order-actions text-center mb-4">
            <form action="${printOrderConfirmationUrl}" id="printOrderConfirmationForm" method="GET">
              <input type="hidden" id="orderCode" name="orderCode" value="${orderCode}"/>
            </form>
						<a href="#" class="mx-2 js-print-quote" alt="Print Order" id="printOrderConfirmation" data-pagetype="${pageType}"> <i
							class="icon-print"></i>
						</a>
					</div>
				</div>
			</c:if>s
			</div>
		</div>
	</div>
</section>
<!-- Featured Stories Section -->
<div id="stories" class="row justify-content-center">
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
