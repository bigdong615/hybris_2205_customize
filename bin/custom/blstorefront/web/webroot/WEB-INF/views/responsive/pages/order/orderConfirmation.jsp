<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">
	<div class="screen"></div>
	<section id="confirmationWindow">
		<div class="container">
			<div class="row justify-content-center">
				<div class="col-12 col-lg-11 col-xl-10">
					<div id="orderConfirmation" class="text-center">
						<h1><spring:theme code="order.confirmation.page.thanks.message"/></h1>
						<h5 class="mb-5">
							<spring:theme code="order.confirmation.page.order.number" arguments="${orderCode}"/><br>(Jan 31 - Feb 2)
						</h5>
						<div class="notification notification-tip check d-inline-block mb-5">
							<b><spring:theme code="order.confirmation.page.gift.card" arguments="BL30OFF"/></b> 
							<spring:theme code="order.confirmation.page.remaining.balance" arguments="$49.99"/>
						</div>
						<p>
							<spring:theme code="order.confirmation.page.email.to"/><br> <b>${orderData.user.uid}</b>
						</p>
						<div class="confirmation-actions my-5">
							<a href="#" class="btn btn-primary mx-3 mb-4 mb-sm-0"><spring:theme code="order.confirmation.page.review.order.button"/></a>
							<a href="#" class="btn btn-outline mx-3 mb-4 mb-sm-0"><spring:theme code="order.confirmation.page.continue.shopping.button"/></a>
						</div>
						<div class="order-actions text-center mb-4">
							<a href="#" class="mx-2" alt="Print Order">
								<i class="icon-print"></i>
							</a>
							<a href="#" class="mx-2">
								<i class="icon-save" alt="Save Order"></i>
							</a>
						</div>
					</div>
				</div>
			</div>
		</div>
	</section>
	<!-- Featured Stories Section -->
	<div id="stories" class="row justify-content-center">
		<div class="col-lg-11 col-xl-9">
			<h5>
				<cms:pageSlot position="OrderConfirmationPageFeaturedStoriesTitleSlot" var="feature">
					<cms:component component="${feature}" />
				</cms:pageSlot>
			</h5>
			<div id="blog-slider" class="splide mt-4">
				<div class="splide__track">
					<ul class="splide__list">
						<cms:pageSlot position="OrderConfirmationPageFeaturedStoriesSectionSlot" var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</ul>
				</div>
			</div>
		</div>
	</div>
</template:page>
