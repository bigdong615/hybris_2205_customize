<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">
	<div class="container search-container">
		<cms:pageSlot position="AffiliatePageSearchBoxSlot" var="feature">
			<cms:component component="${feature}" />
		</cms:pageSlot>
	</div>
	<section id="confirmationWindow" class="pad-none">
		<div class="container">
			<cms:pageSlot position="Section0Slot" var="feature">
				<cms:component component="${feature}" />
			</cms:pageSlot>
			<div class="main-section-affiliate">
				<cms:pageSlot position="Section1Slot" var="feature">
					<h1>
						<cms:component component="${feature}" />
					</h1>
				</cms:pageSlot>
				<cms:pageSlot position="Section2Slot" var="feature">
					<cms:component component="${feature}" />
				</cms:pageSlot>

				<cms:pageSlot position="Section3Slot" var="feature">
					<cms:component component="${feature}" />
				</cms:pageSlot>
				<cms:pageSlot position="Section4Slot" var="feature">
					<cms:component component="${feature}" />
				</cms:pageSlot>

			</div>
			<div class="container2 row justify-content-center" id="affiliate">
				<div class="col-xl-8 col-lg-12 col-md-12">
					<cms:pageSlot position="Section5Slot" var="feature">
						<cms:component component="${feature}" />
					</cms:pageSlot>
					<cms:pageSlot position="Section6Slot" var="feature">
						<cms:component component="${feature}" />
					</cms:pageSlot>
				</div>
			</div>
		</div>
	</section>
	<script>

	</script>
</template:page>