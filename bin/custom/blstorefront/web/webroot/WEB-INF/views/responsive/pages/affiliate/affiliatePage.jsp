<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">
	<div class="screen"></div>
	<section id="confirmationWindow">
		<div class="container">
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
				<div class="container2">
					<cms:pageSlot position="Section5Slot" var="feature">
						<cms:component component="${feature}" />
					</cms:pageSlot>
					<cms:pageSlot position="Section6Slot" var="feature">
						<cms:component component="${feature}" element="div" class="accordion-data" />
					</cms:pageSlot>
					<div id="affiliate-accordion" class="accordion-container">
					</div>
				</div>
			</div>
		</div>
	</section>
	<script>

	</script>
</template:page>