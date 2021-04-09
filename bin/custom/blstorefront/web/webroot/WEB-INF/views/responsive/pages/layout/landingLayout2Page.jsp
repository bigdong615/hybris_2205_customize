<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">
	<div class="screen"></div>
	<section id="hero">
		<div class="container">
			<div id="hero-slider" class="splide">
				<div class="splide__track">
					<ul class="splide__list">
						<cms:pageSlot position="HomePageHeroBannerSlot" var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</ul>
				</div>
			</div>
			<div id="heroSearch">
				<div class="container">
					<div class="row justify-content-center">
						<div class="col-md-10 col-lg-9">
							<div class="input-group">
								<input type="text" class="form-control"
									placeholder="<spring:theme code="text.hero.banner.searchbox.placeholder"/>">
								<input type="text" id="litepicker" class="form-control"
									placeholder="<spring:theme code="text.hero.banner.searchbox.datepicker.placeholder"/>">
								<div class="input-group-append">
									<button class="btn btn-search" type="button">
										<spring:theme code="text.footer.subscription.button.search" />
									</button>
								</div>
							</div>
						</div>
					</div>
				</div>
			</div>
		</div>
	</section>

	<section id="theProcess">
		<div class="container">
			<div class="row justify-content-center">
				<div class="col-lg-11 col-xl-9">
					<h5>
						<cms:pageSlot position="HomePageRentingGearIsEasyTitleSlot"
							var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</h5>
					<div class="row mt-5">
						<cms:pageSlot position="HomePageRentingGearSectionSlot"
							var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</div>
				</div>
			</div>
			<div class="row justify-content-center my-5">
				<div class="divider col-xl-10"></div>
			</div>
		</div>
	</section>

	<section>
		<div class="container">
			<div id="categories" class="row justify-content-center">
				<div class="col-lg-11 col-xl-9">
					<h5>
						<cms:pageSlot position="HomePageCategoriesTitleSlot" var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</h5>
					<div id="cat-slider" class="splide mt-5">
						<div class="splide__track">
							<ul class="splide__list">
								<cms:pageSlot position="HomePageCategorySectionSlot"
									var="feature">
									<cms:component component="${feature}" />
								</cms:pageSlot>
							</ul>
						</div>
					</div>
				</div>
			</div>
			<div class="row justify-content-center my-5">
				<div class="divider col-xl-10"></div>
			</div>
			<div id="brands" class="row justify-content-center mt-5">
				<div class="col-lg-11 col-xl-9">
					<h5>
						<cms:pageSlot position="HomePageBrandTitleSlot" var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</h5>
					<ul class="brand-logos pt-4 d-none d-md-flex">
						<cms:pageSlot position="HomePageBrandSectionSlot" var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</ul>
					<div class="logo-slider splide mt-4 d-block d-md-none">
						<div class="splide__track">
							<ul class="splide__list">
								<li class="splide__slide my-auto canon"><a href="#"><img
										src="assets/logo-canon.svg"></a></li>
								<li class="splide__slide my-auto sony"><a href="#"><img
										src="assets/logo-sony.svg"></a></li>
								<li class="splide__slide my-auto nikon"><a href="#"><img
										src="assets/logo-nikon.svg"></a></li>
								<li class="splide__slide my-auto dji"><a href="#"><img
										src="assets/logo-dji.svg"></a></li>
								<li class="splide__slide my-auto panasonic"><a href="#"><img
										src="assets/logo-animal-planet.svg"></a></li>
							</ul>
						</div>
					</div>

				</div>
			</div>
			<div class="row justify-content-center my-5">
				<div class="divider col-xl-10"></div>
			</div>
			<!-- BL-52 : HomePage Featured Gear section -->
			<div id="featured" class="row justify-content-center">
				<div class="col-lg-11 col-xl-9">
					<cms:pageSlot position="HomePageFeaturedGearSectionSlot" var="feature">
						<cms:component component="${feature}" />
					</cms:pageSlot>
				</div>
			</div>
		</div>
	</section>
	<section id="whyBorrow">
		<div class="container-fluid p-0">
			<cms:pageSlot position="HomePageWhyBorrowLensesSectionSlot"
				var="feature">
				<cms:component component="${feature}" />
			</cms:pageSlot>
		</div>
	</section>
	<section>
		<div class="container">
			<div id="credibility" class="row justify-content-center mt-5">
				<div class="col-lg-11 col-xl-9">
					<h5>
						<cms:pageSlot position="HomePageTheyBorrowFromUsTitleSlot"
							var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</h5>
					<ul class="brand-logos pt-4 d-none d-md-flex">
						<cms:pageSlot position="HomePageBorrowFromUsSectionSlot"
							var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</ul>
					<div class="logo-slider splide mt-5 d-block d-md-none">
						<div class="splide__track">
							<ul class="splide__list">
								<li class="splide__slide my-auto"><a href="#"><img
										src="assets/logo-netflix.svg"></a></li>
								<li class="splide__slide my-auto"><a href="#"><img
										src="assets/logo-disney.svg"></a></li>
								<li class="splide__slide my-auto"><a href="#"><img
										src="assets/logo-hbo.svg"></a></li>
								<li class="splide__slide my-auto"><a href="#"><img
										src="assets/logo-animal-planet.svg"></a></li>
								<li class="splide__slide my-auto"><a href="#"><img
										src="assets/logo-al-jazeera.svg"></a></li>
							</ul>
						</div>
					</div>
				</div>
			</div>
			<div class="row justify-content-center my-5">
				<div class="divider col-xl-10"></div>
			</div>
			<div id="stories" class="row justify-content-center">
				<div class="col-lg-11 col-xl-9">
					<h5>
						<cms:pageSlot position="HomePageFeaturedStoriesTitleSlot"
							var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</h5>
					<div id="blog-slider" class="splide mt-4">
						<div class="splide__track">
							<ul class="splide__list">
								<cms:pageSlot position="HomePageFeaturedStoriesSectionSlot"
									var="feature">
									<cms:component component="${feature}" />
					</cms:pageSlot>
							</ul>
						</div>
					</div>

				</div>
			</div>
		</div>
	</section>

</template:page>
