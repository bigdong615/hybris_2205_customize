<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">
	<div class="screen"></div>
	<div id="heroSearch" class="d-none d-md-block blHomePage-search-bar">
				<div class="container">
					<div class="row justify-content-center">
						<div class="col-12">
							<cms:pageSlot position="HomePageBannerSearchBoxSlot" var="feature">
								<cms:component component="${feature}" />
							</cms:pageSlot>
						</div>
					</div>
				</div>
			</div>
		<!-- BL-360 : HomePage Mobile device Search Box Section -->
    	<div class="wrapup-search">
    	<section id="globalSearch" class="d-md-none">
    		<cms:pageSlot position="MobileHomePageBannerSearchBoxSlot" var="feature">
    			<cms:component component="${feature}" />
    		</cms:pageSlot>
    	</section>
    	</div>
    	
    	<div id="bodySection">
	<section id="hero">

		<div class="container">
			<div id="hero-slider" class="splide">
				<div class="splide__track">
						<cms:pageSlot position="HomePageHeroBannerSlot" var="feature" element="ul" class="splide__list">
							<cms:component component="${feature}" element="li" class="splide__slide"/>
						</cms:pageSlot>
				</div>
			</div>
		</div>
	</section>
	

	<section id="theProcess">
		<div class="container">
			<!-- BL-52 : HomePage Featured Gear section -->
			<div class="page-loader-new-layout">
				<img src="${themeResourcePath}/assets/bl-loader.gif" alt="Loading.." title="Loading.." id="new_loading_Img">
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
							<cms:pageSlot position="HomePageFeaturedStoriesSectionSlot" var="feature" element="ul" class="splide__list">
								<li class="splide__slide">
									<cms:component component="${feature}" />
								</li>
							</cms:pageSlot>
						</div>
					</div>

				</div>
			</div>
			<div id="featured" class="row justify-content-center">
				<div class="col-lg-11 col-xl-9">
					<c:choose>
						<c:when test="${allowAddToCart || isRentalCart}">
							<div class="modal fade" id="addToCart" tabindex="-1" aria-hidden="true">
								<div class="modal-dialog modal-dialog-centered modal-lg" id="addToCartModalDialog"></div>
							</div>
						</c:when>
						<c:otherwise>
							<div class="modal fade" id="addToCart" tabindex="-1" aria-hidden="true">
								<div class="modal-dialog modal-dialog-centered modal-sm" id="addToCartModalDialog"></div>
							</div>
						</c:otherwise>
					</c:choose>
					<cms:pageSlot position="HomePageFeaturedGearSectionSlot" var="feature">
						<cms:component component="${feature}" />
					</cms:pageSlot>
				</div>
			</div>
			<div class="row justify-content-center">
				<div class="col-lg-11 col-xl-9 mt-5">
					<h5>
						<cms:pageSlot position="HomePageRentingGearIsEasyTitleSlot"
							var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</h5>
					<cms:pageSlot position="HomePageRentingGearSectionSlot" var="feature" element="div" class="row mt-5">
						<cms:component component="${feature}" element="div" class="col-6 col-md-3"/>
				 	</cms:pageSlot>
				</div>
			</div>
			<div class="row justify-content-center my-4">
				<div class="divider col-xl-10"></div>
			</div>
		</div>
	</section>

	<section id="categoriesSection">
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
								<cms:pageSlot position="HomePageCategorySectionSlot"
									var="feature" element="ul" class="splide__list">
									<cms:component component="${feature}" element="li" class="splide__slide"/>
								</cms:pageSlot>
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
				<%--  	<ul class="brand-logos pt-4 d-md-flex">
						<cms:pageSlot position="HomePageBrandSectionSlot" var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</ul>  --%>
					<!-- BL-351 : Homepage Mobile Device Brands Section --> 
					<!-- <div class="logo-slider splide mt-4 d-none d-md-none"> -->
					<div id="logo-slider" class="splide mt-4 brand-logos">
						<div class="splide__track">
								<cms:pageSlot position="HomePageBrandSectionSlot"
									var="feature" element="ul" class="splide__list">
									<cms:component component="${feature}" element="li" class="splide__slide my-auto"/>
								</cms:pageSlot>
						</div>
					</div>

				</div>
			</div>
			<div class="row justify-content-center my-5">
				<div class="divider col-xl-10"></div>
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

			<!--BL-2010 :- commented out code for "they borrow from us section" -->
		<!--	<div id="credibility" class="row justify-content-center mt-5">
				<div class="col-lg-11 col-xl-9"> -->
				<!--	<h5>
						<cms:pageSlot position="HomePageTheyBorrowFromUsTitleSlot"
							var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot>
					</h5>  -->
				<!--	<ul class="brand-logos pt-4 d-none d-md-flex"> -->
						<cms:pageSlot position="HomePageBorrowFromUsSectionSlot" var="feature">
							<cms:component component="${feature}" />
						</cms:pageSlot> 
					<!-- </ul>  -->
					<!-- BL-345 : HomePage Mobile Device They Borrow from us section -->
					<!--BL-2010 :- commented out code for "they borrow from us section" -->
				<!-- <div class="logo-slider splide mt-5 d-block d-md-none">
						<div class="splide__track">
							<ul class="splide__list"> -->
								<cms:pageSlot position="MobileHomePageBorrowFromUsSectionSlot"
									var="feature">
									<cms:component component="${feature}" />
								</cms:pageSlot>
						<!--	</ul>
						</div>
					</div>
				</div>
			</div>
			<div class="row justify-content-center my-5">
				<div class="divider col-xl-10"></div>
			</div> -->
		</div>
	</section>
</div>
</template:page>
