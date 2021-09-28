<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<template:page pageTitle="${pageTitle}">

<cms:pageSlot position="SearchBoxBl" var="component">
				<cms:component component="${component}"/>
</cms:pageSlot>

<%-- Changes for BL-66 --%>
	<c:choose>
		<c:when test="${blPageType eq 'rentalgear' and cmsPage.uid eq 'productGrid'}">
			<cms:pageSlot position="BlRentalGearBanner" var="component">
				<cms:component component="${component}" />
			</cms:pageSlot>
		</c:when>
		<c:when test="${blPageType eq 'usedGear' and cmsPage.uid eq 'productGrid' and isNewGearCategory ne true}">
		<cms:pageSlot position="BlUsedGearBanner" var="component">
				<cms:component component="${component}" />
			</cms:pageSlot>
		</c:when>
	</c:choose>
<%-- Ends here --%>

	<cms:pageSlot position="Section1" var="feature" element="div" class="product-grid-section1-slot">
		<cms:component component="${feature}" element="div" class="yComponentWrapper map product-grid-section1-component"/>
</cms:pageSlot>

<section id="products">
   <div class="container">
      <div class="row">
            <cms:pageSlot position="ProductLeftRefinements" var="feature" >
               <cms:component component="${feature}"/>
            </cms:pageSlot>
            <cms:pageSlot position="ProductGridSlot" var="feature">
               <cms:component component="${feature}"/>
            </cms:pageSlot>
      </div>
   </div>
</section>
<section id="seoContent">
   <div class="container">
      <div class="row justify-content-center my-5">
         <div class="divider col-12"></div>
      </div>
      <div id="catDescription" class="row justify-content-center">
         <div class="col-xl-10">
            <p> ${footerContent} </p>
         </div>
      </div>
   </div>
</section>
</template:page>