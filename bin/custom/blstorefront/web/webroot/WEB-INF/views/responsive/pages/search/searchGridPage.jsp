<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>
<%@ taglib prefix="storepickup" tagdir="/WEB-INF/tags/responsive/storepickup" %>


<template:page pageTitle="${pageTitle}">

<cms:pageSlot position="SearchBoxBl" var="component">
				<cms:component component="${component}"/>
</cms:pageSlot>

<section id="products">
   <div class="container">
      <div class="row">
			<cms:pageSlot position="ProductLeftRefinements" var="feature" >
				<cms:component component="${feature}"/>
			</cms:pageSlot>
			<cms:pageSlot position="SearchResultsGridSlot" var="feature">
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
			 <p>  </p>
		  </div>
	   </div>
	</div>
 </section>

</template:page>