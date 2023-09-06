<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<template:page pageTitle="${pageTitle}">
	
	<c:url value="/" var="homePageUrl" />


	<cms:pageSlot position="MiddleContent" var="comp" element="div" class="errorNotFoundPageMiddle">
		<cms:component component="${comp}" element="div" class="errorNotFoundPageMiddle-component" />
	</cms:pageSlot>
	<cms:pageSlot position="BottomContent" var="comp" element="div" class="errorNotFoundPageBottom">
		<cms:component component="${comp}" element="div" class="errorNotFoundPageBottom-component"/>
	</cms:pageSlot>
	<cms:pageSlot position="SideContent" var="feature" element="div" class="errorNotFoundPageSide">
		<cms:component component="${feature}" element="div" class="errorNotFoundPageSide-component"/>
	</cms:pageSlot>
	<div class="container">
       <div class="row justify-content-center">
          <div class="col-xl-12 text-center">
             <div class="row justify-content-center">
                <div class="page404">
                   <h1>OOPS...</h1>
                   <h6 class="mb-4">Sorry, this page is missing. Good thing the gear we rent isn't.</h6>
                   <div>
                      <a href="/search/?text=&amp;blPageType=rentalGear" class="btn btn-rent-gear">RENT GEAR</a>
                   </div>
                   <img src="/_ui/responsive/theme-bltheme/assets/page404-img.png" alt="this page is missing" />
                </div>
             </div>
          </div>
       </div>
    </div>



	


</template:page>