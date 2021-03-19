<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>

<template:page pageTitle="${pageTitle}">


<section id="products">
   <div class="container">
      <div class="row">
         <div class="col-lg-3">
            <cms:pageSlot position="ProductLeftRefinements" var="feature" element="div" class="product-grid-left-refinements-slot">
               <cms:component component="${feature}" element="div" class="yComponentWrapper product-grid-left-refinements-component"/>
            </cms:pageSlot>
         </div>
         <div class="col-sm-12 col-md-9">
            <cms:pageSlot position="ProductGridSlot" var="feature" element="div" class="product-grid-right-result-slot">
               <cms:component component="${feature}" element="div" class="product__list--wrapper yComponentWrapper product-grid-right-result-component"/>
            </cms:pageSlot>
         </div>
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
            <h1>Camera Rentals</h1>
            <p>Rent camera bodies from Canon, Sony, Nikon, Panasonic, RED, & more at BorrowLenses. Many photography enthusiasts love pursuing the fleeting opportunities for the perfect image. The moment when the light strikes a surface perfectly, letting you see a new side of a familiar setting is part of what makes photography so rewarding. Video also presents artists with a new way of looking at the world. BorrowLenses makes renting cameras a breeze, with leading equipment options that give you access to the same items used by the pros. This is a great way to train on equipment considered standard for the photography, video, and film industries. Not sure what gear to rent? Expand your personal horizons with the help of our team BL gear experts who can help you choose the right gear for your shoot.</p>
         </div>
      </div>
   </div>
</section>
</template:page>