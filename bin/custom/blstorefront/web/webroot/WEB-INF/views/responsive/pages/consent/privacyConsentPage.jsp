<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<template:page pageTitle="${pageTitle}">
   <section id="about-us" class="p-0">
      <div class="container row">
         <cms:pageSlot position="BodyContent" var="feature">
         							${feature.content}
         </cms:pageSlot>
      </div>
      <br>
   </section>
</template:page>
