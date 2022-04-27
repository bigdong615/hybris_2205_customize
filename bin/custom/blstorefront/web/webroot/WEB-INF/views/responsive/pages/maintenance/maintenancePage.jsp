<%@ page trimDirectiveWhitespaces="true"%> <%@ taglib prefix="c"
uri="http://java.sun.com/jsp/jstl/core"%> <%@ taglib prefix="template"
tagdir="/WEB-INF/tags/responsive/template"%> <%@ taglib prefix="cms"
uri="http://hybris.com/tld/cmstags"%> <%@ taglib prefix="spring"
uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">
  <div class="screen"></div>
  <section id="maintainace">
    <div class="container">
      <div class="main-section-conatctus row justify-content-center">
        <div class="col-xl-8 col-xl-offset-2">
          <cms:pageSlot position="Section1Slot" var="feature">
            <cms:component component="${feature}" />
          </cms:pageSlot>
            <cms:pageSlot position="Section2Slot" var="feature">
              <cms:component component="${feature}" />
            </cms:pageSlot>
          <cms:pageSlot position="Section3Slot" var="feature">
              <cms:component component="${feature}" />
            </cms:pageSlot>
        </div>
      </div>
    </div>
  </section>
  <script></script>
</template:page>
