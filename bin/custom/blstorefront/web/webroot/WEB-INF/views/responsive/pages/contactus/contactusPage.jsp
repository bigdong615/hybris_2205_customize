<%@ page trimDirectiveWhitespaces="true"%> <%@ taglib prefix="c"
uri="http://java.sun.com/jsp/jstl/core"%> <%@ taglib prefix="template"
tagdir="/WEB-INF/tags/responsive/template"%> <%@ taglib prefix="cms"
uri="http://hybris.com/tld/cmstags"%> <%@ taglib prefix="spring"
uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">
  <script
    type="text/javascript"
    src="https://c.la2-c1-iad.salesforceliveagent.com/content/g/js/48.0/deployment.js"
  ></script>
  <div class="screen"></div>
  <section id="">
    <div class="container">
      <div class="main-section-conatctus row justify-content-center contact-live-chat">
        <div class="col-xl-8 col-xl-offset-2">
          <cms:pageSlot position="Section1Slot" var="feature">
            <cms:component component="${feature}" />
          </cms:pageSlot>
          <div class="notification notification-tip check mb-4 text-start">
            <cms:pageSlot position="Section2Slot" var="feature">
              <cms:component component="${feature}" />
            </cms:pageSlot>
          </div>
          <div class="row">
            <div id="contactUsContent" class="col-lg-12">
              <cms:pageSlot position="Section3Slot" var="feature">
                <cms:component component="${feature}" />
              </cms:pageSlot>
            </div>
          </div>
          <cms:pageSlot position="Section4Slot" var="feature">
            <cms:component component="${feature}" />
          </cms:pageSlot>
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
  <script></script>
</template:page>
