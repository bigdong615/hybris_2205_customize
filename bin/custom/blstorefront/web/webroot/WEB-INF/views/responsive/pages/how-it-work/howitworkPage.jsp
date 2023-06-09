<%@ page trimDirectiveWhitespaces="true"%> <%@ taglib prefix="c"
uri="http://java.sun.com/jsp/jstl/core"%> <%@ taglib prefix="template"
tagdir="/WEB-INF/tags/responsive/template"%> <%@ taglib prefix="cms"
uri="http://hybris.com/tld/cmstags"%> <%@ taglib prefix="spring"
uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">
  <div class="container search-container">
		<cms:pageSlot position="HowItWorkPageSearchBoxSlot" var="feature">
			<cms:component component="${feature}" />
		</cms:pageSlot>
    </div>
  <section id="">
    <div class="container">
      <cms:pageSlot position="Section0Slot" var="feature">
				<cms:component component="${feature}" />
			</cms:pageSlot>
      <div class="main-section-conatctus row justify-content-center specialofferpage">
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
          
          <cms:pageSlot position="Section4Slot" var="feature">
            <cms:component component="${feature}" />
          </cms:pageSlot>
          <cms:pageSlot position="Section5Slot" var="feature">
            <cms:component component="${feature}" />
          </cms:pageSlot>
          <cms:pageSlot position="Section6Slot" var="feature">
            <cms:component component="${feature}" />
          </cms:pageSlot>
          <cms:pageSlot position="Section7Slot" var="feature">
            <cms:component component="${feature}" />
          </cms:pageSlot>
          <cms:pageSlot position="Section8Slot" var="feature">
            <cms:component component="${feature}" />
          </cms:pageSlot>
          <cms:pageSlot position="Section9Slot" var="feature">
            <cms:component component="${feature}" />
          </cms:pageSlot>
        </div>
      </div>
    </div>
  </section>
  <script></script>
</template:page>
