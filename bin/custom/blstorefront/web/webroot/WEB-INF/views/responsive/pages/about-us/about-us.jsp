<%@ page trimDirectiveWhitespaces="true"%> <%@ taglib prefix="c"
uri="http://java.sun.com/jsp/jstl/core"%> <%@ taglib prefix="template"
tagdir="/WEB-INF/tags/responsive/template"%> <%@ taglib prefix="cms"
uri="http://hybris.com/tld/cmstags"%> <%@ taglib prefix="spring"
uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">
    <script type="text/javascript" src="https://c.la2-c1-iad.salesforceliveagent.com/content/g/js/48.0/deployment.js">
    </script>
    <div class="screen"></div>
    <section id="about-us" class="p-0">
        <div class="container">
            <div class="main-section-shipit row justify-content-center ">
                <div class="col-xl-10 text-center specialofferpage about-us">
                    <cms:pageSlot position="Section1Slot" var="feature">
                        <cms:component component="${feature}" />
                    </cms:pageSlot>


                </div>
                <cms:pageSlot position="Section2Slot" var="feature">
                    <cms:component component="${feature}" />
                </cms:pageSlot>

                <cms:pageSlot position="Section3Slot" var="feature">
                    <cms:component component="${feature}" />
                </cms:pageSlot>

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