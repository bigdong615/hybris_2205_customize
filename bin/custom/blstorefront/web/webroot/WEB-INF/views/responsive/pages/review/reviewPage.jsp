<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="common" tagdir="/WEB-INF/tags/responsive/common"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<link rel="stylesheet" type="text/css" media="screen" href="${fn:escapeXml(themeResourcePath)}/css/reviewPage.css"/>
<script src="https://ui.powerreviews.com/stable/4.1/ui.js" async></script>

<template:page pageTitle="${pageTitle}">
    <div class="container search-container">
		<cms:pageSlot position="reviewPageSlot" var="feature">
			<cms:component component="${feature}" />
		</cms:pageSlot>
    </div>

    <section id="">
     <div class="container">
      <div class="wrapper">
            <div class="grid">
               <div class="grid__item large--five-sixths push--large--one-twelfth">
                  <header class="section-header text-center">
                     <h1>Share Your Gear Review</h1>
                     <p>Love it or leave it? Let everyone know what you think about the gear you use whether you rented it from us or it's part of your own kit</p>
                     <hr class="hr--small">
                  </header>
                  <div class="grid">
                   <div class="grid__item large--four-fifths push--large--one-tenth">
                     <div class="rte rte--nomargin rte--indented-images">
                          <c:if test="${not empty productCode}">
                           <div id="pr-write"></div>
                          </c:if>
                     </div>
                   </div>
                 </div>
               </div>
            </div>
         </div>
         <script>
            window.pwr = window.pwr || function () {
                 (pwr.q = pwr.q || []).push(arguments);
                };
                pwr("render", {
                  api_key: '${merchantAPI_Key}',
                  locale: 'en_US',
                  merchant_group_id: '${merchantGroupId}',
                  merchant_id: '${merchantID}',
                  on_submit:function(config, data){
                        window.scrollTo(0,0);
                        },
                  components: {
                     Write: 'pr-write',
                  }
                });
         </script>
     </div>
    </section>

</template:page>
