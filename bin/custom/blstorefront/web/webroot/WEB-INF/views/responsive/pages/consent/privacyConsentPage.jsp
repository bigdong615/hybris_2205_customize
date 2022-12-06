<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<template:page pageTitle="${pageTitle}">
   <section id="about-us" class="p-0">
      <div class="container">
         <cms:pageSlot position="BodyContent" var="feature">
         							${feature.content}
         </cms:pageSlot>
      </div>
      <br>
   </section>
</template:page>

<script>
        function displayConsent() {
            var ele = document.getElementsByName('privacy-consent');
            var baseUrl= window.location.origin;
            var checkboxSelectedUrl ="";

            for(i = 0; i < ele.length; i++) {
                if(ele[i].checked)
                checkboxSelectedUrl= ele[i].value
            }
           var redirectUrl= encodeURI(ACC.config.encodedContextPath+checkboxSelectedUrl);
           alert(redirectUrl);
            if(checkboxSelectedUrl != ""){
            window.location.href = redirectUrl;
            }
        }
    </script>