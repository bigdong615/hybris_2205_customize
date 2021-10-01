<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<%-- Changes for BL-66 --%>
<c:if test="${positionAttribute != 'SocialMediaSectionSlot' && positionAttribute != 'buyingUsedGearEasySection'}">
<section id="featuredPromo">
        <div class="container">
            <div class="row">
                <div class="col-12">
                  <img class="promoImg" src="${media.url}">
                <span> ${headline} <a href="${urlLink}"> <b> ${urlLink} </b> </a> </span>

                </div>
            </div>
        </div>
</section>
</c:if>
<%-- Ends here --%>

	<c:if test="${positionAttribute == 'SocialMediaSectionSlot'}">
	  <div class="social">
			<c:forEach items="${feature.medias}" var="image">
		   	<a href="${image.redirectUrl}" target="_blank">
		  	<img src="${image.url}">
		   	</a>
			</c:forEach>
	  </div>
	</c:if>
	<c:if test="${positionAttribute == 'buyingUsedGearEasySection'}">
			<h5>${feature.headline}</h5>
        <div class="row mt-5">
            <c:forEach items="${feature.medias}" var="image">
           <div class="col-6 col-md-3 text-center">
                  <img src="${image.url}" />
                  <h6>${image.description}</h6>
           </div>
           </c:forEach>
        </div>
	</c:if>

	<c:if test="${positionAttribute == 'Section1Slot' || positionAttribute == 'Section2Slot' || positionAttribute == 'Section3Slot' || positionAttribute == 'Section4Slot'}">
              <c:forEach items="${feature.medias}" var="image">
             <div class="col-6 col-md-3 text-center mb-5">
                    <img src="${image.url}" />
             </div>
             </c:forEach>
  	</c:if>
 
