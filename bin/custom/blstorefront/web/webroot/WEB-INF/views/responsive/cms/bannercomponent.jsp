<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>


<c:choose>
	<c:when test="${positionAttribute == 'SocialMediaSectionSlot'}">
	${feature}ttttttttt
	<div class="social">
			<c:forEach items="${feature.medias}" var="image">
		   	<a href="${image.redirectUrl}" target="_blank">
		  	<img src="${image.url}">
		   	</a>
			</c:forEach>
	</div>
	</c:when>
	<c:otherwise>
			<h5>${feature.headline}</h5>
        <div class="row mt-5">
            <c:forEach items="${feature.medias}" var="image">
           <div class="col-6 col-md-3 text-center">
                  <img src="${image.url}" />
                  <h6>${image.description}</h6>
           </div>
           </c:forEach>
        </div>
	</c:otherwise>
</c:choose>

