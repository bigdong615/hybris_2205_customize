<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>

<c:url value="${feature.urlLink}" var="urlLink" />
<li class="splide__slide">
	<div class="embed-responsive embed-responsive-16by9">
	<a href="${urlLink}">	<c:choose>
			<c:when test="${fn:containsIgnoreCase(feature.media.mime,'image/jpeg')}">
				<img class="embed-responsive-item hero-image-slider" alt="Banner Image" src="${feature.media.url}"/>
			</c:when>
			<c:otherwise>
				<video class="embed-responsive-item hero-video-slider" defaultMuted playsinline autoplay
				muted loop>
					<source src="${feature.media.url}" type="video/mp4">
						Your browser does not support the video tag.
				</video>
			</c:otherwise>
		</c:choose>
		
		<div class="row h-100 justify-content-center">
			<div class="col-md-10 col-lg-9 my-auto">
				<div class="hero-content white text-start">
					<p class="overline">${feature.rentProData}</p>
					<h1>${feature.borrowData}</h1>
				</div>
			</div>
		</div>
	</a>
	</div>
</li>
