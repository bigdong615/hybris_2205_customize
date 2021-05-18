<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ attribute name="productVideos" required="true" type="java.util.List" %>

<div id="overview-slider" class="splide mt-5">
    <c:if test="${not empty productVideos}">
       <div class="splide__track">
           <ul class="splide__list">
              <c:forEach items="${productVideos}" var="productVideo"  varStatus="count">
                 <li class="splide__slide">
                    <div class="embed-responsive embed-responsive-16by9">
                            <iframe class="embed-responsive-item" src="${productVideo.videoUrl}" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen=""></iframe>
                    </div>
                    <!-- BL-556 : removing video name and duration. -->
                  <!-- <p class="text-start mt-1">${productVideo.videoName}<span class="gray80 float-end">${productVideo.videoDuration}</span></p> -->
                </li>
              </c:forEach>
           </ul>
       </div></c:if>
</div>
