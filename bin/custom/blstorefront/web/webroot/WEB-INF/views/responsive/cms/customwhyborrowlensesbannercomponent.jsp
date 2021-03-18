<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>

            <div class="row m-0">
                <div class="col-md-6 p-0">
                    <div id="testimonials-slider" class="splide">
                        <div class="splide__track">
                            <ul class="splide__list">
                           <c:forEach items="${feature.banners}" var="banner" varStatus="status">
                                <li class="splide__slide" style="background-image: url(${banner.media.url});">
                                   <!--  <div class="card">
                                        <img src="assets/img-test-profile.jpg">
                                        <div class="my-auto">
                                            <p class="quote">“I’ve been shooting for 10 years and love to collaborate.”</p>
                                            <p class="credit"><b>Ted</b>&ensp;Customer Support</p>
                                        </div>    
                                    </div> -->
                                </li>
                    			</c:forEach>
                            </ul>
                        </div>
                    </div>
                </div>
                <div class="heavy-padding col-md-6 my-auto">
                    <h4>${feature.title}</h4>
                    <p>${feature.description}</p>
                    <p><a href="#" class="btn btn-primary">${feature.buttonText}</a></p>
                </div>
            </div>
          
          
          
            
         
 