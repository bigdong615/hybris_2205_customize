<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<c:url value="/cart/add" var="addToCartUrl"/>
 <div class="screen"></div>
 <cms:pageSlot position="SearchBoxBl" var="component">
 				<cms:component component="${component}"/>
 </cms:pageSlot>
    <section id="theProduct">
            <div class="container">
                <div class="row justify-content-center">
                    <div class="col-12">
                       <cms:pageSlot position="BreadcrumbSection" var="feature">
                			   <cms:component component="${feature}" />
                		   </cms:pageSlot>
                    </div>
                </div>
                <div class="row justify-content-center">
                    <div class="col-12 col-lg-10 col-xl-9">
                        <div class="row">
                              <div id="productImage" class="col-lg-6 text-center">
                                <product:productImagePanel galleryImages="${galleryImages}" />
                              </div>
                            <div id="productInfo" class="col-lg-5 offset-lg-1">
                            <c:forEach items="${product.categories}" var="categoryData">
                             <c:if test="${product.manufacturer eq categoryData.code}">
                              <c:url var="brandUrl" value="${categoryData.url}"/>
                              <p class="overline"><a href="${brandUrl}">${categoryData.name}</a></p>
                             </c:if>
                                </c:forEach>
                                <h1 class="mb-4">${product.displayName}</h1>
                                <span class="badge badge-limited-stock">Only 2 Left</span> <div class="stars"><span class="stars-filled" style="width: 80%;"></span><img src="assets/stars-empty.svg"></div> <span class="review-count">(138)</span>
                                 <ul class="checklist mt-4">
                                 ${product.shortDescription}
                                </ul>
                                 <div id="productDates">
                                      <div class="input-group">
                                        <span class="rental-dates d-md-inline"><i class="icon-calendar"></i> Rental Dates</span>
                                        <input type="text" id="product-litepicker" class="form-control d-none d-md-inline-block" placeholder="Select dates...">
                                        <input type="text" id="mobile-product-litepicker" class="form-control d-inline-block d-md-none" placeholder="Dates...">
                                       </div>
                                 </div>
                                 <div id="pickupDelivery">
                                   <p><span class="arrival">Get it on Jan 31</span> <a href="#" class="pickupDeliveryLink">Pickup or Delivery</a></p>
                                  </div>
                                <div class="priceSummary">
                                  <span class="productPrice">$215</span>&emsp;<span class="rentalDates">7 day rental</span>
                                </div>
                                <form id="addToCartForm" class="add_to_cart_form" action="${addToCartUrl}" method="post">
                                  <input type="hidden" maxlength="3" size="1" id="qty" name="qty" class="qty js-qty-selector-input" value="1">
                                  <input type="hidden" name="productCodePost" value="${product.code}">
                                  <button id="addToCartButton" type="submit" class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 js-add-to-cart js-enable-btn">
                           				<spring:theme code= "basket.add.to.rental.cart.button.text" /></button>
                           			</form>
                              </div>
                        </div>
                    </div>
                </div>
            </div>
    </section>
    <section id="theProcess">
                <div class="container">
                    <div class="row justify-content-center">
                        <div class="col-lg-11 col-xl-9">
                            <h5>
                             <cms:pageSlot position="HomePageRentingGearIsEasyTitleSlot" var="feature">
                           							<cms:component component="${feature}" />
                           	 </cms:pageSlot>
                            </h5>
                            <div class="row mt-5">
                               <cms:pageSlot position="HomePageRentingGearSectionSlot" var="feature">
                               		<cms:component component="${feature}" />
                             	</cms:pageSlot>
                            </div>
                        </div>
                    </div>
                </div>
    </section>
    <section id="productExtras">
                    <div class="container">
                        <div class="row justify-content-center">
                            <div class="col-lg-11 col-xl-9">
                                <hr>
                                <a class="filter-expand" data-bs-toggle="collapse" href="#overview" role="button" aria-expanded="true" aria-controls="overview">
                                <h5><spring:theme code= "pdp.overview.section.text"/></h5>
                                </a>
                                <div class="collapse show" id="overview">
                                     <p>${ycommerce:sanitizeHTML(product.description)}</p>
                                    <div id="overview-slider" class="splide mt-5">
                                      <c:if test="${not empty product.rentalVideosLink}">
                                           <div class="splide__track">
                                             <ul class="splide__list">
                                               <c:forEach items="${product.rentalVideosLink}" var="productVideo"  varStatus="count">
                                                <li class="splide__slide">
                                                  <div class="embed-responsive embed-responsive-16by9">
                                                      <iframe class="embed-responsive-item" src="${productVideo.videoUrl}" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen=""></iframe>
                                                  </div>
                                                    <p class="text-start mt-1">${productVideo.videoName}<span class="gray80 float-end">${productVideo.videoDuration}</span></p>
                                               </li>
                                                </c:forEach>
                                            </ul>
                                        </div>
                                      </c:if>
                                    </div>
                                 </div>
                                <hr>
                                <a class="filter-expand" data-bs-toggle="collapse" href="#specs" role="button" aria-expanded="false" aria-controls="specs">
                                <h5><spring:theme code = "pdp.specification.section.text"/></h5></a>
                                <div class="collapse" id="specs">
                                <product:productDetailsClassifications product="${product}" />
                                    </div>
                                <hr>
                                <c:if test="${not empty product.rentalIncludes}">
                                <a class="filter-expand" data-bs-toggle="collapse" href="#rental-includes" role="button" aria-expanded="false" aria-controls="includes">
                                <h5><spring:theme code="pdp.rental.includes.section.text"/></h5>
                                </a>
                                <div class="collapse" id="rental-includes">
                                    <p>${ycommerce:sanitizeHTML(product.rentalIncludes)}</p>
                                </div>
                                <hr>
                                </c:if>
                              <c:if test="${not empty product.rentalNote}">
                                <a class="filter-expand" data-bs-toggle="collapse" href="#rental-notes" role="button" aria-expanded="false" aria-controls="notes">
                                <h5><spring:theme code= "pdp.rental.notes.section.text" /></h5></a>
                                <div class="collapse" id="rental-notes">
                                    <p>${ycommerce:sanitizeHTML(product.rentalNote)}</p>
                                </div>
                                 <hr>
                                </c:if>
                                <a class="filter-expand" data-bs-toggle="collapse" href="#resources" role="button" aria-expanded="false" aria-controls="notes">
                                <h5><spring:theme code= "pdp.resources.section.text" /></h5></a>
                                <div class="collapse" id="resources">
                                  <product:productResourcesPanel />
                                   </div>
                                <hr>
                                <!-- Additional Gear Slider -->
                                <!--TO-DO : This section will be fixed in bl-174  -->
                                <product:recommendationsSectionPanel />
                                <ul class="splide__pagination"><li><button class="splide__pagination__page is-active" type="button" aria-current="true" aria-controls="gear-slider-slide01 gear-slider-slide02 gear-slider-slide03 gear-slider-slide04" aria-label="Go to page 1"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="gear-slider-slide03 gear-slider-slide04 gear-slider-slide05 gear-slider-slide06" aria-label="Go to page 2"></button></li></ul></div>
                                <!-- Start Reviews -->
                                <div id="reviews" class="mb-5">
                                    <h5 class="mb-4"><spring:theme code= "pdp.review.section.text"/></h5><div class="stars"><span class="stars-filled" style="width: 80%;"></span><img src="assets/stars-empty.svg"></div> <span class="review-count">(138)</span>
                                    <div class="reviewBlock">
                                        <div class="reviewTitle">
                                            <div class="stars"><span class="stars-filled" style="width: 90%;"></span><img src="assets/stars-empty.svg"></div> <b>A superb camera with a remarkable kit lens.</b>
                                            <p class="my-4">I received this camera just over a week ago and have shot about 1000 pictures in various conditions with the kit lens and an RF f/4 24 to 105 mm L series lens I got with the Canon R camera I also own. I mostly shoot JPG and use RAW for those difficult pictures that in camera processing may not produce the best results. The R6 was purchased to replace the R.</p>
                                            <p><b>–&nbsp;Kate</b></p>
                                        </div>
                                    </div>
                                    <div class="reviewBlock">
                                        <div class="reviewTitle">
                                            <div class="stars"><span class="stars-filled" style="width: 80%;"></span><img src="assets/stars-empty.svg"></div> <b>A superb camera with a remarkable kit lens.</b>
                                            <p class="my-4">I received this camera just over a week ago and have shot about 1000 pictures in various conditions with the kit lens and an RF f/4 24 to 105 mm L series lens I got with the Canon R camera I also own. I mostly shoot JPG and use RAW for those difficult pictures that in camera processing may not produce the best results. The R6 was purchased to replace the R.</p>
                                            <p><b>–&nbsp;Kate</b></p>
                                        </div>
                                    </div>
                                    <div class="reviewActions">
                                        <a href="#" class="btn btn-outline"><img src="assets/icon-filter-open.svg"> Write Review</a> <a href="#" class="btn btn-outline">More Reviews</a>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </section>