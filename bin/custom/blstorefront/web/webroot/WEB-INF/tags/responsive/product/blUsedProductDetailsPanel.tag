<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

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
                    <div class="col-12 col-lg-11 col-xl-10">
                        <div class="row">
                            <div id="productImage" class="col-lg-5 text-center">
                                 <product:productImagePanel galleryImages="${galleryImages}" />
                            </div>
                            <div id="productInfo" class="col-lg-6 offset-lg-1">
                               <c:forEach items="${product.categories}" var="categoryData">
                               <c:if test="${product.manufacturer eq categoryData.code}">
                                <c:url var="brandUrl" value="/used/category/${categoryData.code}"/>
                                  <p class="overline"><a href="${brandUrl}">${categoryData.name}</a></p>
                                  </c:if>
                                 </c:forEach>
                                <h1 class="mb-4">${product.displayName}</h1>
                                <table id="usedProductList">
                                    <thead>
                                        <tr>
                                            <th><spring:theme code="pdp.serial.table.rating.text"/></th>
                                            <th class="d-none d-md-table-cell"><spring:theme code="pdp.serial.table.retail.price.text"/></th>
                                            <th><spring:theme code="pdp.serial.table.price.text"/></th>
                                            <th class="d-none d-md-table-cell"><spring:theme code="pdp.serial.table.number.text"/></th>
                                            <th></th>
                                        </tr>
                                    </thead>
                                  <tbody>
                                     <c:forEach items="${product.serialproducts}" var= "serialProduct"  varStatus="loop">
                                         <tr class= " ${loop.index > 2 ? 'hide-product-row' : ''}">
                                            <td><a href="#" data-bs-toggle="modal" data-bs-target="#sku52678">${serialProduct.conditionRating}</a></td>
                                            <td class="d-none d-md-table-cell"><strike>$1,900</strike></td>
                                            <td>$1,550</td>
                                            <td class="d-none d-md-table-cell"># ${serialProduct.serialId}</td>
                                            <td class="text-end">
                                            <a href="#" class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#addToCart"><spring:theme code="basket.add.to.basket"/></a></td>
                                        </tr>
                                       </c:forEach>
                                    </tbody>
                                </table>
                                <c:if test="${product.serialproducts.size() >2}">
                                <p class="mt-4"><a href="#" id="showmore"><spring:theme code="pdp.show.more.button.text"/></a>
                                </c:if>
                                <c:if test="${product.forRent}">
                                <c:url var="rentUrl" value="/rent/product/${product.code}"/>
                                <a href="${rentUrl}" class="btn btn-sm btn-secondary float-end"><spring:theme code="pdp.product.rent.instead.button.text"/></a></p>
                                </c:if>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </section>

        <section id="theUsedProcess">
               <div class="container">
                   <div class="row justify-content-center">
                       <div class="col-lg-11 col-xl-9">
                           <h5>Buying used gear is easy</h5>
                           <div class="row mt-5">
                               <div class="col-6 col-md-3 text-center">
                                   <h6>Inspected & cleaned by our experts</h6>
                               </div>
                               <div class="col-6 col-md-3 text-center">
                                   <h6>We guarantee your gear will work perfectly</h6>
                               </div>
                               <div class="col-6 col-md-3 text-center">
                                   <h6>Know what to expect with condition details</h6>
                               </div>
                               <div class="col-6 col-md-3 text-center">
                                   <h6>Try it yourself with easy returns</h6>
                               </div>
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
                                <h5><spring:theme code= "pdp.overview.section.text"/></h5></a>
                               <div class="collapse show" id="overview">
                                   <p>${ycommerce:sanitizeHTML(product.usedDescription)}</p>
                                   <c:if test="${not empty product.usedGearVideosLink}">
                                   <div id="overview-slider" class="splide mt-5">
                                       <div class="splide__track">
                                           <ul class="splide__list">
                                              <c:forEach items="${product.usedGearVideosLink}" var="productVideo"  varStatus="count">
                                                 <li class="splide__slide">
                                                    <div class="embed-responsive embed-responsive-16by9">
                                                            <iframe class="embed-responsive-item" src="${productVideo.videoUrl}" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen=""></iframe>
                                                    </div>
                                                  <p class="text-start mt-1">${productVideo.videoName}<span class="gray80 float-end">${productVideo.videoDuration}</span></p>
                                                </li>
                                              </c:forEach>
                                           </ul>
                                       </div>
                                   </div>
                                   </c:if>
                               </div>
                               <hr>
                               <a class="filter-expand" data-bs-toggle="collapse" href="#specs" role="button" aria-expanded="false" aria-controls="specs">
                               <h5><spring:theme code = "pdp.specification.section.text"/></h5></a>
                               <div class="collapse" id="specs">
                                    <product:productDetailsClassifications product="${product}" />
                               </div>
                               <hr>
                               <a class="filter-expand" data-bs-toggle="collapse" href="#rental-includes" role="button" aria-expanded="false" aria-controls="includes">
                               <h5><spring:theme code="pdp.product.includes.section.text"/></h5></a>
                               <div class="collapse" id="rental-includes">
                                   <p>${ycommerce:sanitizeHTML(product.usedIncludes)}</p>
                               </div>
                               <hr>
                               <a class="filter-expand" data-bs-toggle="collapse" href="#rental-notes" role="button" aria-expanded="false" aria-controls="notes">
                               <h5><spring:theme code= "pdp.product.notes.section.text" /></h5></a>
                               <div class="collapse" id="rental-notes">
                                   <p><p>${ycommerce:sanitizeHTML(product.rentalNote)}</p></p>
                               </div>
                               <hr>
                               <a class="filter-expand" data-bs-toggle="collapse" href="#resources" role="button" aria-expanded="false" aria-controls="notes">
                               <h5><spring:theme code= "pdp.resources.section.text" /></h5></a>
                               <div class="collapse" id="resources">
                                  <product:productResourcesPanel />
                               </div>
                               <hr>

<div id="gear-slider" class="splide mt-4">
</div>
                               <!-- Start Reviews -->
                               <div id="reviews" class="mb-5">
                                   <h5 class="mb-4">Reviews</h5><div class="stars"><span class="stars-filled" style="width: 80%;"></span><img src="assets/stars-empty.svg"></div> <span class="review-count">(138)</span>
                                   <div class="reviewBlock">
                                       <div class="reviewTitle">
                                           <div class="stars"><span class="stars-filled" style="width: 100%;"></span><img src="assets/stars-empty.svg"></div> <b>A superb camera with a remarkable kit lens.</b>
                                           <p class="my-4">I received this camera just over a week ago and have shot about 1000 pictures in various conditions with the kit lens and an RF f/4 24 to 105 mm L series lens I got with the Canon R camera I also own. I mostly shoot JPG and use RAW for those difficult pictures that in camera processing may not produce the best results. The R6 was purchased to replace the R.</p>
                                           <p><b>– Kate</b></p>
                                       </div>
                                   </div>
                                   <div class="reviewBlock">
                                       <div class="reviewTitle">
                                           <div class="stars"><span class="stars-filled" style="width: 100%;"></span><img src="assets/stars-empty.svg"></div> <b>A superb camera with a remarkable kit lens.</b>
                                           <p class="my-4">I received this camera just over a week ago and have shot about 1000 pictures in various conditions with the kit lens and an RF f/4 24 to 105 mm L series lens I got with the Canon R camera I also own. I mostly shoot JPG and use RAW for those difficult pictures that in camera processing may not produce the best results. The R6 was purchased to replace the R.</p>
                                           <p><b>– Kate</b></p>
                                       </div>
                                   </div>
                                   <div class="reviewActions">
                                       <a href="#" class="btn btn-outline"><img src="assets/icon-filter-open.svg">&emsp;Write Review</a> <a href="#" class="btn btn-outline">More Reviews</a>
                                   </div>
                               </div>
                           </div>
                       </div>
                   </div>
               </section>
