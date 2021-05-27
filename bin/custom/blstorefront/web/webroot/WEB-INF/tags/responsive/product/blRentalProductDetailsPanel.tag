<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib  prefix = "fn" uri = "http://java.sun.com/jsp/jstl/functions" %>

<c:url value="/cart/add" var="addToCartUrl"/>
 <div class="screen"></div>
 <cms:pageSlot position="SearchBoxBl" var="component">
 				<cms:component component="${component}"/>
 </cms:pageSlot>
    <section id="theProduct">
        <div class="page-loader-new-layout">
          <img src="${themeResourcePath}/assets/bl-loader.gif" alt="Loading.." title="Loading.." id="new_loading_Img">
        </div>
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
                             <c:if test="${fn :toLowerCase(product.manufacturer) eq fn:toLowerCase(categoryData.code)}">
                              <c:url var="brandUrl" value="${categoryData.url}"/>
                              <p class="overline"><a href="${brandUrl}">${fn:toUpperCase(product.manufacturer)}</a></p>
                             </c:if>
                              </c:forEach>
                                <h1 class="mb-4">${product.displayName}</h1>
                                    <c:choose>
                                      <c:when test="${product.stock.stockLevelStatus.code eq 'lowStock'}">
                                        <span class="badge badge-limited-stock"><spring:theme code="text.product.tile.flag.only.left" arguments="${product.stock.stockLevel}"/></span>
                                      </c:when>
                                      <c:when test="${product.stock.stockLevelStatus.code eq 'outOfStock'}">
                                      	<span class="badge badge-out-of-stock"><spring:theme code="text.product.tile.flag.outOfStock" arguments="${product.stock.stockLevel}"/></span>
                                      </c:when>
                                      <c:otherwise>
                                         <c:if test ="${product.productTagValues ne null}">
                                           <span class="badge badge-new">${product.productTagValues}</span>
                                         </c:if>
                                      </c:otherwise>
                                    </c:choose>
                                <div class="stars"><span class="stars-filled" style="width: 80%;"></span><img src="${themeResourcePath}/assets/stars-empty.svg"></div> <span class="review-count">(138)</span>
                                 <ul class="checklist mt-4">
                                 ${product.shortDescription}
                                </ul>
                                 <div id="productDates">
                                      <div class="input-group">
                                        <span class="rental-dates d-md-inline"><i class="icon-calendar"></i> <spring:theme code="pdp.rental.dates.label.text" /></span>
                                        <input type="text" id="product-litepicker" class="form-control d-none d-md-inline-block" placeholder="Select dates...">
                                        <input type="text" id="mobile-product-litepicker" class="form-control d-inline-block d-md-none" placeholder="Dates...">
                                       </div>
                                 </div>
                                 <div id="pickupDelivery">
                                   <p><span class="arrival">Get it on Jan 31</span> <a href="#" class="pickupDeliveryLink"><spring:theme code="pdp.pickup.section.text"/></a></p>
                                  </div>
                                <div class="priceSummary">
                                <!-- BL-483 : Getting price as per the selection on rental days or else default price for seven rentals days will be returned -->
                                  <span class="productPrice"><product:productListerItemPrice product="${product}"/></span>&emsp;<span class="rentalDates">${rentalDate.numberOfDays}&nbsp;<spring:theme code="pdp.rental.product.recommendation.section.days.rental.text"/></span>
                                </div>
                                <c:choose >
                                <c:when test="${product.isUpcoming eq 'true'}">
                                <a href="#" class="btn btn-primary btn-block mt-4 mb-0 mb-md-5" data-bs-toggle="modal"
                                 data-bs-target="#notifyMeModal"><spring:theme code="text.notify.me" /></a>
                                </c:when>
                                <c:otherwise>
                                   <c:choose>
                                  		<c:when test="${product.stock.stockLevelStatus.code eq 'outOfStock' }">
                                  				<button id="addToCartButton" type="submit"
                                  					  class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 js-add-to-cart js-disable-btn"
                                  					  aria-disabled="true" disabled="disabled">
                                  				  <spring:theme code="basket.add.to.rental.cart.button.text" />
                                  				</button>
                                  		</c:when>
                                  		<c:when test="${product.isDiscontinued eq 'true' }">
                                          <button id="addToCartButton" type="submit"
                                                   class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 js-add-to-cart js-disable-btn"
                                                   aria-disabled="true" disabled="disabled">
                                                 <spring:theme code="basket.add.to.rental.cart.button.text" />
                                      		</button>
                                  		</c:when>
                                      <c:otherwise>
                                        <div class="modal fade" id="addToCart" tabindex="-1" aria-hidden="true">
                                           <div class="modal-dialog modal-dialog-centered modal-lg" id="addToCartModalDialog"></div>
                                        </div>
                                        <form class="add_to_cart_form" action="${addToCartUrl}" method="post">
                                          <button type="button"
                                  			    	class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 js-add-to-cart" data-bs-toggle="modal" data-bs-target="#addToCart" data-product-code="${product.code}">
                                  					<spring:theme code="basket.add.to.rental.cart.button.text" />
                                  				</button>
                                  		  </form>
                                  		</c:otherwise>
                                  </c:choose>

                           		</c:otherwise>
                            </c:choose>
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
                                <!-- Product overview -->
                                <a class="filter-expand" data-bs-toggle="collapse" href="#overview" role="button" aria-expanded="true" aria-controls="overview">
                                <h5><spring:theme code= "pdp.overview.section.text"/></h5>
                                </a>
                                <div class="collapse show" id="overview">
                                     <p>${ycommerce:sanitizeHTML(product.description)}</p>
                                      <product:productVideo  productVideos= "${product.rentalVideosLink}"/>
                                </div>
                                <hr>
                                <!--Product specification -->
                                 <product:specification />
                                 <!-- Rental Include -->
                                 <product:rentalInclude />
                                 <!-- Rental Notes -->
                                 <product:rentalNotes />
                                 <!-- product resource  -->
                                 <product:resource />
                                  <hr>
                                <!-- Additional Gear Slider -->
                                 <h5><spring:theme code= "pdp.rental.product.recommendation.section.text" /></h5>
                                  <div id="gear-slider" class="splide mt-4">
                                     <cms:pageSlot position="CrossSelling" var="comp" element="div" class="productDetailsPageSectionCrossSelling">
                                         <cms:component component="${comp}" element="div" class="productDetailsPageSectionCrossSelling-component"/>
                                     </cms:pageSlot>
                                  </div>
                                <!-- Start Reviews -->
                               <product:blProductReview />
                            </div>
                        </div>
                    </div>
                </section>