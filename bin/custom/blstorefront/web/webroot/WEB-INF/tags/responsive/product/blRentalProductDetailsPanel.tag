<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib  prefix = "fn" uri = "http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="sec"	uri="http://www.springframework.org/security/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>

<c:url value="/cart/add" var="addToCartUrl"/>
 <div class="screen"></div>
 <cms:pageSlot position="SearchBoxBl" var="component">
 				<cms:component component="${component}"/>
 </cms:pageSlot>
    <section id="theProduct">
        <div class="page-loader-new-layout">
          <img src="${themeResourcePath}/assets/bl-loader.gif" alt="Loading.." title="Loading.." id="new_loading_Img"/>
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
                              <div class="hide-on-desktop" id="productInfo">
                            <c:forEach items="${product.categories}" var="categoryData">
                             <c:if test="${fn :toLowerCase(product.manufacturer) eq fn:toLowerCase(categoryData.code)}">
                              <c:url var="brandUrl" value="${categoryData.url}"/>
                              <p class="overline"><a href="${brandUrl}">${fn:toUpperCase(product.manufacturer)}</a></p>
                             </c:if>
                              </c:forEach>
                                <h1 class="mb-4">${product.displayName}</h1>
                                <c:choose>
                                
                                    <c:when test="${not empty disableButton and disableButton == true}">
                                      	<span class="badge badge-out-of-stock"><spring:theme code="text.product.tile.flag.outOfStock"/></span>
                                      </c:when>
                                      <c:when test="${product.stock.stockLevelStatus.code eq 'lowStock' && product.isBundle ne true}">
                                        <span class="badge badge-limited-stock"><spring:theme code="text.product.tile.flag.only.left" arguments="${product.stock.stockLevel}"/></span>
                                      </c:when>
                                      <c:when test="${product.stock.stockLevelStatus.code eq 'outOfStock'}">
                                      	<span class="badge badge-out-of-stock"><spring:theme code="text.product.tile.flag.outOfStock" arguments="${product.stock.stockLevel}"/></span>
                                      </c:when>
                                      <c:when test="${empty nextAvailabilityDate }">
                                      	<span class="badge badge-out-of-stock"><spring:theme code="text.product.tile.flag.outOfStock" arguments="${product.stock.stockLevel}"/></span>
                                      </c:when>
                                      <c:otherwise>
                                         <c:if test ="${product.productTagValues ne null}">
                                           <span class="badge badge-new">${product.productTagValues}</span>
                                         </c:if>
                                      </c:otherwise>
                                    </c:choose>
                                <div id="pr-reviewsnippet"></div> 
                                </div>
                                <product:productImagePanel galleryImages="${galleryImages}" />
                              </div>
                            <div id="productInfo" class="col-lg-5 offset-lg-1">
                            <div class="hide-on-mobile">
                            <c:forEach items="${product.categories}" var="categoryData">
                             <c:if test="${fn :toLowerCase(product.manufacturer) eq fn:toLowerCase(categoryData.code)}">
                              <c:url var="brandUrl" value="${categoryData.url}"/>
                              <p class="overline"><a href="${brandUrl}">${fn:toUpperCase(product.manufacturer)}</a></p>
                             </c:if>
                              </c:forEach>
                                <h1 class="mb-4">${product.displayName}</h1>
                                <c:choose>
                                    <c:when test="${not empty disableButton and disableButton == true}">
                                      	<span class="badge badge-out-of-stock"><spring:theme code="text.product.tile.flag.outOfStock"/></span>
                                      </c:when>
                                      <c:when test="${product.stock.stockLevelStatus.code eq 'lowStock' && product.isBundle ne true}">
                                        <span class="badge badge-limited-stock"><spring:theme code="text.product.tile.flag.only.left" arguments="${product.stock.stockLevel}"/></span>
                                      </c:when>
                                      <c:when test="${product.stock.stockLevelStatus.code eq 'outOfStock'}">
                                      	<span class="badge badge-out-of-stock"><spring:theme code="text.product.tile.flag.outOfStock" arguments="${product.stock.stockLevel}"/></span>
                                      </c:when>
                                       <c:when test="${empty nextAvailabilityDate }">
                                      	<span class="badge badge-out-of-stock"><spring:theme code="text.product.tile.flag.outOfStock" arguments="${product.stock.stockLevel}"/></span>
                                      </c:when>
                                      <c:otherwise>
                                         <c:if test ="${product.productTagValues ne null}">
                                           <span class="badge badge-new">${product.productTagValues}</span>
                                         </c:if>
                                      </c:otherwise>
                                    </c:choose>                                 
                                <div id="pr-reviewsnippet"></div> 
                                </div>                                   

                                 <ul class="checklist mt-4">
                                 ${product.shortDescription}
                                </ul>
                                <c:choose >
                                 <c:when test="${product.isDiscontinued eq 'true'}">
                                 <div class="notification notification-error mt-4">
                                     <c:choose>
                                            <c:when test="${not empty product.alternativeProduct}">
                                            ${ycommerce:sanitizeHTML(product.alternativeProduct)}
                                             </c:when>
                                            <c:otherwise>
                                            <spring:theme code="pdp.product.discontinue.text"/>
                                            </c:otherwise>
                                     </c:choose>
                                 </div>
                                 </c:when>
                                <c:when test="${product.isUpcoming eq 'true' && !product.isBundle}">
                                <div id="pickupDelivery">
                                  <p><span class="arrival"><spring:theme code="pdp.rental.comming.soon.text"/></span></p>
                                </div>
                                </c:when>

                                <c:otherwise>
								<c:if test="${product.isBundle}">
									<spring:theme code="text.bundle.pdp.include" />
									<ul>
										<c:forEach items="${product.bundleProductReference}"
											var="bundleReference">
											<li>${bundleReference.productReferenceName}<br /></li>
										</c:forEach>
									</ul>
								</c:if>
								<div id="productDates">
                                      <div class="input-group">
                                        <span class="rental-dates d-md-inline"><i class="icon-calendar"></i> <spring:theme code="pdp.rental.dates.label.text" /></span>
                                        <input type="text" id="product-litepicker" class="form-control d-none d-md-inline-block" placeholder="Select dates...">
                                        <input type="text" id="mobile-product-litepicker" class="form-control d-inline-block d-md-none" placeholder="Dates...">
                                       </div>
                                 </div>
                                 <div id="pickupDelivery" style=" float: right; width: 100%; ">
                                 <p>
	                                 <c:choose>
	                                 	<c:when test="${product.stock.stockLevelStatus.code eq 'outOfStock' and not empty nextAvailabilityDate}">
	                                 		<span class="arrival  nextAvailDate"><spring:theme code="rental.pdp.next.available" arguments="${nextAvailabilityDate}" /></span>
	                                 	</c:when>
	                                 	<c:when test="${not empty nextAvailabilityDate and not empty disableButton and disableButton == true}">
	                                 		<span class="arrival  nextAvailDate"><spring:theme code="rental.pdp.next.available" arguments="${nextAvailabilityDate}" /></span>
	                                 	</c:when>
	                                 	<c:when test="${not empty nextAvailabilityDate }">
	                                 		<span class="arrival"><spring:theme code="rental.pdp.get.it.on" arguments="${nextAvailabilityDate}" /></span>
	                                 	</c:when>
	                                 	<c:when test="${empty nextAvailabilityDate }">
	                                 		<span class="arrival nextAvailDate"><spring:theme code="Not Available on Selected Dates" arguments="${nextAvailabilityDate}" /></span>
	                                 	</c:when>
	                                 </c:choose>
	                                 <a href="#" class="pickupDeliveryLink" data-bs-toggle="modal" data-bs-target="#pickup-delivery-options"><spring:theme code="pdp.pickup.section.text"/></a>
                                 </p>
                                  </div>
                                  </c:otherwise>
                                   </c:choose>
                                   <c:if test = "${product.isDiscontinued ne 'true'}">
                                <div class="priceSummary" style="width: 100%;">
                                <!-- BL-483 : Getting price as per the selection on rental days or else default price for seven rentals days will be returned -->
                                  <span class="productPrice"><product:productListerItemPrice product="${product}"/></span>&emsp;<span class="rentalDates">${rentalDate.numberOfDays}&nbsp;<spring:theme code="pdp.rental.product.recommendation.section.day.rental.text"/></span>
                                </div>
                                </c:if>
                                <!--BL-628: Notify Me-->
                                <c:choose >
                                 <c:when test="${product.isDiscontinued eq 'true'}">
                                 </c:when>
                                <c:when test="${product.isUpcoming eq 'true' && !product.isBundle}">
                                <spring:theme code="text.stock.notification.subscribe.title" var="colorBoxTitle" />
                                <sec:authorize access="hasAnyRole('ROLE_ANONYMOUS')">
                                   <a href="#" class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 js-login-popup"   data-link="<c:url value='/login/loginpopup'/>"
                                      data-bs-toggle="modal"  data-bs-target="#signIn">
                                      <spring:theme code="text.get.notified" />
                                   </a>
                                </sec:authorize>
                                <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
                                   <c:choose>
                                      <c:when test="${isWatching}">
                                         <a href="#" class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 removeInterestbtn"  data-box-title="${colorBoxTitle}"
                                            data-box-productcode="${product.code}" data-bs-toggle="modal"
                                            data-bs-target="#getNotified"><spring:theme code="text.remove.notified.button.text"/></a>
                                      </c:when>
                                      <c:otherwise>
                                         <a href="#" class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 arrival-notification"  data-box-title="${colorBoxTitle}"
                                               data-box-productcode="${product.code}" data-bs-toggle="modal"
                                               data-bs-target="#getNotified">
                                               <spring:theme code="text.get.notified" />
                                            </a>
                                         </c:otherwise>
                                   </c:choose>
                                </sec:authorize>
                                </c:when>
                                <c:otherwise>
                                   <c:choose>
                                  		<c:when test="${product.stock.stockLevelStatus.code eq 'outOfStock' }">
                                  				<button id="addToCartButton" type="submit"
                                  					  class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 js-add-to-cart js-disable-btn "
                                  					  aria-disabled="true" disabled="disabled">
                                  				  <spring:theme code="basket.add.to.rental.cart.button.text" />
                                  				</button>
                                  		</c:when>
                                  		<c:when test="${product.isDiscontinued eq 'true' }">
                                          <button id="addToCartButton" type="submit"
                                                   class="btn btn-primary btn-block mt-4 mb-0 mb-md-5 js-add-to-cart js-disable-btn "
                                                   aria-disabled="true" disabled="disabled">
                                                 <spring:theme code="basket.add.to.rental.cart.button.text" />
                                      		</button>
                                  		</c:when>
                                      <c:otherwise>
                                      <c:choose>
                                        <c:when test="${allowAddToCart || isRentalCart}">
                                              <div class="modal fade" id="addToCart" tabindex="-1" aria-hidden="true">
                                                   <div class="modal-dialog modal-dialog-centered modal-lg" id="addToCartModalDialog"></div>
                                              </div>
                                        </c:when>
                                        <c:otherwise>
                                              <div class="modal fade" id="addToCart" tabindex="-1" aria-hidden="true">
                                                   <div class="modal-dialog modal-dialog-centered modal-sm" id="addToCartModalDialog"></div>
                                              </div>
                                        </c:otherwise>
                                      </c:choose>
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
                                 <!-- BL-605 :added class below -->
                                  <div id="gear-slider" class="splide mt-4 hide-days">
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
				<product:productPageReviewsTab product="${product}" />
