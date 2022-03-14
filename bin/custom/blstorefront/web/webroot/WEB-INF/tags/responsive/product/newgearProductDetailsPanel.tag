<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib  prefix = "fn" uri = "http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="sec"	uri="http://www.springframework.org/security/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>


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
                          <div class="hide-on-desktop" id="productInfo">
                          <h1 class="mb-4">${product.displayName}</h1>
                        </div>
                              <div id="productImage" class="col-lg-6 text-center">
                                <product:productImagePanel galleryImages="${galleryImages}" />
                              </div>
                           <div id="productInfo" class="col-lg-5 offset-lg-1">
                              <c:forEach items="${product.categories}" var="categoryData">
                                <c:url var="brandUrl" value="${categoryData.url}"/>
                              </c:forEach>
                             <div class="hide-on-mobile">
                          <h1 class="mb-4">${product.displayName}</h1>
                        </div>
                               <c:choose>
                                  <c:when test="${product.isRetailGearInStock eq true}">
                                    <span class="badge badge-limited-stock"><spring:theme
                                        code="text.product.newgear.flag.inStock"/></span>
                                  </c:when>
                                  <c:otherwise>
                                    <span class="badge badge-out-of-stock"><spring:theme
                                        code="text.product.newgear.flag.outOfStock" /></span>
                                	</c:otherwise>
                               </c:choose>
                              <ul class="checklist mt-4">${product.shortDescription} </ul>
                              <div class="priceSummary">
                                <strong><format:price	priceData="${product.retailGearPrice}" /></strong>
                              </div>
                               <c:choose>
                                 <c:when test="${allowAddToCart || isNewGearCart}">
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
                              <form:form id="addToCartForm${fn:escapeXml(product.code)}" action="${addToCartUrl}" method="post" class="add_to_cart_form">
                                  <button type="button" class="btn btn-primary js-add-to-cart btnwidthplp" data-bs-toggle="modal" data-bs-target="#addToCart"
                                   <c:if test="${product.isRetailGearInStock eq false}"> disabled</c:if> data-product-code="${product.code}">
                                        <spring:theme code="text.add.to.cart" />
                                  </button>
                              </form:form>
                          </div>
                        </div>
                    </div>
                </div>
            </div>
    </section>
