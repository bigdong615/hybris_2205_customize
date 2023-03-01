<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="component" tagdir="/WEB-INF/tags/shared/component"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>

<c:url value="/removewishlistentry" var="removeProduct" />
<c:url value="/cart/add" var="addToCartUrl"/>
<c:url value="/bookmark/addtorental" var="addToCartAndRemoveUrl"/>

<div id="accountContent" class="col-lg-8 offset-lg-1">
                    <h3><spring:theme code="text.account.bookmarkpage"/></h3>
                    <c:choose>
                      	<c:when test="${empty searchPageData.results}">                          
                                              <hr>
                                              <div class="notification no-orders">
                                                  <p><strong><spring:theme code="text.account.bookmarkpage.messageone"/></strong></p>
                                                  <p><spring:theme code="text.account.bookmarkpage.messagetwo"/></p>
                                              </div>
                          
                        </c:when>
           	  <c:otherwise>
                 <c:forEach items="${searchPageData.results}" var="wishlistDatas" varStatus="loopindex">
                    <div class="order-block">
                        <div class="row">
                            <div class="col-3 col-md-2 text-center">
                               <c:set value="true" var="continueLoop"/>
                               <c:forEach items="${wishlistDatas.product.images}" var="productImagePdp">
                                  <c:if test="${productImagePdp.format eq 'product' and productImagePdp.imageType eq 'GALLERY' and continueLoop}">
                                    <c:url value="${productImagePdp.url}" var="primaryImagePdpUrl" />
                                    <c:set value="this is alternate" var="altTextHtml"/>
                                           <a href ="${rentalPDPUrl}"><img src="${primaryImagePdpUrl}">
                                           <c:set value="false" var="continueLoop"/>
                                           </a>
                                  </c:if>
                               </c:forEach>
                            </div>
                            <div class="col-9 col-md-6 my-auto">
                                <c:url var="rentalPDPUrl" value="/rent/product/${wishlistDatas.product.code}"/>
                                <b><a href="${rentalPDPUrl}">${wishlistDatas.product.displayName}</a></b>
                                <p class="gray80 body14">
                                    <c:if test="${rentalDate.selectedFromDate ne null and rentalDate.selectedToDate ne null}">
                                     ${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}
                                    </c:if>
                                    <br>
                                    <format:price priceData="${wishlistDatas.product.price}"/>
                                </p>
                            </div>
                            <div class="col-12 col-md-4 text-start text-md-end my-auto bookmarkpage">
                            <form id="removewishlistForm_${loopindex.index}" action="${removeProduct}" method="post" >
                             <input type="hidden" name="removeProductEntry" value="${wishlistDatas.product.code}" id="removeProductEntry${loopindex.index}" />
                             <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
                             <button type="submit" name="REMOVE" class="body14 lightteal">Remove</button>
                             </form>
                              <form id="addToCartAndRemoveForm_${loopindex.index}" action="${addToCartAndRemoveUrl}" method="post">
                                  <input type="hidden" name="addtocartremoveProductEntry" value="${wishlistDatas.product.code}" id="addtocartremoveProductEntry{loopindex.index}" />
                                   <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
                                
                               </form>
                               <c:choose>
                               	<c:when test="${wishlistDatas.product.stock.stockLevelStatus eq 'outOfStock'}">
                              	<button type="submit" class="btn btn-outline btn-disabled btnwidthplp js-add-to-cart js-disable-btn" disabled="disabled">
                                      <spring:theme code="pdp.rental.product.recommendation.section.addtorental.text" />
                                 </button>
                               	</c:when>
                               	<c:otherwise>
                               	 <button type="submit" class="btn btn-primary bookmark-addToCart" data-id="addToCartAndRemoveForm_${loopindex.index}">
                                      <spring:theme code="pdp.rental.product.recommendation.section.addtorental.text" />
                                 </button>
                               	</c:otherwise>
                               </c:choose>

                            </div>
                        </div>
                    </div>
                 </c:forEach>
                     <nav:pagination searchPageData="${searchPageData}" searchUrl="${searchUrl}"/>
     </c:otherwise>
    </c:choose>
 </div>
  
