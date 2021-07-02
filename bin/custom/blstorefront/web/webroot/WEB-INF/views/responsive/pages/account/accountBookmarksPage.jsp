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
                    <h1><spring:theme code="text.account.bookmarkpage"/></h1>
                 <c:forEach items="${searchPageData.results}" var="wishlistDatas" varStatus="loopindex">
                    <div class="order-block">
                        <div class="row">
                            <div class="col-3 col-md-2 text-center">
                               <c:forEach items="${wishlistDatas.product.images}" var="productImagePdp">
                                  <c:if test="${productImagePdp.format eq 'product' and productImagePdp.imageType eq 'GALLERY'}">
                                    <c:url value="${productImagePdp.url}" var="primaryImagePdpUrl" />
                                    <c:set value="this is alternate" var="altTextHtml"/>
                                           <a href ="${rentalPDPUrl}"><img src="${primaryImagePdpUrl}"></a>
                                  </c:if>
                               </c:forEach>
                            </div>
                            <div class="col-9 col-md-6 my-auto">
                                <b>${wishlistDatas.product.displayName}</b>
                                <p class="gray80 body14">
                                    ${rentalDate.selectedFromDate} - ${rentalDate.selectedToDate}<br>
                                    <format:price priceData="${wishlistDatas.product.price}"/>
                                </p>
                            </div>
                            <div class="col-12 col-md-4 text-start text-md-end my-auto">
                            <form id="removewishlistForm_${loopindex.index}" action="${removeProduct}" method="post" >
                             <input type="hidden" name="removeProductEntry" value="${wishlistDatas.product.code}" id="removeProductEntry{loopindex.index}" />
                             <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
                             <button type="submit" name="REMOVE" class="body14 lightteal">Remove</button>
                             </form>
                              <form id="addToCartAndRemoveForm_${loopindex.index}" action="${addToCartAndRemoveUrl}" method="post">
                                  <input type="hidden" name="addtocartremoveProductEntry" value="${wishlistDatas.product.code}" id="addtocartremoveProductEntry{loopindex.index}" />
                                   <input type="hidden"  name="${CSRFToken.parameterName}"  value="${CSRFToken.token}"/>
                                 <button type="submit" class="btn btn-primary">
                                      <spring:theme code="pdp.rental.product.recommendation.section.addtorental.text" />
                                 </button>
                               </form>

                            </div>
                        </div>
                    </div>
                 </c:forEach>
                     <nav:pagination searchPageData="${searchPageData}" searchUrl="${searchUrl}"/>
 </div>

