<%@ page trimDirectiveWhitespaces="true"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="component" tagdir="/WEB-INF/tags/shared/component"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:choose>
	<c:when test="${not empty productReferences and component.maximumNumberProducts > 0}">
    		<div class="splide__track">
                  <ul class="splide__list">
                                <c:forEach end="${component.maximumNumberProducts}" items="${productReferences}" var="productReference">
            				        	<li class="splide__slide">
                                            <div class="card">
                                            <c:if test ="${productReference.target.productTagValues ne null}">
                                            <span class="badge badge-new">${productReference.target.productTagValues}</span>
                                            </c:if>
                                               <span class="bookmark"></span>
                                                   <div class="card-slider splide">
                                                     <div class="splide__track">
                                                       <ul class="splide__list">
                                                       <c:forEach items="${productReference.target.images}" var="productImagePdp">
                                                          <c:if test="${productImagePdp.format eq 'product' and productImagePdp.imageType eq 'GALLERY'}">
                                                                <c:url value="${productImagePdp.url}" var="primaryImagePdpUrl" />
                                                                <c:set value="this is alternate" var="altTextHtml"/>
                                                                     <li class="splide__slide"><img src="${primaryImagePdpUrl}"></li>
                                                          </c:if>
                                                       </c:forEach>
                                                       </ul>
                                                     </div>
                                                   </div>
                                                   <p class="overline"><a href="#">${fn:escapeXml(productReference.target.manufacturer)}</a></p>
                                                   <h6 class="product"><a href="#">${fn:escapeXml(productReference.target.name)}</a></h6>
                                                   <!-- BL-483 : Getting price as per the selection on rental days or else default price for seven rentals days will be returned -->
                                                   <h6 class="price"><product:productListerItemPrice product="${productReference.target}"/> <span class="period">${datedata.selectedFromDate} - ${datedata.selectedToDate}</span></h6>
                                                    <c:choose>
                                                          <c:when test="${productReference.target.isDiscontinued}">
                                                                <a href="#" class="btn btn-outline" disabled="disabled"><spring:theme code="pdp.rental.product.recommendation.section.addtorental.text" /></a>
                                                          </c:when>
                                                          <c:when test="${productReference.target.isUpcoming}">
                                                                <a href="#" class="btn btn-primary"><spring:theme code="pdp.rental.product.recommendation.section.notifyme.text" /></a>
                                                          </c:when>
                                                           <c:otherwise>
                                                                <a href="#" class="btn btn-primary"><spring:theme code="pdp.rental.product.recommendation.section.addtorental.text" /></a>
                                                            </c:otherwise>
                                                    </c:choose>
                                            </div>
                                        </li>
                                </c:forEach>
                  </ul>
            </div>
    	</c:when>

	<c:otherwise>
		<component:emptyComponent />
	</c:otherwise>
</c:choose>