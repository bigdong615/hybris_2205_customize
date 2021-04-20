<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.CartData" %>
<%@ attribute name="entry" required="true" type="de.hybris.platform.commercefacades.order.data.OrderEntryData" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:url value="${entry.product.url}" var="productUrl"/>

<div class="cartProduct">
     <div class="row">
         <div class="col-md-2 text-center">
          <a href="${fn:escapeXml(productUrl)}"><product:productPrimaryImage product="${entry.product}" format="thumbnail"/></a>
         </div>
         <div class="col-md-7 mt-3"><b>${entry.product.name}</b></div>
         <div class="col-md-3 mt-3 text-md-end">
             <b>$XX ${entry.product.price.formattedValue}</b>
              <spring:theme code="text.rental.cart.qty" />
             <select class="mt-3">
               <c:forEach var="item" begin="1" end="10">
                   <option value="${item}" ${item == entry.quantity ? 'selected="selected"' : ''}>${item}</option>
               </c:forEach>
             </select>
         </div>
     </div>

     <%-- This section will be covered in BL-462 --%>
     <div id="damageOptions" class="row mt-3">
         <div class="col-md-10 offset-md-2">
             <p class="body14 mb-1"><spring:theme code="text.cart.damage.waiver"/><a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></p>
             <div class="dropdown">
               <a class="btn btn-block btn-outline dropdown-toggle text-start gearguard-plus" href="#" role="button" id="coverageOptions1" data-bs-toggle="dropdown" aria-expanded="false">
                 GearGuard Plus <span class="float-end">$XX</span>
               </a>
               <ul class="dropdown-menu" aria-labelledby="coverageOptions1">
                 <li><a class="dropdown-item gearguard-plus" href="#"><spring:theme code="text.damage.waiver.option.gear.plus"/> <span class="float-end">$XX</span></a></li>
                 <li><a class="dropdown-item gearguard" href="#"><spring:theme code="text.damage.waiver.option.gear"/> <span class="float-end">$XX</span></a></li>
                 <li><a class="dropdown-item no-gearguard" href="#"><spring:theme code="text.damage.waiver"/></a></li>
               </ul>
             </div>
         </div>
     </div>
     <%-- It will be handled in BL-463 --%>
     <%--<div id="productOptions" class="row mt-3">
         <div class="col-md-10 offset-md-2">
             <p class="body14 mb-1"><spring:theme code="text.cart.options"/></p>
             <div class="dropdown">
               <a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="coverageOptions1" data-bs-toggle="dropdown" aria-expanded="false">
                 <img src="https://d2ieyhi8galmxj.cloudfront.net/product/MDe5966894-1fa0-4dbb-89be-30c74da8f3a5.jpg">EF/EF-S Lens to EOS R Camera Adapter <span class="float-end">$25</span>
               </a>
               <ul class="dropdown-menu" aria-labelledby="coverageOptions1">
                 <li><a class="dropdown-item" href="#"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MDe5966894-1fa0-4dbb-89be-30c74da8f3a5.jpg">EF/EF-S Lens to EOS R Camera Adapter <span class="float-end">$25</span></a></li>
                 <li><a class="dropdown-item" href="#"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MDe5966894-1fa0-4dbb-89be-30c74da8f3a5.jpg">EF/EF-S Lens to EOS R Camera Adapter <span class="float-end">$25</span></a></li>
                 <li><a class="dropdown-item" href="#"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MDe5966894-1fa0-4dbb-89be-30c74da8f3a5.jpg">EF/EF-S Lens to EOS R Camera Adapter <span class="float-end">$25</span></a></li>
               </ul>
             </div>
         </div>
     </div>--%>

     <%--<div class="productNotifications row">
         <div class="col-12">
             <div class="notification notification-warning">This is a product warning.</div>
             <div class="notification notification-error">This item is no longer available for your selected date range. Change your dates or select a comparable item.</div>
         </div>
     </div>--%>
</div>