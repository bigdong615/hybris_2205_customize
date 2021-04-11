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
          <%--<img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD5796f428-6173-4219-8a48-683c1afd2b05.jpg">--%>
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
     <div id="damageOptions" class="row mt-3">
         <div class="col-md-10 offset-md-2">
             <p class="body14 mb-1">Damage Waiver <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></p>
             <div class="dropdown">
               <a class="btn btn-block btn-outline dropdown-toggle text-start gearguard-plus" href="#" role="button" id="coverageOptions1" data-bs-toggle="dropdown" aria-expanded="false">
                 GearGuard Plus <span class="float-end">$25</span>
               </a>
               <ul class="dropdown-menu" aria-labelledby="coverageOptions1">
                 <li><a class="dropdown-item gearguard-plus" href="#">GearGuard Plus <span class="float-end">$25</span></a></li>
                 <li><a class="dropdown-item gearguard" href="#">GearGuard <span class="float-end">$15</span></a></li>
                 <li><a class="dropdown-item no-gearguard" href="#">No Damage Waiver</a></li>
               </ul>
             </div>
         </div>
     </div>
     <div id="productOptions" class="row mt-3">
         <div class="col-md-10 offset-md-2">
             <p class="body14 mb-1">Options</p>
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
     </div>
     <div class="productNotifications row">
         <div class="col-12">
             <div class="notification notification-warning">This is a product warning.</div>
             <div class="notification notification-error">This item is no longer available for your selected date range. Change your dates or select a comparable item.</div>
         </div>
     </div>
</div>