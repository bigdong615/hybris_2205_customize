<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.CartData" %>
<%@ attribute name="entry" required="true" type="de.hybris.platform.commercefacades.order.data.OrderEntryData" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:url value="/rent/product/${entry.product.code}" var="productUrl"/>
<c:url value="/cart/update" var="cartUpdateFormAction" />

<div class="cartProduct">
     <div class="row">
         <div class="col-md-2 text-center">
          <a href="${fn:escapeXml(productUrl)}"><product:productPrimaryImage product="${entry.product}" format="thumbnail"/></a>
         </div>
         <div class="col-md-7 mt-3">
           <b>${entry.product.name}</b>
           <form:form id="removeCartForm${entry.entryNumber}" action="${cartUpdateFormAction}" method="post"
                      modelAttribute="updateQuantityForm${entry.entryNumber}" class="js-qty-form${entry.entryNumber}">
               <input type="hidden" name="entryNumber" value="${entry.entryNumber}" />
               <input type="hidden" name="productCode" value="${entry.product.code}" />
               <input type="hidden" name="initialQuantity" value="${entry.quantity}" />
               <input type="hidden" name="quantity" value=0 />
               <a href="" class="shopping-cart__item-remove" id="removeEntry_${entry.entryNumber}"><small>Remove Item</small></a>
           </form:form>
         </div>
         <div class="col-md-3 mt-3 text-md-end">
             <b><format:price priceData="${entry.totalPrice}" displayFreeForZero="true" /></span></b>
             <c:url value="/cart/update" var="cartUpdateFormAction"/>
           <form:form id="updateCartForm${entry.entryNumber}" action="${cartUpdateFormAction}" method="post"
                                            modelAttribute="updateQuantityForm${entry.entryNumber}" class="js-qty-form${entry.entryNumber}">
                 <input type="hidden" name="entryNumber" value="${entry.entryNumber}" />
                 <input type="hidden" name="productCode" value="${entry.product.code}" />
                 <input type="hidden" name="initialQuantity" value="${entry.quantity}" />
                 <input type="hidden" name="quantity" value="${entry.quantity}" />
                <spring:theme code="text.rental.cart.qty" />
             <select class="mt-3 select js-select js-component-init update" id="shopping-cart-qty_${entry.entryNumber}" name="shopping-cart-qty">
               <c:forEach var="item" begin="1" end="10">
                    <c:choose>
                        <c:when test="${entry.quantity == item}">
                            <option value="${item}" selected="selected">${item}</option>
                        </c:when>
                           <c:otherwise>
                              <option value="${item}">${item}</option>
                           </c:otherwise>
                    </c:choose>
               </c:forEach>
             </select>
           </form:form>
         </div>
     </div>

     <%-- This section will be covered in BL-462 --%>
     <div id="damageOptions" class="row mt-3">
         <div class="col-md-10 offset-md-2">
             <p class="body14 mb-1"><spring:theme code="text.cart.damage.waiver"/><a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></p>
             <div class="dropdown">
             <c:choose>
             	<c:when test="${entry.gearGuardProFullWaiverSelected }">
             		<a class="btn btn-block btn-outline dropdown-toggle text-start gearguard-plus" href="#" role="button" id="coverageOptions1" data-bs-toggle="dropdown" aria-expanded="false">
                 <spring:theme code="text.damage.waiver.option.gear.plus"/> <span class="float-end"><format:price priceData="${entry.gearGuardProFullWaiverPrice}"/></span>
               </a>
             	</c:when>
             	<c:when test="${entry.gearGuardWaiverSelected }">
             		<a class="btn btn-block btn-outline dropdown-toggle text-start gearguard" href="#" role="button" id="coverageOptions1" data-bs-toggle="dropdown" aria-expanded="false">
                 <spring:theme code="text.damage.waiver.option.gear"/> <span class="float-end"><format:price priceData="${entry.gearGuardWaiverPrice}"/></span>
               </a>
             	</c:when>
             	<c:otherwise>
             		<a class="btn btn-block btn-outline dropdown-toggle text-start no-gearguard" href="#" role="button" id="coverageOptions1" data-bs-toggle="dropdown" aria-expanded="false">
                 <spring:theme code="text.damage.waiver"/>
                 </a>
             	</c:otherwise>
             </c:choose>
               
               <ul class="dropdown-menu damage-wavier damage-Waiver-update" aria-labelledby="coverageOptions1">
                 <li><a class="dropdown-item gearguard-plus" href="#" data-entry="${entry.entryNumber}" data-id="gearguardpro">
                 <spring:theme code="text.damage.waiver.option.gear.plus"/> <span class="float-end"><format:price priceData="${entry.gearGuardProFullWaiverPrice}"/></span></a></li>
                 <li><a class="dropdown-item gearguard" href="#" data-entry="${entry.entryNumber}" data-id="gearguard">
                 <spring:theme code="text.damage.waiver.option.gear"/> <span class="float-end"><format:price priceData="${entry.gearGuardWaiverPrice}"/></span></a></li>
                 <li><a class="dropdown-item no-gearguard" href="#" data-entry="${entry.entryNumber}" data-id="nogearguard"><spring:theme code="text.damage.waiver"/></a></li>
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

     <div class="productNotifications row">
         <div class="col-12">
             <%-- This div is commented and can be used for product level warning as per requirement--%>
             <%--<div class="notification notification-warning">This is a product warning.</div>--%>
             <c:if test="${entry.product.stock.stockLevelStatus eq 'outOfStock'}">
              <div class="notification notification-error"><spring:theme code="text.stock.not.available"/></div>
             </c:if>
         </div>
     </div>
</div>