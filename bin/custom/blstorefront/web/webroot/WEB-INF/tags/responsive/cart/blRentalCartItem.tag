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
          <a href="${fn:escapeXml(productUrl)}" class="js-pdplinkUrl" data-productCode="${entry.product.code}"
            data-brand="${entry.product.manufacturer}" data-productName="${ycommerce:sanitizeHTML(entry.product.name)}" data-productType="rental">
          <product:productPrimaryImage product="${entry.product}" format="thumbnail"/></a>
         </div>
         <div class="col-md-7 mt-3">
           <a href="${fn:escapeXml(productUrl)}" class="js-pdplinkUrl" data-productCode="${entry.product.code}" data-brand="${entry.product.manufacturer}"
             data-productName="${ycommerce:sanitizeHTML(entry.product.name)}" data-productType="rental"><b>${entry.product.name}</b></a>
             <c:if test="${not empty entry.product.bundleProductReference}">
             <ul class="checklist mt-4">
             <c:forEach items="${entry.product.bundleProductReference}" var="bundleItems">
              <li>  ${bundleItems.productReferenceName}</li>
             </c:forEach>
             </ul>
             </c:if>
           <form:form id="removeCartForm${entry.entryNumber}" action="${cartUpdateFormAction}" method="post"
                      modelAttribute="updateQuantityForm${entry.entryNumber}" class="js-qty-form${entry.entryNumber}">
               <input type="hidden" name="entryNumber" value="${entry.entryNumber}" />
               <input type="hidden" name="productCode" value="${entry.product.code}" />
               <input type="hidden" name="productName" value="${entry.product.name}" />
               <input type="hidden" name="initialQuantity" value="${entry.quantity}" />
               <input type="hidden" name="quantity" value=0 />
               <input type="hidden" name="removeEntry" value="true" />
               <a href="" class="shopping-cart__item-remove" id="removeEntry_${entry.entryNumber}"><small>Remove Item</small></a>
           </form:form>
         </div>
         <div class="col-md-3 mt-3 text-md-end">
             <b>
             <c:choose>
             <c:when test="${isReplacementOrderCart eq true}">
              <format:price priceData="${entry.totalPrice}"/>
             </c:when>
             <c:otherwise>
             <format:price priceData="${entry.totalPrice}" displayFreeForZero="true" />
             </c:otherwise>
             </c:choose>
             </span></b>
             <c:url value="/cart/update" var="cartUpdateFormAction"/>
           <form:form id="updateCartForm${entry.entryNumber}" action="${cartUpdateFormAction}" method="post"
                                            modelAttribute="updateQuantityForm${entry.entryNumber}" class="js-qty-form${entry.entryNumber}">
                 <input type="hidden" name="entryNumber" value="${entry.entryNumber}" />
                 <input type="hidden" name="productCode" value="${entry.product.code}" />
                 <input type="hidden" name="initialQuantity" value="${entry.quantity}" />
                 <input type="hidden" name="quantity" value="${entry.quantity}" />
                 <input type="hidden" name="removeEntry" value="false" />
                <spring:theme code="text.rental.cart.qty" />
                <div class="quantity">
             	    <div class="input-group">
             		    <span class="input-group-btn">
             			  <button type="button" class="btn btn-default btn-number"
             				  data-type="minus" data-field="quant[1]${entry.entryNumber}" entryNumber="${entry.entryNumber}">
             				  <span class="glyphicon glyphicon-minus"></span>
             			  </button>
             		    </span> <input type="text" name="quant[1]${entry.entryNumber}" class="form-control input-number"
             			    value="${entry.quantity}" min="1" max="99" entryNumber="${entry.entryNumber}"> <span class="input-group-btn">
             			    <button type="button" class="btn btn-default btn-number"
             				    data-type="plus" data-field="quant[1]${entry.entryNumber}" entryNumber="${entry.entryNumber}">
             				    <span class="glyphicon glyphicon-plus"></span>
             			    </button>
             		    </span>
             	    </div>
                </div>
           </form:form>
         </div>
     </div>

     <%-- This section will be covered in BL-462 --%>
     <c:if test="${fn:containsIgnoreCase(entry.product.manufacturerAID, '9') == false}">
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
                 <li><a class="dropdown-item gearguard-plus" href="#" data-entry="${entry.entryNumber}" data-id="gearguardpro" data-product-code="${entry.product.code}">
                 <spring:theme code="text.damage.waiver.option.gear.plus"/> <span class="float-end"><format:price priceData="${entry.gearGuardProFullWaiverPrice}"/></span></a></li>
                 <li><a class="dropdown-item gearguard" href="#" data-entry="${entry.entryNumber}" data-id="gearguard" data-product-code="${entry.product.code}">
                 <spring:theme code="text.damage.waiver.option.gear"/> <span class="float-end"><format:price priceData="${entry.gearGuardWaiverPrice}"/></span></a></li>
                 <li><a class="dropdown-item no-gearguard" href="#" data-entry="${entry.entryNumber}" data-id="nogearguard" data-product-code="${entry.product.code}"><spring:theme code="text.damage.waiver"/></a></li>
               </ul>
             </div>
         </div>
     </div>
     </c:if>
     <c:if test="${not empty entry.option}">
	<div id="damageOptions" class="row mt-3">
		<div class="col-md-10 offset-md-2 rental-bl-options">
			<p class="body14 mb-1"><spring:theme code="text.cart.rental.options"/></p>
			<div class="dropdown">
				<a class="btn btn-block btn-outline dropdown-toggle text-start" href="#" role="button" id="coverageOptions1" data-bs-toggle="dropdown" aria-expanded="false">
						<c:choose>
							<c:when test="${not empty entry.selectedProductOptions}">
										                 ${entry.selectedProductOptions.optionName}
              			   <c:choose>
									<c:when test="${isReplacementOrderCart eq true}">
										<span class="float-end"><spring:theme
												code="text.replacement.option.cost" /></span>
									</c:when>
									<c:otherwise>
										<c:if test="${not empty entry.selectedProductOptions.optionPrice}">
											<span class="float-end"><format:price
													priceData="${entry.selectedProductOptions.optionPrice}" /></span>
										</c:if>
									</c:otherwise>
								</c:choose>
							</c:when>
							<c:otherwise>
						${entry.mainOptionName}
					</c:otherwise>
						</c:choose>
					</a>
				<ul class="dropdown-menu damage-wavier bl-options-update"
					aria-labelledby="coverageOptions1">
					<c:forEach items="${entry.option}" var="subOptions">
						<li><a class="dropdown-item" href="#" href="#" data-id="${subOptions.optionCode}" data-entry="${entry.entryNumber}" data-product-code="${entry.product.code}">${subOptions.optionName} </a><span class="float-end">
						  <c:choose>
                <c:when test="${isReplacementOrderCart eq true}">
                <spring:theme code="text.replacement.option.cost"/>
                </c:when>
                <c:otherwise>
						        <format:price  priceData="${subOptions.optionPrice}" />
						    </c:otherwise>
						   </c:choose>
						</span></li>
					</c:forEach>
				</ul>
			</div>
		</div>
	</div>
</c:if>

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
             <c:choose>
             	<c:when test="${not empty entryNumber and not empty entryMessage and entryNumber == entry.entryNumber}">
             		<div class="notification notification-error"><spring:theme code="${entryMessage.messageCode}" arguments="${entryMessage.arguments}" htmlEscape= "false"/></div>
             	</c:when>
             	<c:when test="${not empty entry.availabilityMessage }">
             		<div class="notification notification-error"><spring:theme code="${entry.availabilityMessage.messageCode}" arguments="${entry.availabilityMessage.arguments}" htmlEscape= "false"/></div>
             	</c:when>
             	<c:when test="${entry.product.stock.stockLevelStatus eq 'outOfStock'}">
             		<div class="notification notification-error"><spring:theme code="text.stock.not.available"/></div>
             	</c:when>
             </c:choose>
         </div>
     </div>
</div>