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
<c:url value="/buy/product/${entry.product.code}" var="productUrl"/>
<c:url value="/cart/update" var="cartUpdateFormAction" />

<div class="cartProduct">
     <div class="row">
         <div class="col-md-2 text-center">
          <a href="${fn:escapeXml(productUrl)}" class="js-pdplinkUrl" data-productCode="${entry.product.code}"
            data-brand="${entry.product.manufacturer}" data-productName="${ycommerce:sanitizeHTML(entry.product.name)}" data-productType="">
          <product:productPrimaryImage product="${entry.product}" format="thumbnail"/></a>
         </div>
         <div class="col-md-7 mt-3">
           <a href="${fn:escapeXml(productUrl)}" class="js-pdplinkUrl" data-productCode="${entry.product.code}" data-brand="${entry.product.manufacturer}"
             data-productName="${ycommerce:sanitizeHTML(entry.product.name)}" data-productType="rental"><b>${entry.product.name}</b></a>
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
             <b><format:price priceData="${entry.totalPrice}" displayFreeForZero="true" /></span></b>
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
</div>