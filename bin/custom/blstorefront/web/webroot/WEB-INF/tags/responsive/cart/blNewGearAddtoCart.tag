<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ attribute name="productData" required="false" type="de.hybris.platform.commercefacades.product.data.ProductData" %>
<%@ taglib prefix="sec"	uri="http://www.springframework.org/security/tags"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:set var="product" value="${product}"/>

<c:if test="${empty product and not empty productData}">
	<c:set var="product" value="${productData}"/>
</c:if>

<c:if test="${not product.multidimensional}">
    <c:url value="/cart/add" var="addToCartUrl"/>
	<spring:url value="${product.url}/configuratorPage/{/configuratorType}" var="configureProductUrl" htmlEscape="false">
		<spring:param name="configuratorType" value="${configuratorType}" />
	</spring:url>
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
                 <c:if test="${product.isRetailGearInStock eq false}"> disabled</c:if>  data-product-code="${product.code}">
                      <spring:theme code="text.add.to.cart" />
						    </button>
						</form:form>

</c:if>
