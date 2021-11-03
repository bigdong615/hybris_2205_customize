<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ attribute name="productData" required="false" type="de.hybris.platform.commercefacades.product.data.ProductData" %>
<%@ taglib prefix="sec"	uri="http://www.springframework.org/security/tags"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:set var="product" value="${product }"/>

<c:if test="${empty product and not empty productData}">
	<c:set var="product" value="${productData }"/>
</c:if>

<c:if test="${not product.multidimensional }">
    <c:url value="/cart/add" var="addToCartUrl"/>
	<spring:url value="${product.url}/configuratorPage/{/configuratorType}" var="configureProductUrl" htmlEscape="false">
		<spring:param name="configuratorType" value="${configuratorType}" />
	</spring:url>
     <c:choose>
     			<c:when test="${product.isUpcoming eq true && product.isBundle ne true}">
     			 <sec:authorize access="hasAnyRole('ROLE_ANONYMOUS')">
                 <button type="submit" class="btn btn-primary dis btnwidthplp js-login-popup" data-link="<c:url value='/login/loginpopup'/>"
                   data-bs-toggle="modal"  data-bs-target="#signIn" aria-disabled="false">
                  <spring:theme code="text.get.notified" />
                  </button>
     			  </sec:authorize>
     			  <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
     			  <c:choose>
                     <c:when test="${product.isWatching}">
                        <button type="submit" class="btn btn-primary dis btnwidthplp removeInterestbtn"  data-box-productcode="${product.code}"
                               data-bs-toggle="modal" data-bs-target="#getNotified" aria-disabled="false">
                    			<spring:theme code="text.remove.notified.button.text"/>
       					</button>
                     </c:when>
                     <c:otherwise>
                      <button type="submit" class="btn btn-primary dis btnwidthplp arrival-notification"  data-box-productcode="${product.code}"
                                data-bs-toggle="modal" data-bs-target="#getNotified" aria-disabled="false">
                          			<spring:theme code="text.get.notified" />
                      </button>
                     </c:otherwise>
                  </c:choose>
     			   </sec:authorize>
				</c:when>
				<c:when test="${product.isDiscontinued or product.stock.stockLevelStatus.code eq 'outOfStock' || not empty product.nextAvailableDate && product.disableButton eq 'true'}">
					<button type="submit" class="btn btn-outline btn-disabled btnwidthplp js-add-to-cart js-disable-btn"
						aria-disabled="true" disabled="disabled">
						<spring:theme code="text.add.to.rental" />
					</button>
				</c:when>
				<c:otherwise>
				<c:choose>

				  <c:when test="${allowAddToCart || isRentalCart}">
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
                <button type="button" class="btn btn-primary js-add-to-cart btnwidthplp" data-bs-toggle="modal" data-bs-target="#addToCart" data-product-code="${product.code}">
                      <spring:theme code="text.add.to.rental" />
						    </button>
						</form:form>
				</c:otherwise>
		</c:choose>

     <form:form id="configureForm${fn:escapeXml(product.code)}" action="${configureProductUrl}" method="get" class="configure_form">
        <c:if test="${product.configurable}">
            <c:choose>
                <c:when test="${product.stock.stockLevelStatus.code eq 'outOfStock' }">
                    <button id="configureProduct" type="button" class="btn btn-primary btn-block"
                            disabled="disabled">
                        <spring:theme code="basket.configure.product"/>
                    </button>
                </c:when>
                <c:otherwise>
                    <button id="configureProduct" type="button" class="btn btn-primary btn-block js-enable-btn"
                            onclick="location.href='${fn:escapeXml(configureProductUrl)}'">
                        <spring:theme code="basket.configure.product"/>
                    </button>
                </c:otherwise>
            </c:choose>
        </c:if>
    </form:form>
</c:if>
