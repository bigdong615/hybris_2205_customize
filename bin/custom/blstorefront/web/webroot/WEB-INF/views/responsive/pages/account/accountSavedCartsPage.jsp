<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>

<spring:htmlEscape defaultHtmlEscape="true"/>

<spring:url value="/my-account/saved-carts/" var="savedCartsLink" htmlEscape="false"/>
<c:set var="searchUrl" value="/my-account/saved-carts?sort=${ycommerce:encodeUrl(searchPageData.pagination.sort)}"/>

<div id="accountContent" class="col-lg-8 offset-lg-1">
	<h1>Saved Carts</h1>
	<c:choose>
	<c:when test="${empty searchPageData.results}">
      <div class="account-section-content content-empty">
          <ycommerce:testId code="savedCarts_noOrders_label">
              <spring:theme code="text.account.savedCarts.noSavedCarts"/>
          </ycommerce:testId>
      </div>
  </c:when>
	<c:otherwise>
		<c:forEach items="${searchPageData.results}" var="savedCart" varStatus="loop">
			<div class="order-block">
				<div class="row">
					<div class="col-12 col-md-7 my-auto"> <b>${fn:escapeXml(savedCart.name)}</b>
						<p class="gray80 body14">
							<fmt:formatDate value="${savedCart.saveTime}" dateStyle="medium" />
							<br> ${fn:escapeXml(savedCart.totalPrice.formattedValue)} </p>
						<div class="row mb-4 mb-md-0 order-images">
							<c:forEach items="${savedCart.entries}" var="entry">
								<div class="col-4 col-md-3">
									<product:productPrimaryImage product="${entry.product}" format="product" />
								</div>
							</c:forEach>
						</div>
					</div>
					<div class="col-6 col-md-3 offset-md-1 text-start text-md-end">
						<a href="#" class="js-restore-saved-cart restore-item-link btn btn-primary" data-savedcart-id="${fn:escapeXml(savedCart.code)}" data-restore-popup-title="<spring:theme code='text.account.savedcart.restore.popuptitle'/>"> <span class="hidden-xs">Use This Cart</span> </a>
					</div>
					<div class="col-6 col-md-1">
						<div class="btn-group"> <a id="btn-rental-${loop.index}" class="dropdown-toggle" data-bs-toggle="dropdown" aria-expanded="false" href="#"><i class="icon-dots"></i></a>
							<ul class="dropdown-menu" aria-labeledby="btn-rental-001">
								<li><a href="#" data-bs-toggle="modal" data-bs-target="#renameCart">Rename</a></li>
								<li>
									<a href="#" class="js-delete-saved-cart remove-item-link" data-savedcart-id="${fn:escapeXml(savedCart.code)}" data-delete-popup-title="Remove"> <span class="hidden-xs">Remove</span> </a>
								</li>
							</ul>
						</div>
					</div>
				</div>
			</div>
		</c:forEach>
		 <nav:pagination searchPageData="${searchPageData}" searchUrl="${searchUrl}"/>
	</c:otherwise>

</c:choose>
</div>
