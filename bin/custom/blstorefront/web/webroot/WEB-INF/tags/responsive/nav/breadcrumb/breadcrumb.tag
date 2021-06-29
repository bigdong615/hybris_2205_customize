<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="breadcrumbs" required="true" type="java.util.List"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:url value="/" var="homeUrl" />

<a href="${homeUrl}"><spring:theme code="breadcrumb.home" /></a> &gt;
<c:forEach items="${breadcrumbs}" var="breadcrumb" varStatus="status">

		<spring:url htmlEscape="false" value="${breadcrumb.url}" var="breadcrumbUrl" />
		<c:choose>
			<c:when test="${status.last}">
			<a href="#">${fn:escapeXml(breadcrumb.name)} </a>

			<form class="add_to_wishList_form" action="${addWishList}" method="post" id="js-wishlist-form">
             <input type="hidden" name="productCodePost" id="productCodePost" value="${product.code}">
             <c:choose>
              <c:when test="${product.isDiscontinued}">
                   <span class="bookmark" disabled="disabled"></span>
               </c:when>
              <c:when test="${product.isBookMarked}">
                <span class="bookmark set js-add-to-wishlist" id="card-${status.index}" data-product-code="${product.code}"
                data-bookmark-value="${product.isBookMarked}">${product.isBookMarked}hhh</span>
              </c:when>
             <c:otherwise>
              <span class="bookmark js-add-to-wishlist" id="card-${status.index}" data-product-code="${product.code}"
              data-bookmark-value="${product.isBookMarked}"></span>
             </c:otherwise>
            </c:choose>
      </form>
			</c:when>
			<c:when test="${breadcrumb.url eq '#'}">
					<a href="#">${fn:escapeXml(breadcrumb.name)}</a>&gt;
			</c:when>
			<c:otherwise>
					<a href="${fn:escapeXml(breadcrumbUrl)}">${fn:escapeXml(breadcrumb.name)}</a>&gt;
			</c:otherwise>
		</c:choose>
	</c:forEach>
