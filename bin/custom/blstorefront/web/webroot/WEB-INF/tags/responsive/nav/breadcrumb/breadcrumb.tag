<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="breadcrumbs" required="true" type="java.util.List"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="sec"	uri="http://www.springframework.org/security/tags"%>

<spring:htmlEscape defaultHtmlEscape="true" />
<c:url value="/" var="homeUrl" />

<a href="${homeUrl}"><spring:theme code="breadcrumb.home" /></a> &gt;
<c:forEach items="${breadcrumbs}" var="breadcrumb" varStatus="status">

		<spring:url htmlEscape="false" value="${breadcrumb.url}" var="breadcrumbUrl" />
		<c:choose>
			<c:when test="${status.last}">
			<a href="#">${fn:escapeXml(breadcrumb.name)} </a>
      <c:if test="${product.productType ne 'GIFTCARD' && product.retailGear ne true && product.isDiscontinued ne true && product.forSale ne 'true'}">
      <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
			<form class="add_to_wishList_form" action="${addWishList}" method="post" id="js-wishlist-form">
             <input type="hidden" name="productCodePost" id="productCodePost" value="${product.code}">
             <c:choose>
              <c:when test="${product.isBookMarked}">
                <span class="bookmark set js-add-to-wishlist" id="card-${status.index}" data-product-code="${product.code}"
                data-bookmark-value="${product.isBookMarked}"></span>
              </c:when>
             <c:otherwise>
              <span class="bookmark js-add-to-wishlist" id="card-${status.index}" data-product-code="${product.code}"
              data-bookmark-value="${product.isBookMarked}"></span>
             </c:otherwise>
            </c:choose>
      </form>
      </sec:authorize>
      <sec:authorize access="hasAnyRole('ROLE_ANONYMOUS')">
       <span class="bookmark js-login-popup" id="card-${status.index}" data-product-code="${product.code}"
          data-bs-target="#signIn" data-bs-toggle="modal" data-link="<c:url value='/login/loginpopup'/>"   data-bookmark-value="${product.isBookMarked}"></span>
       </sec:authorize>
      </c:if>
			</c:when>
			<c:when test="${breadcrumb.url eq '#'}">
					<a href="#">${fn:escapeXml(breadcrumb.name)}</a>&gt;
			</c:when>
			<c:otherwise>
			    <c:choose>
    			    <c:when test="${IsRentalPage eq 'false' && product.forSale eq 'true'}">
    			        <c:if test="${status.first eq 'true' && breadcrumb.name eq 'Used Gear'}">
                            <a href="${fn:escapeXml(breadcrumbUrl)}">${fn:escapeXml(breadcrumb.name)}</a>&gt;
    			        </c:if>
	    		    </c:when>
	    		    <c:otherwise>
                        <a href="${fn:escapeXml(breadcrumbUrl)}">${fn:escapeXml(breadcrumb.name)}</a>&gt;
	    		    </c:otherwise>
	    		</c:choose>
			</c:otherwise>
		</c:choose>
	</c:forEach>
