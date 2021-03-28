<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>

<c:if test="${isFirstElement}">
<li class="me-auto my-auto ${fn:toLowerCase(feature.brandImageTitle)}">
</c:if>
<c:if test="${!isFirstElement and !isLastElement}">
<li class="mx-auto my-auto ${fn:toLowerCase(feature.brandImageTitle)}">
</c:if>
<c:if test="${isLastElement}">
<li class="ms-auto my-auto ${fn:toLowerCase(feature.brandImageTitle)}">
</c:if>
<c:url value="${feature.urlLink}" var="brandUrl"/>
<a href="${brandUrl}"><img src="${feature.media.url}"></a>
</li>
