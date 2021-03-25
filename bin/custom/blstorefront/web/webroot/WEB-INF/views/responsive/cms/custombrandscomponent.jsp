<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<li class="me-auto my-auto canon">
<c:url value="${feature.urlLink }" var="brandUrl"/>
<a href="${brandUrl}"><img src="${feature.media.url}"></a>
</li>
