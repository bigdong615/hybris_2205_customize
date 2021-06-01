<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="sec" uri="http://www.springframework.org/security/tags"%>
<c:url value="${component.urlLink}" var="siteUrl"/>
<a class="navbar-brand" href="${siteUrl}"><img src="${component.media.url}"></a>
