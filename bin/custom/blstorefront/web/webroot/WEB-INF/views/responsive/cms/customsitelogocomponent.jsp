<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="sec" uri="http://www.springframework.org/security/tags"%>
<c:url var="homePageUrl" value="${component.urlLink }"/>
<a class="navbar-brand" href="${homePageUrl }"><img src="${component.media.url}"></a>
