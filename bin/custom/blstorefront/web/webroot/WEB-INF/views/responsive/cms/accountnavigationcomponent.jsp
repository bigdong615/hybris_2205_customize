<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<a class="nav-link dropdown-toggle" href="#" id="accountdropdown" data-bs-toggle="dropdown" aria-expanded="false">${component.name }</a>
<div class="dropdown-menu dropdown-menu-right" aria-labelledby="accountdropdown">
	<h5>${component.name }</h5>
	<ul>
		<c:forEach items="${component.navigationNode.entries}"	var="entry">
			<li><a class="dropdown-item" href="${entry.item.url }">${entry.item.linkName }</a></li>
		</c:forEach>
		<li><a class="dropdown-item" href="#"><spring:theme code="text.header.account.sign.in"/></a></li>
		<li class="divider"></li>
		<li><a class="dropdown-item" href="#"><spring:theme code="text.header.account.sign.out"/></a></li>
	</ul>
</div>