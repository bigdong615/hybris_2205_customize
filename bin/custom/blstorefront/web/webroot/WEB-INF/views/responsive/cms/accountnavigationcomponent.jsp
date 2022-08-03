<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="sec"	uri="http://www.springframework.org/security/tags"%>


<script type="text/javascript">
	function closeMenu() {
		$('#my-menu').removeClass('mm-menu_opened');
		//$('#signIn').modal('show');
	}
</script>

<!-- Mobile device account section -->
<c:if test="${positionAttribute == 'MobileHeaderLinkForAccountSlot'}">
<c:url value="#" var="urlLink"/>
	
	<!-- To display myaccount in hamberger menu in mobile view -->
	 <a href="#mm-7"><i class="icon-myaccount"></i> ${component.name}</a>  
	 <ul>
	<sec:authorize access="hasAnyRole('ROLE_ANONYMOUS')">
		<c:url value="/login" var="loginUrl" />
		<li onclick="closeMenu()"><a class="dropdown-item js-signUp-popup"  data-link="<c:url value='/login/register'/>" href="#" data-bs-toggle="modal" data-bs-target="#signUp"><spring:theme code="text.header.account.create.account" /></a></li>
		<li onclick="closeMenu()"><a class="dropdown-item js-login-popup"  data-link="<c:url value='/login/loginpopup'/>" href="#" data-bs-toggle="modal" data-bs-target="#signIn"><spring:theme code="text.header.account.sign.in" /></a></li>
 
 		</sec:authorize>
		<sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
		<c:forEach items="${component.navigationNode.entries}" var="entry">
		<c:if test="${ not empty entry.item.linkName}">
		  <c:url var="acountNavURL" value="${entry.item.url }"/>
			<li><a class="dropdown-item" href="${acountNavURL}">${entry.item.linkName }</a></li>
			</c:if>
		</c:forEach>		
		<!-- <li class="divider"></li> -->
		<c:url value="/logout" var="signoutUrl" />
		<li><a class="dropdown-item usedgear-signout" href="${signoutUrl}"><spring:theme code="text.header.account.sign.out" /></a></li>
		</sec:authorize>		
		</ul>
		
		
		
<%-- <c:forEach items="${component.navigationNode.entries}"	var="entry">
		<c:if test="${entry.item.type  eq 'Link'}">
			<c:url value="${entry.item.url}" var="urlLink"/>
		</c:if>
	</c:forEach> --%>
	<%-- <a href="${urlLink}"><i class="icon-myaccount"></i> ${component.name}</a>  --%>
	
	
</c:if>
<c:if test="${positionAttribute == 'MyAccountSlot'}">
<a class="nav-link dropdown-toggle" href="#" id="accountdropdown"
	data-bs-toggle="dropdown" aria-expanded="false">${component.name }</a>
<div class="dropdown-menu dropdown-menu-right"
	aria-labelledby="accountdropdown">
	<h5>${component.name }</h5>
	<ul>
		<sec:authorize access="hasAnyRole('ROLE_ANONYMOUS')">
		<c:url value="/login" var="loginUrl" />
		<li><a class="dropdown-item js-signUp-popup"  data-link="<c:url value='/login/register'/>" href="#" data-bs-toggle="modal" data-bs-target="#signUp"><spring:theme code="text.header.account.create.account" /></a></li>
		<li><a class="dropdown-item js-login-popup"  data-link="<c:url value='/login/loginpopup'/>" href="#" data-bs-toggle="modal" data-bs-target="#signIn"><spring:theme code="text.header.account.sign.in" /></a></li>
		</sec:authorize>
		<sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
		<c:forEach items="${component.navigationNode.entries}" var="entry">
		  <c:url var="acountNavURL" value="${entry.item.url }"/>
			<li><a class="dropdown-item" href="${acountNavURL}">${entry.item.linkName }</a></li>
		</c:forEach>		
		<li class="divider"></li>
		<c:url value="/logout" var="signoutUrl" />
		<li><a class="dropdown-item usedgear-signout" href="${signoutUrl}"><spring:theme code="text.header.account.sign.out" /></a></li>
		</sec:authorize>		
	</ul>
</div>
</c:if>
