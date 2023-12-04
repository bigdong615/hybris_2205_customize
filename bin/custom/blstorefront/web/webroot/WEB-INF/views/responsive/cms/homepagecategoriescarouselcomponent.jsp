<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

	<c:url value="${feature.urlLink }" var="categoryUrl"/>
	<a href="${categoryUrl }" class="cat-holder"> 
		<img src="${feature.media.url}" style="width: 100%;">
		<h6>${feature.imagetitle}</h6>
	</a>
