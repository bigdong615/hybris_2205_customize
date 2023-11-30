<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>

<%@ taglib prefix="cms" tagdir="/WEB-INF/tags/responsive/template/cms" %>

<link rel="stylesheet" type="text/css" href="//fonts.googleapis.com/css?family=Open+Sans:400,300,300italic,400italic,600,600italic,700,700italic,800,800italic" />
<c:choose>
	<c:when test="${wro4jEnabled}">
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(contextPath)}/wro/all_responsive.css" />
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(contextPath)}/wro/${fn:escapeXml(themeName)}_responsive.css" />
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(contextPath)}/wro/addons_responsive.css" />
	</c:when>
	<c:otherwise>
		<%-- Theme CSS files --%>
		<!-- Bootstrap core CSS -->
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/bootstrap.min.css"/>
		<link rel="stylesheet" href="https://d2ieyhi8galmxj.cloudfront.net/litepicker/litepicker.css"/>
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/splide-core.min.css"/>
		<!-- Mobile Nav -->
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/mmenu.css"/>
		<!-- Favicons -->
		<link rel="apple-touch-icon" href="#" sizes="180x180">
    	<link rel="icon" href="#" sizes="32x32" type="image/png">
    	<link rel="icon" href="#" sizes="16x16" type="image/png">
    	<!-- Custom styles for this template -->
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/mburger.css"/>
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/mmenu-light.css"/>
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/cart.css"/>
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/blstyle.css"/>
		<%--  BL Custom CSS file--%>
		<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/blcustom.css"/>
		<%--  AddOn Common CSS files --%>
		<c:forEach items="${addOnCommonCssPaths}" var="addOnCommonCss">
			<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(addOnCommonCss)}"/>
		</c:forEach>
	</c:otherwise>
</c:choose>

<%--  AddOn Theme CSS files --%>
<c:forEach items="${addOnThemeCssPaths}" var="addOnThemeCss">
	<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(addOnThemeCss)}"/>
</c:forEach>

<cms:previewCSS cmsPageRequestContextData="${cmsPageRequestContextData}" />
