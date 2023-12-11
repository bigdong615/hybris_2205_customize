<%@ tag body-content="scriptless" trimDirectiveWhitespaces="true" %>
<%@ attribute name="pageTitle" required="false" rtexprvalue="true" %>
<%@ attribute name="metaDescription" required="false" %>
<%@ attribute name="metaKeywords" required="false" %>
<%@ attribute name="pageCss" required="false" fragment="true" %>
<%@ attribute name="pageScripts" required="false" fragment="true" %>

<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template" %>
<%@ taglib prefix="analytics" tagdir="/WEB-INF/tags/shared/analytics" %>
<%@ taglib prefix="addonScripts" tagdir="/WEB-INF/tags/responsive/common/header" %>
<%@ taglib prefix="generatedVariables" tagdir="/WEB-INF/tags/shared/variables" %>
<%@ taglib prefix="debug" tagdir="/WEB-INF/tags/shared/debug" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="htmlmeta" uri="http://hybris.com/tld/htmlmeta"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="tealium" tagdir="/WEB-INF/tags/addons/tealiumiqaddon/shared/analytics" %>
<spring:htmlEscape defaultHtmlEscape="true" />

<!DOCTYPE html>
<html lang="${fn:escapeXml(currentLanguage.isocode)}">
<head>
    <%--Analytics Google Tag Manager Header
    <analytics:googleTagManagerHeader/> --%>
	<title>
		${not empty pageTitle ? pageTitle : not empty cmsPage.title ? fn:escapeXml(cmsPage.title) : 'Accelerator Title'}
	</title>

	<%-- Meta Content --%>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta charset="utf-8">
	<meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=no">

	<%-- Additional meta tags --%>
	<htmlmeta:meta items="${metatags}"/>

	<%-- Favourite Icon --%>
	<spring:theme code="img.favIcon" text="/" var="favIconPath"/>
	
	<c:choose>
		<%-- if empty webroot, skip originalContextPath, simply use favIconPath --%>
		<c:when test="${fn:length(originalContextPath) eq 1}" >
			<link rel="shortcut icon" type="image/x-icon" media="all" href="${favIconPath}" />
		</c:when>
		<c:otherwise>
			<link rel="shortcut icon" type="image/x-icon" media="all" href="${originalContextPath}${favIconPath}" />
		</c:otherwise>
	</c:choose>
	<%-- Tealium sync data --%>
    <tealium:sync/>

	<%-- CSS Files Are Loaded First as they can be downloaded in parallel --%>
	<template:styleSheets/>

	<%-- Inject any additional CSS required by the page --%>
	<jsp:invoke fragment="pageCss"/>

	<%-- Google Analytics --%>
	<analytics:analytics/>
	<generatedVariables:generatedVariables/>
	<script src='https://www.dwin1.com/53833.js' type='text/javascript' defer='defer'></script>
   <%-- BLS-78 --%>
	<c:if test="${cmsPage.uid eq 'productGrid' || cmsPage.uid eq 'search' || cmsPage.uid eq 'searchEmpty' || cmsPage.uid eq 'productDetails'}">
  <link rel="canonical" href="https://www.borrowlenses.com${request.getAttribute('javax.servlet.forward.request_uri')}" />
	</c:if>
	
	<c:if test="${cmsPage.uid eq 'productGrid' || cmsPage.uid eq 'search'}">
	<c:set var="hasPreviousPage" value="${searchPageData.pagination.currentPage > 0}"/>
          <c:set var="hasNextPage" value="${(searchPageData.pagination.currentPage + 1) < searchPageData.pagination.numberOfPages}"/>
          <c:if test="${hasPreviousPage}">
             <spring:url value="${searchPageData.currentQuery.url}" var="previousPageUrl" htmlEscape="true">
                <spring:param name="page" value="${searchPageData.pagination.currentPage - 1}"/>
             </spring:url>
          </c:if>
          <c:if test="${hasNextPage}">
             <spring:url value="${searchPageData.currentQuery.url}" var="nextPageUrl" htmlEscape="true">
                <spring:param name="page" value="${searchPageData.pagination.currentPage + 1}"/>
             </spring:url>
          </c:if>
           <c:if test="${hasPreviousPage}">
          <link rel="prev" href="https://www.borrowlenses.com${previousPageUrl}<c:if test="${cmsPage.uid eq 'search'}">&blPageType=${blPageType} </c:if>" />
          </c:if>
           <c:if test="${hasNextPage}">
          <link rel="next" href="https://www.borrowlenses.com${nextPageUrl}<c:if test="${cmsPage.uid eq 'search'}">&blPageType=${blPageType} </c:if>" />
          </c:if>
	</c:if>


    <%-- BLS-416: Add schema.org information to the Homepage --%>
    <c:if test="${cmsPage.uid eq 'homepage'}">
        <analytics:schemaOrg/>
    </c:if>

    <%-- BLS-417: Add schema.org information to the PDP --%>
    <c:if test="${cmsPage.uid eq 'productDetails'}">
        <analytics:pdpSchemaOrg/>
    </c:if>
</head>
<body class="${pageBodyCssClasses} ${cmsPageRequestContextData.liveEdit ? ' yCmsLiveEdit' : ''} language-${fn:escapeXml(currentLanguage.isocode)}">

<%-- Analytics Google Tag Manager Body
 <analytics:googleTagManagerBody/> --%>
<!-- Talkable integration Script -->
<analytics:talkableScript/>

<analytics:shareASaleLeadScript/>
<%-- Tealium Data --%>
    <tealium:tealium/>
	<%-- Inject the page body here --%>

	<jsp:doBody/>
	<form name="accessiblityForm">
		<input type="hidden" id="accesibility_refreshScreenReaderBufferField" name="accesibility_refreshScreenReaderBufferField" value=""/>
	</form>
	<div id="ariaStatusMsg" class="skip" role="status" aria-relevant="text" aria-live="polite"></div>

	<%-- Load JavaScript required by the site --%>
	<template:javaScript/>
	
	<%-- Inject any additional JavaScript required by the page --%>
	<jsp:invoke fragment="pageScripts"/>

	<%-- Inject CMS Components from addons using the placeholder slot--%>
	<addonScripts:addonScripts/>


</body>

<debug:debugFooter/>

</html>
