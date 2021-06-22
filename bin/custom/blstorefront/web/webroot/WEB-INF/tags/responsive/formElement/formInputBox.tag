<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="idKey" required="true" type="java.lang.String"%>
<%@ attribute name="labelKey" required="false" type="java.lang.String"%>
<%@ attribute name="path" required="true" type="java.lang.String"%>
<%@ attribute name="mandatory" required="false" type="java.lang.Boolean"%>
<%@ attribute name="inputCSS" required="false" type="java.lang.String"%>
<%@ attribute name="placeholder" required="false" type="java.lang.String"%>
<%@ attribute name="tabindex" required="false" rtexprvalue="true"%>
<%@ attribute name="autocomplete" required="false" type="java.lang.String"%>
<%@ attribute name="disabled" required="false" type="java.lang.Boolean"%>
<%@ attribute name="maxlength" required="false" type="java.lang.Integer"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ attribute name="labelCSS" required="false" type="java.lang.String"%>
<%@ attribute name="styleCSS" required="false" type="java.lang.String"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>


<spring:htmlEscape defaultHtmlEscape="true" />
<template:errorSpanField path="${path}" >
	<ycommerce:testId code="LoginPage_Item_${idKey}">

<!--	<label class="control-label ${fn:escapeXml(labelCSS)}" for="${fn:escapeXml(idKey)}">
    			<spring:theme code="${labelKey}" />
    			<c:if test="${mandatory != null && mandatory == false}">
    				<span>&nbsp;<spring:theme code="login.optional" /></span>
    			</c:if>
    		</label>-->

		<spring:theme code="${placeholder}" var="placeHolderMessage" htmlEscape="false"/>
		<form:input cssClass="${fn:escapeXml(inputCSS)} " id="${idKey}" path="${path}"
					tabindex="${tabindex}" autocomplete="${autocomplete}" placeholder="${placeHolderMessage}"
					disabled="${disabled}" maxlength="${maxlength}" style="${styleCSS}"/>
	</ycommerce:testId>
</template:errorSpanField>
