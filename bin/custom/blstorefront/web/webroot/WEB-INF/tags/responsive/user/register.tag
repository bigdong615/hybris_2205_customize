<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="actionNameKey" required="true" type="java.lang.String"%>
<%@ attribute name="action" required="true" type="java.lang.String"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<spring:url value="/login/register/termsandconditions" var="getTermsAndConditionsUrl"/>

<div class="user-register__headline">
	<spring:theme code="register.new.customer" />
</div>
<p>
	<spring:theme code="register.description" />
</p>

<form:form method="post" modelAttribute="registerForm" action="${action}">

	<formElement:formInputBox idKey="register.email"
		 path="email" inputCSS="form-control" />
	<formElement:formPasswordBox idKey="password"
		path="pwd" inputCSS="form-control "/>
	<formElement:formPasswordBox idKey="register.checkPwd"
		 path="checkPwd" inputCSS="form-control"/>

    <c:if test="${ not empty consentTemplateData }">
        <form:hidden path="consentForm.consentTemplateId" value="${consentTemplateData.id}" />
        <form:hidden path="consentForm.consentTemplateVersion" value="${consentTemplateData.version}" />
        <div class="checkbox">
            <label class="control-label uncased">
                <form:checkbox path="consentForm.consentGiven" disabled="true"/>
                <c:out value="${consentTemplateData.description}" />

            </label>
        </div>
		<div class="help-block">
			<spring:theme code="registration.consent.link" />
		</div>

    </c:if>

	<spring:theme code="register.termsConditions" arguments="${getTermsAndConditionsUrl}" var="termsConditionsHtml" htmlEscape="false" />
	<input type="hidden" id="recaptchaChallangeAnswered"
		value="${fn:escapeXml(requestScope.recaptchaChallangeAnswered)}" />
	<div class="form_field-elements control-group js-recaptcha-captchaaddon"></div>
	<div class="form-actions clearfix">
		<ycommerce:testId code="register_Register_button">
			<button type="submit" class="btn btn-default btn-block">
				<spring:theme code='${actionNameKey}' />
			</button>
		</ycommerce:testId>
	</div>
</form:form>
