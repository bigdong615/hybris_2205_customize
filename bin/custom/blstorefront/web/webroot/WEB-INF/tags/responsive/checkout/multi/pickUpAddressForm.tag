<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement" %>
<%@ taglib prefix="address" tagdir="/WEB-INF/tags/responsive/address"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<div class="mt-2 mb-4">
    <form:form method="POST" modelAttribute="blPickUpByForm">
        <formElement:formInputBox idKey="blPickUpBy.firstName" placeholder="blPickUpBy.firstName" labelKey="" path="firstName" inputCSS="form-control" mandatory="true" />
        <formElement:formInputBox idKey="blPickUpBy.lastName" labelKey="" placeholder="blPickUpBy.lastName" path="lastName" inputCSS="form-control" mandatory="true" />
        <formElement:formInputBox idKey="blPickUpBy.email" labelKey="" placeholder="blPickUpBy.email" path="email" inputCSS="form-control" mandatory="true" />
        <formElement:formInputBox idKey="blPickUpBy.phone" labelKey="" placeholder="blPickUpBy.phone" path="phone" inputCSS="form-control" mandatory="true" />
    </form:form>
</div>