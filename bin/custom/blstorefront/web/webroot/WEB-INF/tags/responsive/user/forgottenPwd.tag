<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<div class="modal-dialog modal-dialog-centered modal-sm">
   <div class="modal-content">
      <div class="modal-header">
         <h5 class="modal-title text-center"><img class="logo" src="${themeResourcePath}/assets/bl-logo@2x.png"></h5>
         <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
      </div>
      <div class="modal-body">
         <h5>
            <spring:theme code="forgottenPwd.header"/>
         </h5>
         <p class="body14">
            <spring:theme code="forgottenPwd.description"/>
         </p>
         <c:url value="/login/pw/request" var="passwordResetActionUrl" />
         <form:form method="post" modelAttribute="forgottenPwdForm" id="forgottenPwdForm">
            <ycommerce:testId code="login_forgotPasswordEmail_input">
               <formElement:formInputBox idKey="forgottenPwd.email"
                  inputCSS="form-control mb-3" path="email" mandatory="true" placeholder="forgottenPwd.email"/>
            </ycommerce:testId>
            <ycommerce:testId code="login_forgotPasswordSubmit_button">
               <button class="btn btn-block btn-primary mt-4 js-password-reset" type="submit" value="${passwordResetActionUrl}">
                  <spring:theme code="forgottenPwd.submit"/>
               </button>
            </ycommerce:testId>
            <p class="body14 text-center mb-0 mt-4">
               <a class="js-login-popup" href="#signIn" data-link="<c:url value='/login/loginpopup'/>" data-bs-dismiss="modal">
                  <spring:theme code="login.login" />
               </a>
            </p>
         </form:form>
      </div>
   </div>
</div>
