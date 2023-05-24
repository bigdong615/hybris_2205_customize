<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>


<div class="modal-dialog modal-dialog-centered modal-sm">
  <div class="modal-content">
    <div class="modal-header">
      <h5 class="modal-title text-center"><img class="logo" src="${themeResourcePath}/assets/bl-logo@2x.png"></h5>
      <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
    </div>
    <div class="modal-body">
      <h5>
        <spring:theme code="register.submit" />
      </h5>
      <c:url value="/login/register" var="registerActionUrl" />
      <form:form method="post" modelAttribute="registerForm" action="${registerActionUrl}" id="signUppopup-validation">
      
      
      <formElement:formInputBox idKey="firstName" path="firstName"
          inputCSS="form-control mb-3"  placeholder="register.firstName"/>
          <formElement:formInputBox idKey="lastName" path="lastName"
          inputCSS="form-control mb-3"  placeholder="register.lastName"/>
      
      
      
        <formElement:formInputBox idKey="register-form-id" path="email"
          inputCSS="form-control mb-3"  placeholder="register.email"/>
        <formElement:formPasswordBox idKey="password"  path="pwd"
          inputCSS="form-control mb-2 "  placeholder="register.pwd" />
        <formElement:formPasswordBox idKey="checkPwd-form-id"
          path="checkPwd" inputCSS="form-control mb-2"  placeholder="register.checkPwd" />
           <input type="hidden" id="serialClickSignUP" name="serialClickSignUP" value=""/>
           <input type="hidden" id="bookmarkClickSignUP" name="bookmarkClickSignUP" value=""/>
        <ycommerce:testId code="register_Register_button">
          <button type="submit" class="btn btn-block btn-primary mt-4 js-signUp-popup-validation" value="${registerActionUrl}">
            <spring:theme code="register.submit" />
          </button>
        </ycommerce:testId>

        <c:if test="${not empty accErrorMsgs}">
                                         			<c:forEach items="${accErrorMsgs}" var="msg">
                                         					<spring:theme code="${msg.code}" arguments="${msg.attributes}" htmlEscape="false" var="errorMessages"/>
                                               	<input type="hidden" name="errorMessages_id" id="errorMessages_id-signup" data-value="${ycommerce:sanitizeHTML(errorMessages)}"/>
                                         			</c:forEach>
                           </c:if>

           <div class ="notification notification-error d-none" id="errorMessages_sigin_errorbox">
                                     <!--  BL:689 changes in below two line -->
                                     <div id="errorMessages_sigin_firstName" class="mb-2"></div>
                                     <div id="errorMessages_sigin_lastName" class="mb-2"></div>
                                       <div id="errorMessages_sigin_email" class="mb-2"></div>
                                       <div id="errorMessages_sigin_pwd" class="mb-2"></div>
                                       <div id="errorMessages_sigin_chkPwd"></div>
           </div>
        <p class="body14 text-center mb-0 mt-4">
          <a class="js-login-popup" id="serialSignInInstead" data-click="" href="#signIn" data-link="<c:url value='/login/loginpopup'/>"
            data-product-code="" data-bs-dismiss="modal">
            <spring:theme code="login.login" />
          </a>
          <spring:theme code="login.suggestion.text"/>
        </p>
      </form:form>
    </div>
  </div>
</div>
