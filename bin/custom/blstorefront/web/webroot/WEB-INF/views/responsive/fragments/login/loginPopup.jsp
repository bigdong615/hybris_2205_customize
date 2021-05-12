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
        <spring:theme code="login.login" />
      </h5>
      <c:url value="/j_spring_security_check" var="loginActionUrl" />
      <form:form action="${loginActionUrl}" method="post" modelAttribute="loginForm">
        <formElement:formInputBox idKey="j_username" path="j_username"
          inputCSS="form-control mb-3" placeholder="Email"/>
        <formElement:formPasswordBox idKey="j_password"
          path="j_password" inputCSS="form-control mb-2"  placeholder="Password" />
        <input type="checkbox" name="remember-me" id="_spring_security_remember_me">
        <label for="_spring_security_remember_me">
          <small>
            <spring:theme code="login.rememberme"/>
          </small>
        </label>
        <a href="#forgotPass" data-bs-toggle="modal" data-bs-dismiss="modal" class="float-end">
          <small>
            <spring:theme code="login.link.forgottenPwd"/>
          </small>
        </a>
        <ycommerce:testId code="loginAndCheckoutButton">
          <button type="submit" class="btn btn-block btn-primary mt-4">
            <spring:theme code="login.login" />
          </button>
        </ycommerce:testId>
        <p class="body14 text-center mb-0 mt-4">
          <a class="js-signUp-popup" data-link="<c:url value='/login/register'/>"
            href="#signUp" data-bs-toggle="modal" data-bs-dismiss="modal">
            <spring:theme code="register.new.customer"/>
          </a>
        </p>
      </form:form>
    </div>
  </div>
</div>