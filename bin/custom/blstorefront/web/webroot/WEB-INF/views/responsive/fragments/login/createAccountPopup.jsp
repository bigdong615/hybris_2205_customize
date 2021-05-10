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
              <h5><spring:theme code="register.submit" /></h5>
          <c:url value="/login/register" var="registerActionUrl" />
          <form:form method="post" modelAttribute="registerForm" action="${registerActionUrl}">
               <formElement:formInputBox idKey="register.email" path="email"
                 inputCSS="form-control mb-3"  placeholder="register.email"/>
                <formElement:formPasswordBox idKey="password"  path="pwd"
                  inputCSS="form-control mb-2 "  placeholder="register.pwd" />
                <formElement:formPasswordBox idKey="register.checkPwd"
                 path="checkPwd" inputCSS="form-control mb-2"  placeholder="register.checkPwd" />
                  <ycommerce:testId code="register_Register_button">
                  			<button type="submit" class="btn btn-block btn-primary mt-4">
                    				<spring:theme code="register.submit" />
                   			</button>
                 	</ycommerce:testId>
              <p class="body14 text-center mb-0 mt-4"><a class="js-login-popup" href="#signIn" data-link="<c:url value='/login/loginpopup'/>"
              data-bs-toggle="modal" data-bs-dismiss="modal"><spring:theme code="login.login" /></a>&nbsp; <spring:theme code="login.suggestion.text"/></p>
           </form:form>
          </div>
        </div>
        </div>