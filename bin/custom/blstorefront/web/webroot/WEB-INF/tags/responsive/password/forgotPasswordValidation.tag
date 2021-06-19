<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
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
           <spring:theme code="forgottenPwd.title" />
         </h5>
         <p class="body14">
            <spring:theme code="account.confirmation.forgotten.password.link.sent.first"/>
            &nbsp;
            <c:out value="${userEmail}"/>
            &nbsp;
            <spring:theme code="account.confirmation.forgotten.password.link.sent.second"/>
         </p>
         <p class="body14 text-center mb-0 mt-4">
            <a class="js-login-popup" href="#signIn" data-link="<c:url value='/login/loginpopup'/>" data-bs-dismiss="modal">
               <spring:theme code="login.login" />
            </a>
            </p>
      </div>
   </div>
</div>
