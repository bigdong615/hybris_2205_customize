<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:if test="${not empty passwordError}">
   <c:set var="errormsgvalid" value="error" />
</c:if>
<c:if test="${not empty currentPasswordError}">
  <c:set var="errormsgvalidates" value="error" />
</c:if>
 <div id="accountContent" class="col-lg-8 offset-lg-1">
                    <h3><spring:theme code="text.account.profile.updatePasswordForm"/></h3>
                    <hr>
                    <div class="row">
                        <div class="col-lg-7">
                             <form:form action="${action}" method="post" modelAttribute="updatePasswordForm" class="my-4">

                       					<formElement:formPasswordBox idKey="currentPassword"  path="currentPassword" inputCSS="form-control mb-3 ${errormsgvalid}" placeholder="Current Password" mandatory="true" />
                       					<formElement:formPasswordBox idKey="newPassword" path="newPassword" inputCSS="form-control mt-3" placeholder="New Password" mandatory="true" />
                       					<formElement:formPasswordBox idKey="checkNewPassword"  path="checkNewPassword" inputCSS="form-control mt-3 ${errormsgvalidates}" placeholder="Confirm New Password" mandatory="true" />

                       							<div class="accountActions text-end mt-3">
                       						     	<button type="button" class="btn btn-outline backToHome">
                                     				<spring:theme code="text.button.cancel" text="Cancel" />
                                     		</button>
                       								<button type="submit" class="btn btn-primary">
                       									<spring:theme code="updatePwd.submit" text="Update Password" />
                       								</button>
                       						  </div>
                       						  <c:if test="${not empty errorMsg}">
                                         <div class="notification notification-error mt-4">${errorMsg}</div>
                                    </c:if>
                       						  <c:if test="${not empty successMsg && empty errorMsg}"> 
                               								<div class="notification notification-tip check mt-4">${successMsg}</div>
                               			</c:if>
                       				</form:form>
                        </div>
                    </div>
  </div>

