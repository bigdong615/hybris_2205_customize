<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<spring:htmlEscape defaultHtmlEscape="true" />





 <div id="accountContent" class="col-lg-8 offset-lg-1">
                    <h3><spring:theme code="text.account.profile.updatePersonalDetails"/></h3>
                    <hr>
                    <div class="row">
                        <div class="col-lg-7">
                             <form:form action="${action}" method="post" modelAttribute="updateProfileForm" class="my-6">

                       					<formElement:formInputBox idKey="fullName"  path="fullName" inputCSS="form-control mb-3 ${errormsgvalid}" placeholder="Name" mandatory="true" />
                       					<!-- <formElement:formInputBox idKey="lastName" path="lastName" inputCSS="form-control mt-3" placeholder="name" mandatory="true" /> -->
                       					<formElement:formInputBox idKey="email"  path="email" inputCSS="form-control mt-3 ${errormsgvalidates}" placeholder="email" mandatory="true" disabled="true"/>

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