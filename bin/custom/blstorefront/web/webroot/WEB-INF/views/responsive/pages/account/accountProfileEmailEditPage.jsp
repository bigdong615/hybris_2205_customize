<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:if test="${not empty passwordError}">
   <c:set var="errormsgvalid" value="error"/>
</c:if> 
<div id="accountContent" class="col-lg-8 offset-lg-1">
                    <h3><spring:theme code="text.account.update.email.address"/></h3>
                    <hr>
                    <div class="row">
                        <div class="col-lg-7">
                            <form:form action="update-email" method="post" modelAttribute="updateEmailForm" class="my-4 changeEmail-error">
                                <formElement:formInputBox idKey="profile.email" path="email" inputCSS="form-control mb-3" mandatory="true" placeholder="Current Email"/>
                                <formElement:formInputBox idKey="profile.checkEmail" path="chkEmail" inputCSS="form-control mb-3" mandatory="true" placeholder="New Email"/>
                                <formElement:formPasswordBox idKey="profile.pwd" path="password" inputCSS="form-control ${errormsgvalid}" mandatory="true" placeholder="Password"/>
                               <div class="form-actions text-end mt-3">
                                         	                  <ycommerce:testId code="email_cancelEmail_button">
                                               		              <button type="button" class="btn btn-outline backToHome">
                                               		              <spring:theme code="text.account.profile.cancel" />
                                               		              </button>
                                                             </ycommerce:testId>
                                         		                <ycommerce:testId code="email_saveEmail_button">
                                         					               <button type="submit" class="btn btn-primary js-change-email-validation">
                                         					               <spring:theme code="text.account.profile.saveUpdates" />
                                         					               </button>
                                         		                </ycommerce:testId>
                               </div>
                               <c:if test="${not empty errorMsg}">
                                <div class="notification notification-error mt-4">${errorMsg}</div>
                               </c:if>
                               <c:if test="${not empty successMsgEmail}">
                                 <div class="notification notification-tip check mt-4">${successMsgEmail}</div>
                               </c:if>
                            </form:form>
                        </div>
                    </div>
 </div>