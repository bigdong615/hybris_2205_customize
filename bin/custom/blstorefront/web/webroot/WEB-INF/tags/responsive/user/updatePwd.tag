<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement" %>

<spring:htmlEscape defaultHtmlEscape="true"/>

<section id="myAccount">
        <div class="container">
            <div class="row justify-content-center">
                <div id="accountContent" class="col-lg-8 justify-content-center">
                    <h1><spring:theme code="text.account.profile.resetPassword"/></h1>
                    <hr>
                    <spring:theme code="text.account.profile.resetPassword.note"/>
                    <br><br>

                    <div class="row justify-content-center">
                        <div class="col-lg-8 justify-content-center">
                            <div class="notification notification-warning mt-4"><spring:theme code="text.account.profile.resetPassword.securityTips"/></div>
                            <form:form method="post" modelAttribute="blUpdatePwdForm" class="my-4">
                           <formElement:formPasswordBox idKey="password" labelKey="" path="pwd"
                                                                placeholder="updatePwd.pwd" inputCSS="form-control mb-3" mandatory="true"/>
                            <formElement:formPasswordBox idKey="" labelKey="updatePwd.checkPwd"
                                                      placeholder="updatePwd.checkPwd"    path="checkPwd" inputCSS="form-control" mandatory="true"/>

                                <div class="text-end mt-3">
                                    <button class="btn btn-primary"><spring:theme code="updatePwd.submit"/></button>
                                </div>
                            </form:form>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </section>
