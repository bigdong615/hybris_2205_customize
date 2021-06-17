<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="address" tagdir="/WEB-INF/tags/responsive/address"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<spring:url value="/my-account/address-book" var="addressBookUrl" htmlEscape="false" />
<c:choose>
	<c:when test="${edit eq true }">
        <c:set var="headline"><spring:theme code="text.account.addressBook.updateAddress" /></c:set>
	</c:when>
	<c:otherwise>
        <c:set var="headline"><spring:theme code="text.account.addressBook.addAddress" /></c:set>
	</c:otherwise>
</c:choose>

<div id="accountContent" class="col-lg-8 offset-lg-1 accountAddressAdd">
   <h1><spring:theme code="text.address.header"/></h1>
   <hr>
   <div class="row">
      <div class="col-lg-7">
         <b>${headline}</b>
         <form:form id="editAddress" class="my-4" method="post" modelAttribute="addressForm">
            <formElement:formInputBox idKey="address.firstName" labelKey="address.firstName" path="firstName" inputCSS="form-control mb-3" mandatory="true" placeholder="address.firstName" />
            <formElement:formInputBox idKey="address.surname" labelKey="address.surname" path="lastName" inputCSS="form-control mb-3" mandatory="true" placeholder="address.surname" />
            <formElement:formInputBox idKey="address.companyName"
               labelKey="address.companyName" path="companyName"
               inputCSS="form-control mb-3" mandatory="true" placeholder="address.companyName"/>
            <formElement:formInputBox idKey="address.line1" labelKey="address.line1" path="line1" inputCSS="form-control mb-3" mandatory="true" placeholder="address.line1"/>
            <formElement:formInputBox idKey="address.line2" labelKey="address.line2" path="line2" inputCSS="form-control mb-3" mandatory="false" placeholder="address.line2"/>
            <formElement:formInputBox idKey="address.townCity" labelKey="address.townCity" path="townCity" inputCSS="form-control mb-3" mandatory="true" placeholder="address.townCity"/>
            <formElement:formInputBox idKey="zip" labelKey="address.postalcode" path="postcode" inputCSS="form-control mb-3 float-start" mandatory="true"  placeholder="address.postalcode" styleCSS="width: calc(40% - 15px);"/>
            <div class="select-wrapper float-end mb-3" style="width: 60%;">
               <formElement:formSelectBox idKey="state" labelKey="" path="regionIso" mandatory="true" skipBlank="false" styleCSS="width:100%;"
                  skipBlankMessageKey="address.state" items="${regions}" itemValue="${useShortRegionIso ? 'isocodeShort' : 'isocode'}" selectedValue="${addressForm.regionIso}" selectCSSClass="form-control"/>
            </div>
            <formElement:formInputBox idKey="address.email" labelKey="" placeholder="address.email" path="email" inputCSS="form-control mb-3" mandatory="false" />
            <formElement:formInputBox idKey="address.phone" labelKey="" placeholder="address.phone" path="phone" inputCSS="form-control mb-3" mandatory="false" />
            <input type="checkbox" id="default-billing-address" name="defaultBillingAddress" checked><label for="default-billing-address"><span class="gray80"><spring:theme code="text.default.billing"/></span></label>
            <input type="checkbox" id="default-shipping-address" name="defaultAddress" checked><label for="default-shipping-address"><span class="gray80"><spring:theme code="text.default.shipping"/></span></label>
            <div class="text-end mt-3">
               <button class="btn btn-outline"><spring:theme code="text.button.cancel" /></button>
               <button class="btn btn-primary"><spring:theme code="text.button.save" /></button>
            </div>
         </form:form>
      </div>
   </div>
</div>
