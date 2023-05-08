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
   <h3>
      <spring:theme code="text.address.header"/>
   </h3>
   <hr>
   <div class="row">
      <div class="col-lg-7">
         <b>${headline}</b>
         <form:form id="editAddress" class="my-4" method="post" modelAttribute="addressForm">
            <form:hidden path="addressId" class=""
               status="${not empty suggestedAddresses ? 'hasSuggestedAddresses' : ''}" />
            <formElement:formInputBox idKey="address.firstName" labelKey="" path="firstName" inputCSS="form-control mb-3" mandatory="true" placeholder="address.firstName" />
            <formElement:formInputBox idKey="address.surname" labelKey="" path="lastName" inputCSS="form-control mb-3" mandatory="true" placeholder="address.surname" />
            <formElement:formInputBox idKey="address.companyName"
               labelKey="" path="companyName"
               inputCSS="form-control mb-3" mandatory="false" placeholder="address.companyName"/>
            <formElement:formInputBox idKey="address.line1" labelKey="" path="line1" inputCSS="form-control mb-3" mandatory="true" placeholder="address.line1"/>
            <formElement:formInputBox idKey="address.line2" labelKey="" path="line2" inputCSS="form-control mb-3" mandatory="false" placeholder="address.line2"/>
            <formElement:formInputBox idKey="address.townCity" labelKey="" path="townCity" inputCSS="form-control mb-3" mandatory="true" placeholder="address.townCity"/>
<%--              <formElement:formInputBox idKey="zip" labelKey="" path="postcode" inputCSS="form-control mb-3 float-start" mandatory="true"  placeholder="address.postalcode" styleCSS="width: calc(40% - 15px);"/> 
 --%>          
             <div class="form-group">
            <form:input id="zip" name="postcode" class="form-control mb-3 float-start" style="width: calc(40% - 15px);" placeholder="Zip" type="number" path="postcode" value="" onKeyPress="if(this.value.length==5) return false;" />
            </div>
            
            <div class="select-wrapper float-end mb-3" style="width: 60%;">
               <formElement:formSelectBox idKey="state" labelKey="" path="regionIso" mandatory="true" skipBlank="false" styleCSS="width:100%;"
                  skipBlankMessageKey="address.state" items="${regions}" itemValue="${useShortRegionIso ? 'isocodeShort' : 'isocode'}"  selectCSSClass="form-control"/>
            </div>
            <formElement:formInputBox idKey="address.email" labelKey="" placeholder="address.email" path="email" inputCSS="form-control mb-3" mandatory="true" />
           <%--  <formElement:formInputBox idKey="address.phone" labelKey="" placeholder="address.phone" path="phone" inputCSS="form-control mb-3" mandatory="true" /> --%>
             <div class="form-group">
            <form:input id="address.phone" name="phone" class="form-control mb-3"  placeholder="Phone Number" type="text" path="phone" inputCSS="form-control" mandatory="false" maxlength="16" />
            </div> 
            <input type="radio" checked="true" id="default-billing-address" name="billingAddress" ${addressForm.billingAddress eq 'true' ? 'checked':''} >
            <%-- <label for="default-billing-address">
               <span class="gray80">
                  <spring:theme code="text.default.billing"/>
               </span>
            </label> --%>
            <input hidden type="radio" checked="true" id="default-shipping-address" name="shippingAddress" ${addressForm.shippingAddress eq 'true' ? 'checked':''} >
            <%-- <label for="default-shipping-address">
               <span class="gray80">
                  <spring:theme code="text.default.shipping"/>
               </span>
            </label> --%>
            <div class="text-end mt-3">
               <c:url var="addressBook" value="/my-account/address-book"/>
               <a href="${addressBook}" class="btn btn-outline">
                  <spring:theme code="text.button.cancel" />
               </a>
               <button class="btn btn-primary js-validate-address-form-data" >
                  <spring:theme code="text.button.save" />
               </button>
            </div>
         </form:form>
         <div id="js-add-address-length-Validation" class=""></div>
         <div id="js-add-address-Validation" class=""></div>
         <div id="js-default-address-Validation" class=""></div>
      </div>
   </div>
</div>
