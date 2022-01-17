<%@ attribute name="regions" required="true" type="java.util.List"%>
<%@ attribute name="country" required="false" type="java.lang.String"%>
<%@ attribute name="tabindex" required="false" type="java.lang.Integer"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="theme" tagdir="/WEB-INF/tags/shared/theme" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>






     
		<formElement:formInputBox idKey="address.firstName" placeholder="address.firstName" labelKey="" path="billTo_firstName" inputCSS="form-control" mandatory="true" />
		<formElement:formInputBox idKey="address.lastName" labelKey="" placeholder="address.lastName" path="billTo_lastName" inputCSS="form-control" mandatory="true" />
		<input type="text" name="address.companyName" id="address.companyName" class="form-control" placeholder='<spring:theme code="address.companyName" />'/>
		<formElement:formInputBox idKey="address.line1" labelKey="" placeholder="address.street1" path="billTo_street1" inputCSS="form-control" mandatory="true" />
		<formElement:formInputBox idKey="address.line2" labelKey="" placeholder="address.street2" path="billTo_street2" inputCSS="form-control" mandatory="false"/>
		<formElement:formInputBox idKey="address.townCity" labelKey="" placeholder="address.townCity" path="billTo_city" inputCSS="form-control" mandatory="true" />
		<formElement:formInputBox idKey="address.postcode" labelKey="" placeholder="address.zipcode1" path="billTo_postalCode" inputCSS="form-control float-start shortInput" mandatory="true" />
		<%--<formElement:formSelectBox idKey="address.countryIso" labelKey="" path="billTo_country" mandatory="true" skipBlank="false"
                                      skipBlankMessageKey="address.selectCountry2" items="${regions}" itemValue="isocodeShort" tabindex="${tabindex + 7}" selectCSSClass="form-control"/>--%>
		<formElement:formSelectBox idKey="address.region" labelKey="" path="billTo_state" mandatory="true" skipBlank="false" skipBlankMessageKey="billing.address.selectState" items="${regions}" itemValue="isocode" tabindex="${tabindex + 7}" selectCSSClass="form-control selectBoxSize" divCSS="select-wrapper float-end selectBoxDivWidth"/>
		<%-- <formElement:formSelectBox idKey="address.region" labelKey="address.state" path="billTo_state" mandatory="true" skipBlank="false"  skipBlankMessageKey="address.selectState" items="${regions}" itemValue="isocodeShort"  selectCSSClass="form-control"/>--%>
		<formElement:formInputBox idKey="address.email" labelKey="" placeholder="address.email" path="billTo_email" inputCSS="form-control" mandatory="false" />
		<formElement:formInputBox idKey="address.phone" labelKey="" placeholder="address.phone" path="billTo_phoneNumber" inputCSS="form-control mb-3" mandatory="false" />
		<input type="checkbox" id="save-address"><label for="save-address"><span class="gray80">Save address</span></label>	