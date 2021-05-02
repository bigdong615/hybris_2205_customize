<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/responsive/formElement" %>
<%@ taglib prefix="address" tagdir="/WEB-INF/tags/responsive/address"%>

<c:url value="/checkout/multi/delivery-address/add" var="saveAddress" />

<spring:htmlEscape defaultHtmlEscape="true" />

<c:if test="${not empty deliveryAddresses}">
    <div id="ship-it-saved-addresses-dropdown">
        <b><spring:theme code="text.ship.it.saved.delivery.address"/></b>
        <div class="dropdown my-2">
            <select id="ship-it-savedAddresses" class="form-control btn btn-block btn-outline text-start my-4" onChange="onSavedAddressChange()">
                <c:forEach items="${deliveryAddresses}" var="deliveryAddress" varStatus="loop">
                    <option value="${deliveryAddress.id}">
                        ${deliveryAddress.line1}, ${deliveryAddress.town}, ${deliveryAddress.country.isocode}, ${deliveryAddress.postalCode}
                    </option>
                </c:forEach>
                <option value="addNewAddress" selected="selected" id="addNewAddress" style="display:none;">
                     Enter New Address in the Saved Shipping Address dropdown
                </option>
            </select>
        </div>
        <a onClick="onAddNewAddressClicked()" class="gray80"><spring:theme code="text.add.new.shipping.address"/></a>
    </div>
</c:if>

<div id="ship-it-shippingAddressFormDiv">
    <b class="mt-4"><spring:theme code="text.existing.shipping.address"/></b>
    <div id="ship-it-shippingAddressForm" class="mb-5">
        <form:form method="POST" modelAttribute="addressForm">
            <formElement:formInputBox idKey="address.firstName" placeholder="address.firstName" labelKey="" path="firstName" inputCSS="form-control" mandatory="true" />
            <formElement:formInputBox idKey="address.lastName" labelKey="" placeholder="address.lastName" path="lastName" inputCSS="form-control" mandatory="true" />
            <formElement:formInputBox idKey="address.line1" labelKey="" placeholder="address.line1" path="line1" inputCSS="form-control" mandatory="true" />
            <formElement:formInputBox idKey="address.line2" labelKey="" placeholder="address.line2" path="line2" inputCSS="form-control" mandatory="false"/>
            <formElement:formInputBox idKey="address.townCity" labelKey="" placeholder="address.townCity" path="townCity" inputCSS="form-control" mandatory="true" />
            <formElement:formInputBox idKey="address.postcode" labelKey="" placeholder="address.postcode" path="postcode" inputCSS="form-control" mandatory="true" />
            <formElement:formSelectBox idKey="address.countryIso" labelKey="" path="countryIso" mandatory="true" skipBlank="false"
                skipBlankMessageKey="address.selectCountry" items="${regions}" itemValue="isocodeShort" tabindex="${tabindex + 7}" selectCSSClass="form-control"/>
            <formElement:formSelectBox idKey="address.addressType" labelKey="" path="addressType" mandatory="true" skipBlank="false"
                skipBlankMessageKey="address.addressType" items="${addressTypes}" itemLabel="code" itemValue="" tabindex="${tabindex + 7}" selectCSSClass="form-control"/>
            <formElement:formInputBox idKey="address.email" labelKey="" placeholder="address.email" path="email" inputCSS="form-control" mandatory="false" />
            <formElement:formInputBox idKey="address.phone" labelKey="" placeholder="address.phone" path="phone" inputCSS="form-control" mandatory="false" />
            <input type="checkbox" id="save-address-checkbox"><label for="save-address"><span class="gray80">Save address</span></label>
        </form:form>
    </div>
</div>