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
    <div id="delivery-saved-addresses-dropdown">
        <b><spring:theme code="text.ship.it.saved.delivery.address"/></b>
        <div class="dropdown">
            <select id="ship-it-savedAddresses" class="form-control btn btn-block btn-outline text-start" onChange="onSavedAddressChange()">
                <option value="selectedValue" selected="selected" disabled="disabled">
                               <spring:theme code="text.enter.select.shipping.address"/>
                            </option>
                <c:forEach items="${deliveryAddresses}" var="deliveryAddress" varStatus="loop">              
                            <option value="${deliveryAddress.id}">
                                ${deliveryAddress.line1}, ${deliveryAddress.town}, ${deliveryAddress.country.isocode}, ${deliveryAddress.postalCode}
                            </option>
                </c:forEach>
                <option value="newAddress" id="#newAddress" style=" display: none;">
                    <spring:theme code="text.enter.new.shipping.address"/>
                </option>
            </select>
        </div>
        <a onClick="onAddNewAddressClicked()" class="gray80 newAddressAdd"><spring:theme code="text.add.new.shipping.address"/></a>
    </div>
</c:if>

<div id="delivery-shippingAddressFormDiv" class="mb-3">
    <b class="mt-4"><spring:theme code="text.existing.shipping.address"/></b>
    <div id="delivery-shippingAddressForm" class="mb-1">
        <form:form method="POST" modelAttribute="addressForm">
            <formElement:formInputBox idKey="address.firstName" placeholder="address.firstName" labelKey="" path="firstName" inputCSS="form-control" mandatory="true" />
            <formElement:formInputBox idKey="address.lastName" labelKey="" placeholder="address.lastName" path="lastName" inputCSS="form-control" mandatory="true" />
            <formElement:formInputBox idKey="address.companyName" labelKey="" placeholder="address.companyName" path="companyName" inputCSS="form-control" mandatory="false" />
            <formElement:formInputBox idKey="address.line1" labelKey="" placeholder="address.street1" path="line1" inputCSS="form-control" mandatory="true" />
            <formElement:formInputBox idKey="address.line2" labelKey="" placeholder="address.street2" path="line2" inputCSS="form-control" mandatory="false"/>
            <formElement:formInputBox idKey="address.townCity" labelKey="" placeholder="address.townCity" path="townCity" inputCSS="form-control" mandatory="true" />
            <formElement:formInputBox idKey="address.postcode" labelKey="" placeholder="address.zipcode1" path="postcode" inputCSS="form-control" mandatory="true" />
            <formElement:formSelectBox idKey="address.countryIso" labelKey="" path="countryIso" mandatory="true" skipBlank="false"
                skipBlankMessageKey="address.selectCountry2" items="${regions}" itemValue="isocodeShort" tabindex="${tabindex + 0}" selectCSSClass="form-control forcolor-change"/>
            <formElement:formInputBox idKey="address.email" labelKey="" placeholder="address.email" path="email" inputCSS="form-control" mandatory="false" />
            <formElement:formInputBox idKey="address.phone" labelKey="" placeholder="address.phone" path="phone" inputCSS="form-control" mandatory="false" />
        </form:form>
    </div>
</div>