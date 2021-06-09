<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="theme" tagdir="/WEB-INF/tags/shared/theme" %>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/desktop/nav" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="common" tagdir="/WEB-INF/tags/desktop/common" %>
<%@ taglib prefix="formElement" tagdir="/WEB-INF/tags/desktop/formElement" %>
<div class="span-20 append-1">

<div class="account-section-content">
   <div class="back-link border">
   <c:url value="/my-account/payment-details"  var="accountPaymentMethodUrl" />
      <a class="addressBackBtn" href="${accountPaymentMethodUrl}">
            <span class="glyphicon glyphicon-chevron-left"></span>
         </a>
      <span class="label"><spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.editPaymentMethod" /></span>
   </div>
</div>
</div>

   <c:if test="${not empty ccPaymentInfo}">
      <div class="account-section">
         <form:form id="braintree-payment-form" action="${request.contextPath}/my-account/edit-payment-method" method="POST">
            <input type="hidden" name="paymentInfoId" id="paymentInfoId" value="${selectedPaymentMethodId}"/>
            <c:if test="${ccPaymentInfo.cardType!=null}">
               <div style="float: right;">
                  <spring:theme code="form.required" text="Fields marked * are required"/>
               </div>
               <br>
               <div class="account-section-content  account-section-content-small">
                  <div class="control-group cardForm" style="dispaly: none;" id="braintree-payment-edit-form">
                     <label for="cardholderName" class="control-label ">
                        <spring:theme code="braintree.text.cc.cardholder" />
                     </label>
                     <div class="controls" >
                        <c:choose>
                           <c:when test="${not empty cardholder}">
                              <input id="cardholderName" name="cardholder"  value="${cardholder}" placeholder="Cardholder Name" maxlength="175"/>
                           </c:when>
                           <c:otherwise>
                              <input id="cardholderName" name="cardholder"  value="${ccPaymentInfo.cardholderName}" placeholder="Cardholder Name" maxlength="175"/>
                           </c:otherwise>
                        </c:choose>
                     </div>
                     <label for="number" class="control-label ">
                        <spring:theme code="braintree.text.cc.number"/>
                     </label>
                     <div class="controls" >
                        <input id="number" value="${ccPaymentInfo.cardNumber}" readonly/>
                     </div>
                     <label for="expiration-date" class="control-label ">
                        <spring:theme code="braintree.text.cc.expiration.date"/>
                        *
                     </label>
                     <div class="controls" >
                        <c:choose>
                           <c:when test="${not empty expirationDate}">
                              <input id="expiration-date"  name="expirationDate" value="${expirationDate}" placeholder="MM/YYYY" />
                           </c:when>
                           <c:otherwise>
                              <input id="expiration-date"  name="expirationDate" value="${ccPaymentInfo.expiryMonth}/${ccPaymentInfo.expiryYear}" placeholder="MM/YYYY" />
                           </c:otherwise>
                        </c:choose>
                     </div>
                     <label for="cvv" class="control-label ">
                        <spring:theme code="braintree.text.cc.cvv" />
                     </label>
                     <div class="" >
                        <input id="cvv" name="cvv" value="" placeholder="3 or 4 digit" type="password" maxlength="4"/>
                     </div>
                  </div>
               </div>
            </c:if >
            <br>
            <div class="account-section-header">
               <spring:theme code="checkout.multi.paymentMethod.addPaymentDetails.billingAddress" />
            </div>
            <div id="address-item">
               <c:if test="${ccPaymentInfo.billingAddress!=null}">
                  <input type="hidden" name="billingAddressId" id="billingAddressId" value="${paymentInfo.billingAddress.id}"/>
                  <br/><span id="title">${fn:escapeXml(ccPaymentInfo.billingAddress.title)}</span>&nbsp;<span id="firstName">${fn:escapeXml(ccPaymentInfo.billingAddress.firstName)}</span>&nbsp;<span id="lastName">${fn:escapeXml(ccPaymentInfo.billingAddress.lastName)}</span>
                  <br/><span id="line1">${fn:escapeXml(ccPaymentInfo.billingAddress.line1)}</span>
                  <br/><span id="line2">${fn:escapeXml(ccPaymentInfo.billingAddress.line2)}</span>
                  <br/><span id="town">${fn:escapeXml(ccPaymentInfo.billingAddress.town)}&nbsp;</span> <span id="postalCode">${fn:escapeXml(ccPaymentInfo.billingAddress.postalCode)}</span>
                  <br/><span id="country-name">${fn:escapeXml(ccPaymentInfo.billingAddress.country.name)}</span>
                  <br/><span id="region-name">${fn:escapeXml(ccPaymentInfo.billingAddress.region.name)}</span>
               </c:if>
            </div>
            <br/>
            <c:if test="${not empty deliveryAddresses}">
               <button id="viewAddressBook" class="btn btn-primary js-address-book" type="button">
                  <spring:theme code="account.payment.selectOther" text="Select new Address"/>
               </button>
            </c:if>
            <br/>
            <div class="account-section-content">
               <div class="col-xs-12 col-sm-5 col-md-4 col-lg-3 col-sm-push-5 col-sm-offset-2 col-md-push-4 col-lg-push-3 col-md-offset-4 col-lg-offset-6">
                  <button class="btn btn-primary btn-block change_address_button show_processing_message" type="submit">
                     <spring:theme code="account.add.paymentMethod.save" text="Save" />
                  </button>
               </div>
               <div class="col-xs-12 col-sm-5 col-md-4 col-lg-3 col-sm-pull-5 col-md-pull-4 col-lg-pull-3">
                  <c:url value="/my-account/payment-details"  var="accountPaymentMethodUrl" />
                  <a class="btn btn-block btn-default" href="${accountPaymentMethodUrl}">
                     <spring:theme code="account.add.paymentMethod.cancel" text="Cancel" />
                  </a>
               </div>
            </div>
         </form:form>
         <div id="savedAddressListHolder" class="clear">
               <div id="addressbook">
                  <div class="headline">
                     <spring:theme code="account.payment.addressBook" text="Address Book"/>
                  </div>
                  <div class="addressList">
                     <c:forEach items="${deliveryAddresses}" var="deliveryAddress">
                        <div class="addressEntry">
                           <input type="hidden" name="selectedAddressCode" id="selectedAddressCode" value="${deliveryAddress.id}"/>
                           <br>${fn:escapeXml(deliveryAddress.title)}&nbsp; ${fn:escapeXml(deliveryAddress.firstName)}&nbsp; ${fn:escapeXml(deliveryAddress.lastName)}
                           <br>${fn:escapeXml(deliveryAddress.line1)}
                           <br>${fn:escapeXml(deliveryAddress.line2)}
                           <br>${fn:escapeXml(deliveryAddress.town)}&nbsp; ${fn:escapeXml(deliveryAddress.postalCode)}
                           <br>${fn:escapeXml(deliveryAddress.country.name)}
                           <c:if test="${not empty deliveryAddress.region.name}">&nbsp; ${fn:escapeXml(deliveryAddress.region.name)}</c:if>
                           <br>
                           <button id="resolveAddressButton" class="btn btn-primary btn-block" onclick="receiveNewAddressData(${deliveryAddress.id})">
                              <spring:theme code="account.payment.address.useThisAddress" text="Use this address"/>
                           </button>
                        </div>
                     </c:forEach>
                  </div>
               </div>
         </div>
      </div>
   </c:if>
</div>

<script>
   var editPaymentMethodsPage = "editPaymentMethodsPage";
</script>