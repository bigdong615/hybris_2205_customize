<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:set var="noBorder" value=""/>
<c:if test="${not empty addressData}">
    <c:set var="noBorder" value="no-border"/>
</c:if>

<div id="accountContent" class="col-lg-8 offset-lg-1">
<h1>
   <spring:theme code="text.address.header"/>
</h1>
<hr>
<c:if test="${empty addressData}">
<div class="notification no-orders">
    <p><strong><spring:theme code="text.account.addressBook.noSavedAddresses"/></strong></p>
     <p><spring:theme code="text.account.addressBook.noSavedAddresses.paragraph.text1"/>&nbsp;<a href="add-address"><spring:theme code="text.account.addressBook.noSavedAddresses.addAddressButton"/></a>&nbsp;<spring:theme code="text.account.addressBook.noSavedAddresses.paragraph.text2"/></p>
   </div>
</c:if>
<c:if test="${not empty addressData}">
  <div class="notification notification-tip truck">
   <p>
      <spring:theme code="address.notification.banner.text"/>
   </p>
  </div>
 <div id="addressList" class="row mt-5">
      <c:forEach items="${addressData}" var="address">
         <div class="col-md-6 saved-address">
            <div class="card mb-4">
               <div class="badges">
                  <c:choose>
                     <c:when test="${address.defaultBillingAddress}">
                        <span class="badge badge-default float-start">
                           <spring:theme code="text.default.billing"/>
                        </span>
                     </c:when>
                     <c:otherwise>
                         <c:choose>
                         <c:when test="${address.upsStoreAddress eq 'true' || address.pickStoreAddress eq 'true' }">
                         <button class="badge badge-outline float-md-start">
                               <spring:theme code="text.setDefault.billing"/>
                         </button>
                         </c:when>
                         <c:when test="${address.billingAddress}">
                        <button class="badge badge-outline float-md-start js-set-default-address" value="set-default-billing-address/${address.id}">
                           <spring:theme code="text.setDefault.billing"/>
                        </button>
                         </c:when>
                         </c:choose>
                     </c:otherwise>
                  </c:choose>
                  <c:choose>
                     <c:when test="${address.defaultAddress}">
                        <span class="badge badge-default float-end">
                           <spring:theme code="text.default.shipping"/>
                        </span>
                     </c:when>
                     <c:when test="${address.shippingAddress}">
                        <button class="badge badge-outline float-md-end js-set-default-address" value="set-default-address/${address.id}">
                           <spring:theme code="text.setDefault"/>
                        </button>
                     </c:when>
                  </c:choose>
               </div>
               <div class="scroll-over-text">
               <c:if test="${not empty fn:escapeXml(address.companyName)}">
                  ${fn:escapeXml(address.companyName)}<br>
               </c:if>
               <b>${fn:escapeXml(address.firstName)}&nbsp;${fn:escapeXml(address.lastName)}</b>
               ${fn:escapeXml(address.line1)}<br>
               <c:if test="${not empty fn:escapeXml(address.line2)}">
                  ${fn:escapeXml(address.line2)}<br>
               </c:if>
               ${fn:escapeXml(address.town)},&nbsp;${fn:escapeXml(address.region.isocodeShort)}&nbsp;${fn:escapeXml(address.postalCode)}<br>
               <c:if test="${not empty address.email}" >
                  ${fn:escapeXml(address.email)}<br>
               </c:if>
               ${fn:escapeXml(address.phone)}
               </div>
               <p class="address-actions mb-0">
                  <a href="edit-address/${address.id}">
                     <spring:theme code="edit.address.button.text"/>
                  </a>
                  <a href="#" class="removeAddressFromBookButton" data-address-id="${address.id}" data-bs-toggle="modal" data-bs-target="#removeAddressWarning">
                     <spring:theme code="text.address.delete" />
                  </a>
               </p>
            </div>
         </div>
      </c:forEach>

   <ycommerce:testId code="addressBook_addNewAddress_button">
      <div class="col-12">
         <a href="add-address" class="btn btn-primary">
            <spring:theme code="text.account.addressBook.addAddress"/>
         </a>
      </div>
   </ycommerce:testId>
 </div>
 </c:if>
 </div>

<!-- Remove address modal-->
 <div class="modal fade" id="removeAddressWarning" tabindex="-1" aria-hidden="true">
    <div class="modal-dialog modal-dialog-centered modal-sm">
       <div class="modal-content">
          <div class="modal-header">
             <h5 class="modal-title">
                <spring:theme code="address.remove.popup.title.text"/>
             </h5>
             <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
          <div class="modal-body">
             <p class="body14">
                <spring:theme code="address.remove.popup.body.text"/>
             </p>
             <a href="#" id="removeAddressLink" class="btn btn-primary btn-block my-4">
                <spring:theme code="text.address.delete" />
             </a>
             <p class="text-center mb-0">
                <a href="#" class="lightteal" data-bs-dismiss="modal" aria-label="Close">
                   <spring:theme code="text.button.cancel"/>
                </a>
             </p>
          </div>
       </div>
    </div>
 </div>
 </div>