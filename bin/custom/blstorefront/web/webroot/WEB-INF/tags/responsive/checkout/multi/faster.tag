<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="checkout" tagdir="/WEB-INF/tags/responsive/checkout/multi" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<div class="row">
    <div class="col-12">
        <p class="overline"><spring:theme code="text.checkout.multi.order.delivery.faster"/></p>
    </div>
    <div class="col-1 text-center">
        <button class="btn-checkbox" type="button" data-bs-toggle="collapse" data-bs-target="#pickup-expand"
            aria-controls="pickup-expand" aria-expanded="false" onClick="$('#validationMessage').empty();">
            <input type="radio" id="pickup" name="shipProduct"><label for="pickup"></label>
        </button>
    </div>
    <div class="col-11">
        <b><spring:theme code="text.pick.up.section.header"/></b>
        <span class="gray80"><spring:theme code="text.pick.up.section.pick.up.msg"/></span>
        <div class="collapse" id="pickup-expand" data-bs-parent="#shippingOptions">
            <div class="dropdown my-4">
               <c:if test="${not empty partnerPickUpLocation}">
                  <select id="pick-up-select-box" class="btn btn-block btn-outline text-start my-4" onChange="onSelectPartnerPickup(this)">
                      <option selected disabled><spring:theme code="text.pick.up.section.pick.up.default.option"/></option>
                      <c:forEach items="${partnerPickUpLocation}" var="entry" varStatus="loop">
                          <option value="${entry.code}"> ${entry.name} </option>
                      </c:forEach>
                  </select>
               </c:if>
            </div>
            <div id="partnerPickUpShippingMethods">

            </div>
            <!-- <div id="pick-up-pickup-gear">
                <b class="mt-4"><spring:theme code="text.ship.it.pick.up.section"/></b>
                <div id="pickup-person" class="row mt-2 mb-4">
                    <div class="col-3">
                        <button class="btn-checkbox" onClick="pickUpByMeClick()">
                            <input type="radio" id="pickup-me" name="pickup-person" checked>
                            <label for="pickup-me">
                                <span class="gray80"><spring:theme code="text.ship.it.pick.up.section.i.am"/></span>
                            </label>
                        </button>
                    </div>
                    <div class="col-9">
                        <button class="btn-checkbox" onClick="pickUpBySomeoneForm()">
                            <input type="radio" id="pickup-other" name="pickup-person">
                            <label for="pickup-other">
                                <span class="gray80"><spring:theme code="text.ship.it.pick.up.section.someone.else"/></span>
                            </label>
                        </button>
                    </div>
                </div>
            </div> -->
            <div id="store-pickup-person">
                <b><spring:theme code="text.ship.it.pick.up.section.form.header"/></b>
                <checkout:pickUpAddressForm />
            </div>
            <div id="pick-up-notification-faster"></div>
        </div>
    </div>
</div>