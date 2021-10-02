<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="checkout" tagdir="/WEB-INF/tags/responsive/checkout/multi" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<div class="row">
    <div class="col-12">
        <p class="overline"><spring:theme code="text.checkout.multi.order.delivery.fast"/></p>
    </div>
    <div class="col-1 text-center">
        <button class="btn-checkbox" type="button" data-bs-toggle="collapse" data-bs-target="#ship-it-expand"
            aria-controls="ship-it-expand" aria-expanded="false" onClick="$('#validationMessage').empty();">
            <input type="radio" id="ship-it" name="shipProduct" ><label for="ship-it"></label>
        </button>
    </div>
    <div class="col-11">
        <b><spring:theme code="text.checkout.multi.order.delivery.fast.ship.it"/></b>
        <span class="gray80"><spring:theme code="text.checkout.multi.order.delivery.fast.ship.it.msg"/></span>
        <div class="collapse show" id="ship-it-expand" data-bs-parent="#shippingOptions">
            <div class="tab-container">
                <div class="tab-navigation">
                    <select id="ship-it-select-box" class="btn btn-block btn-outline text-start my-4" onChange="onChangeOfShipItShippingMethod()">
                        <c:forEach items="${shippingGroup}" var="entry" varStatus="loop">
                            <c:if test="${entry.shippingType eq 'FAST'}">
                                <c:choose>
                                    <c:when test="${entry.defaultShippingGroup}">
                                        <option value="${entry.code}" selected="selected"> ${entry.name} </option>
                                    </c:when>
                                    <c:otherwise>
                                        <option value="${entry.code}"> ${entry.name} </option>
                                    </c:otherwise>
                                </c:choose>
                            </c:if>
                        </c:forEach>
                    </select>
                </div>
                <div id="tab-SHIP_HOME_HOTEL_BUSINESS" class="ship-it-tab-content">
                    <checkout:addressForm />
                    <div id="ship-it-save-address-div" class="mb-5">
                        <input type="checkbox" id="ship-it-save-address" checked>
                        <label for="ship-it-save-address">
                            <span class="gray80"><spring:theme code="text.add.new.shipping.save.address"/></span>
                        </label>
                    </div>
                    <b class="mt-4"><spring:theme code="text.ship.it.available.delivery.methods"/></b>
                    <div id="shipToHomeShippingMethods" class="sub-option"></div>
                </div>
                <div id="tab-SHIP_UPS_OFFICE" class="ship-it-tab-content">
                    <div id="shipToUPSShippingMethods" class="sub-option">

                    </div>
                    <div id="checkZipForUPSPickup" style="display:none;">
                        <b class="mt-4"><spring:theme code="text.ship.it.ups.find.nearest.location"/></b>
                        <div class="input-group mt-2 mb-5">
                          <input id="ship-it-ups-zip-code" type="text" class="form-control" placeholder="Zip code">
                          <div class="input-group-append">
                            <button class="btn btn-secondary" type="button" onClick="onClickOfFindStore()">
                                <spring:theme code="text.ship.it.ups.find.store"/>
                            </button>
                          </div>
                        </div>
                    </div>
                    <div id="ship-it-SHIP_UPS_OFFICE">

                    </div>
                    <div id="changeUPSStoreButton" class="text-end mb-4">
                        <button type="button" class="btn btn-outline" onClick="changeUPSStore()">
                            <spring:theme code="text.ship.it.ups.change.store"/>
                        </button>
                    </div>
                    <%-- <div id="ship-it-pickup-gear">
                        <b class="mt-4"><spring:theme code="text.ship.it.pick.up.section"/></b>
                        <div id="pickup-person" class="row mt-2 mb-4">
                            <div class="col-3">
                                <button class="btn-checkbox" onClick="showPickUpByMeClick()">
                                    <input type="radio" id="store-pickup-me" name="store-pickup-person" checked="">
                                    <label for="store-pickup-me">
                                        <span class="gray80"><spring:theme code="text.ship.it.pick.up.section.i.am"/></span>
                                    </label>
                                </button>
                            </div>
                            <div class="col-9">
                                <button class="btn-checkbox" onClick="showPickUpBySomeoneForm()">
                                    <input type="radio" id="store-pickup-other" name="store-pickup-person">
                                    <label for="store-pickup-other">
                                        <span class="gray80"><spring:theme code="text.ship.it.pick.up.section.someone.else"/></span>
                                    </label>
                                </button>
                            </div>
                        </div> --%>
                        <div id="ship-it-pickup-person">
                            <b><spring:theme code="text.ship.it.pick.up.section.form.header"/></b>
                            <checkout:pickUpAddressForm />
                        </div>
                    </div>
                </div>
                <div id="ship-it-notification"></div>
                <div id="ship-it-am-notification"></div>
            </div>
        </div>
    </div>
</div>