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
        <p class="overline">Pay By Bl</p>
    </div>
    <div class="col-1 text-center">
        <button class="btn-checkbox" type="button" data-bs-toggle="collapse" data-bs-target="#bl-delivery-expand"
            aria-controls="ship-it-expand" aria-expanded="false">
            <input type="radio" id="bl-delivery" name="shipProduct" ><label for="bl-delivery"></label>
        </button>
    </div>
    <div class="col-11">
        <b>BL</b>
        <div class="collapse show" id="bl-delivery-expand" data-bs-parent="#shippingOptions">
            <div class="tab-container">

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
                        </div>
                        <div id="ship-it-pickup-person">
                            <b><spring:theme code="text.ship.it.pick.up.section.form.header"/></b>
                            <checkout:pickUpAddressForm />
                        </div>
                    </div> --%>
                </div>
                <div id="ship-it-notification"></div>
                <div id="ship-it-am-notification"></div>
            </div>
        </div>
    </div>
</div>