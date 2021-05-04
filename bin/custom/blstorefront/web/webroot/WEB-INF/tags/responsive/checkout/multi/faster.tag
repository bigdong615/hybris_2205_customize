<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<div class="row">
    <div class="col-12">
        <p class="overline">Faster</p>
    </div>
    <div class="col-1 text-center">
        <button class="btn-checkbox" type="button" data-bs-toggle="collapse" data-bs-target="#pickup-expand"
            aria-controls="pickup-expand" aria-expanded="false">
            <input type="radio" id="pickup" name="shipProduct"><label for="pickup"></label>
        </button>
    </div>
    <div class="col-11">
        <b>Pickup</b>
        <span class="gray80">Grab your gear at either a BL or partner location</span>
        <div class="collapse" id="pickup-expand" data-bs-parent="#shippingOptions">
        <div class="dropdown my-4">
           <c:if test="${not empty partnerPickUpLocation}">
              <button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="pickupArea"
                data-bs-toggle="dropdown" aria-expanded="false">
                SF Bay Area
                <span class="float-end">NEED To ASK?</span>
              </button>
              <ul class="dropdown-menu" aria-labelledby="pickupArea">
                <c:forEach items="${partnerPickUpLocation}" var="partnerPickUp" varStatus="loop">
                    <li>
                        <c:if test="${loop.index eq 0}">

                        </c:if>
                        <button class="dropdown-item" id="partnerPickUpLocation-${partnerPickUp.code}"
                            onClick="onSelectPartnerPickup(this)" name="${partnerPickUp.code}">
                            ${partnerPickUp.name}
                            <span class="float-end">FREE</span>
                        </button>
                     </li>
                </c:forEach>
              </ul>
           </c:if>
        </div>
        <div id="pickUpLocations">
            <div id="pickup-nyc" class="collapse row store-location mb-3" data-bs-parent="#pickup-expand">
                <div class="col-1">
                    <input type="radio" id="pu-location-1" name="pickup-locations"><label for="pu-location-1"></label>
                </div>
                <div class="col-11">
                    <p>BorrowLenses Office<br>
                    <a href="#" target="_blank">1234 Main St. New York City, NY 12345</a><br>
                    555-456-7894</p>
                    <p class="mb-0 mt-3"><span class="gray80">M-F</span>&emsp;9am-5pm</p>
                    <p class="mb-0"><span class="gray80">Sat</span>&emsp;10:30am-2pm</p>
                    <p class="mb-0"><span class="gray80">Sun</span>&emsp;CLOSED</p>
                </div>
            </div>

        </div>
        <b class="mt-4">Who's Picking Up the Gear?</b>
        <div id="pickup-person" class="row mt-2 mb-4">
            <div class="col-3"><button class="btn-checkbox" data-bs-toggle="collapse" data-bs-target="#store-pickup-person" aria-controls="store-pickup-person" aria-expanded="false"><input type="radio" id="pickup-me" name="pickup-person" checked><label for="pickup-me"><span class="gray80">I am</span></label></button></div>
            <div class="col-9"><button class="btn-checkbox" data-bs-toggle="collapse" data-bs-target="#store-pickup-person" aria-controls="store-pickup-person" aria-expanded="false"><input type="radio" id="pickup-other" name="pickup-person"><label for="pickup-other"><span class="gray80">Someone else</span></label></button></div>
        </div>
        <div class="collapse" id="store-pickup-person">
            <b>Pickup Person</b>
            <div class="mt-2 mb-4">
                 <input type="text" class="form-control error" id="pickup-first-name" placeholder="First Name">
                <input type="text" class="form-control" id="pickup-last-name" placeholder="Last Name">
                <input type="text" class="form-control" id="pickup-email" placeholder="Email">
                <input type="text" class="form-control" id="pickup-phone" placeholder="Phone Number">
            </div>
            <div class="notification notification-warning">They must show ID at time of pickup</div>
        </div>
        </div>
    </div>
</div>