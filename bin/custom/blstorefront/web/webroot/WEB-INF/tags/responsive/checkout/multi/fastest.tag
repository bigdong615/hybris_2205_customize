<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>

<spring:htmlEscape defaultHtmlEscape="true" />
<div class="row">
    <div class="col-12">
        <p class="overline">Fastest</p>
    </div>
    <div class="col-1 text-center">
        <button class="btn-checkbox" type="button" data-bs-toggle="collapse" data-bs-target="#sameday-expand" aria-controls="sameday-expand" aria-expanded="false">
        <input type="radio" id="sameday" name="shipProduct"><label for="sameday"></label>
        </button>
    </div>
    <div class="col-11">
        <b>Same Day Delivery</b>
        <span class="gray80">SF & NYC by courier</span>
        <div class="collapse" id="sameday-expand" data-bs-parent="#shippingOptions">
        <div class="dropdown mt-4 mb-2">
          <button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="deliveryTo" data-bs-toggle="dropdown" aria-expanded="false">
            SF Bay Area
          </button>
          <ul class="dropdown-menu" aria-labelledby="deliveryTo">
            <li><button class="dropdown-item">SF Bay Area</button></li>
            <li><button class="dropdown-item">NYC Metro Area</button></li>
          </ul>
        </div>
        <span class="gray80">Delivery to your door with <a href="#" class="gray80">Fedex return shipping</a>.</span>
        <b class="mt-4">Check Eligibility</b>
        <div class="input-group mt-2 mb-4">
          <input type="text" class="form-control" placeholder="Zip code">
          <div class="input-group-append">
            <button class="btn btn-secondary" type="button">Check</button>
          </div>
        </div>
        <b>Delivery Window</b>
        <div class="dropdown mt-2 mb-4">
          <button class="btn btn-block btn-outline dropdown-toggle text-start" role="button" id="deliveryTime" data-bs-toggle="dropdown" aria-expanded="false">
            3:00pm - 6:00pm <span class="float-end">$99.99</span>
          </button>
          <ul class="dropdown-menu" aria-labelledby="deliveryTime">
            <li><button class="dropdown-item">11:00am - 2:00pm <span class="float-end">$99.99</span></button></li>
            <li><button class="dropdown-item">3:00pm - 6:00pm <span class="float-end">$99.99</span></button></li>
          </ul>
        </div>
        <div class="notification notification-warning mb-4">
            A few important notes about your delivery:
            <ul>
                <li>Your gear will be delivered to your door.</li>
                <li>A signature is required.</li>
                <li>Check your email: you may need to verify or add details to your order.</li>
                <li>Delivery will be with FedEx SameDay city.</li>
            </ul>
        </div>
        <b>Add Delivery Notes</b>
        <textarea class="form-control mt-2 mb-4" placeholder="Optional (i.e. deliver to back door.)"></textarea>
        <b>Shipping Address</b>
        <div class="mb-5">

            <input type="text" class="form-control" id="first-name" placeholder="First Name">
            <input type="text" class="form-control" id="last-name" placeholder="Last Name">
            <input type="text" class="form-control" id="street" placeholder="Street Address">
            <input type="text" class="form-control" id="street-2" placeholder="Apt., Suite, Floor">
            <input type="text" class="form-control" id="city" placeholder="City">
            <input type="text" class="form-control float-start" id="zip" placeholder="Zip" style="width: calc(40% - 15px);">
            <div class="select-wrapper float-end" style="width: 60%;">
            <select class="form-control" id="state" style="width:100%;">
                <option value="" disabled selected>State</option>
                <option value="AL">Alabama</option>
                <option value="AK">Alaska</option>
                <option value="AZ">Arizona</option>
                <option value="AR">Arkansas</option>
                <option value="CA">California</option>
                <option value="CO">Colorado</option>
                <option value="CT">Connecticut</option>
                <option value="DE">Delaware</option>
                <option value="DC">District Of Columbia</option>
                <option value="FL">Florida</option>
                <option value="GA">Georgia</option>
                <option value="HI">Hawaii</option>
                <option value="ID">Idaho</option>
                <option value="IL">Illinois</option>
                <option value="IN">Indiana</option>
                <option value="IA">Iowa</option>
                <option value="KS">Kansas</option>
                <option value="KY">Kentucky</option>
                <option value="LA">Louisiana</option>
                <option value="ME">Maine</option>
                <option value="MD">Maryland</option>
                <option value="MA">Massachusetts</option>
                <option value="MI">Michigan</option>
                <option value="MN">Minnesota</option>
                <option value="MS">Mississippi</option>
                <option value="MO">Missouri</option>
                <option value="MT">Montana</option>
                <option value="NE">Nebraska</option>
                <option value="NV">Nevada</option>
                <option value="NH">New Hampshire</option>
                <option value="NJ">New Jersey</option>
                <option value="NM">New Mexico</option>
                <option value="NY">New York</option>
                <option value="NC">North Carolina</option>
                <option value="ND">North Dakota</option>
                <option value="OH">Ohio</option>
                <option value="OK">Oklahoma</option>
                <option value="OR">Oregon</option>
                <option value="PA">Pennsylvania</option>
                <option value="RI">Rhode Island</option>
                <option value="SC">South Carolina</option>
                <option value="SD">South Dakota</option>
                <option value="TN">Tennessee</option>
                <option value="TX">Texas</option>
                <option value="UT">Utah</option>
                <option value="VT">Vermont</option>
                <option value="VA">Virginia</option>
                <option value="WA">Washington</option>
                <option value="WV">West Virginia</option>
                <option value="WI">Wisconsin</option>
                <option value="WY">Wyoming</option>
            </select>
            </div>
            <input type="text" class="form-control" id="email" placeholder="Email">
            <input type="text" class="form-control mb-3" id="phone" placeholder="Phone Number">
            <input type="checkbox" id="delivery-save-address"><label for="delivery-save-address"><span class="gray80">Save address</span></label>&emsp;<input type="checkbox" id="status-updates"><label for="status-updates"><span class="gray80">Get text message status updates*</span></label>
        </div>
    </div>
    </div>
</div>