$(document).ready(function() {
    defaultShipIt();
    hideLabelsFromForm();
    shipToHomeShippingMethods();
    changeUPSStore();
    hideShippingForm();
});

function defaultShipIt() {
    $('#ship-it').prop("checked", true);
    $('.ship-it-tab-content').hide();
    $('#tab-SHIP_HOME_HOTEL_BUSINESS').show();
}

function hideLabelsFromForm() {
    $('#addressForm').find('.control-label').hide();
    $('#blPickUpByForm').find('.control-label').hide();
}

$('#ship-it-select-box').change(function () {
  dropdown = $('#ship-it-select-box').val();
  $('.ship-it-tab-content').hide();
  $('#' + "tab-" + dropdown).show();
});

$("input[id='ship-it']").click(function() {
    defaultShipIt();
    resetSelectBox('ship-it-select-box');
    hideShippingForm();
    resetSelectBox('ship-it-savedAddresses');
    resetSelectBox('ship-it-shipping-methods-select-box');
    emptyAddressFormAttributes();
    changeUPSStore();
});

function hideShippingForm() {
    if($('#ship-it-saved-addresses-dropdown').length == 1) {
        $('#ship-it-shippingAddressFormDiv').hide();
    }
}

function onChangeOfShipItShippingMethod() {
    $('#ship-it-notification').html("");
    var shippingMethod = $('#ship-it-select-box').val();
    if(shippingMethod == 'SHIP_HOME_HOTEL_BUSINESS') {
        resetSelectBox('ship-it-savedAddresses');
        resetSelectBox('ship-it-shipping-methods-select-box');
        emptyAddressFormAttributes();
        hideShippingForm();
    } else if(shippingMethod == 'SHIP_HOLD_UPS_OFFICE') {
        changeUPSStore();
    }
}

function emptyAddressFormAttributes() {
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]').val('');
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]').val('');
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]').val('');
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]').val('');
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]').val('');
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]').val('');
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]').val('');
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.addressType"]').val('');
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]').val('');
    $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]').val('');
}

function resetSelectBox(id) {
    var dropDown = document.getElementById(id);
    dropDown.selectedIndex = 0;
}

function shipToHomeShippingMethods() {
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/chooseShippingDelivery',
        data: {
            shippingGroup: "SHIP_HOME_HOTEL_BUSINESS",
            partnerZone: null,
            pinCode: null
        },
        type: "GET",
        dataType: 'json',
        success: function (data) {
            if(data != null && data != '') {
                let shippingModes = '<select id="ship-it-shipping-methods-select-box" class="btn btn-block btn-outline text-start my-4">';
                let numberSelected = 0;
                for (let i = 0; i < data.length; i++) {
                    shippingModes += '<option value="' + data[i].code + '">' + data[i].name;
                    if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                        shippingModes += '<span class="float-end">' + data[i].deliveryCost.formattedValue + '</span>';
                    }
                    shippingModes += '</option>';
                }
                shippingModes += '</select>';
                $('#shipToHomeShippingMethods').html(shippingModes);
            } else {
                showErrorNotification('Rental Dates not eligible for the selected shipping option!!');
            }
        },
        error: function (data) {
            console.log('error');
        }
    });
 }

function shippingMethodContinue() {
    $('#ship-it-notification').html("");
    var shippingCategory = $('input[name="shipProduct"]:checked').attr('id');
    var shippingMethod = $('#ship-it-select-box').val();
    if(shippingMethod == 'SHIP_HOME_HOTEL_BUSINESS') {
        shipToHomeShippingContinue(shippingMethod);
    } else if(shippingMethod == 'SHIP_HOLD_UPS_OFFICE') {
        shipToUPSStoreLocationContinue(shippingMethod);
    }
}

function shipToHomeShippingContinue(shippingMethod) {
    var savedAddress = null;
    var deliveryMode = $('#shipToHomeShippingMethods').find('select[id="ship-it-shipping-methods-select-box"]').val();
    if($('#ship-it-shippingAddressFormDiv').css('display') == "none") {
        savedAddress = $('select[id="ship-it-savedAddresses"]').val();
        (async() => {
          await saveSelectedAddress(savedAddress, deliveryMode);
        })();
    } else {
        var firstName = $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]');
        var lastName = $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]');
        var line1 = $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]');
        var line2 = $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]');
        var townCity = $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]');
        var postcode = $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]');
        var regionIso = $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]');
        var email = $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]');
        var phone = $('#ship-it-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]');
        if(validateField(firstName.val(), firstName) && validateField(lastName.val(), lastName) && validateField(line1.val(), line1) &&
            validateField(townCity.val(), townCity) && validateField(postcode.val(), postcode) && validateField(regionIso.val(), regionIso)
            && validateEmail(email.val(), email) && validatePhone(phone.val(), phone)) {
            let addressForm = {
                firstName : firstName.val(),
                lastName : lastName.val(),
                line1 : line1.val(),
                line2 : line2.val(),
                townCity : townCity.val(),
                regionIso : regionIso.val(),
                countryIso : 'US',
                postcode : postcode.val(),
                saveInAddressBook : $('#ship-it-shippingAddressForm').find('input[id="save-address"]').prop("checked"),
                phone : phone.val(),
                email : email.val()
            };
            (async() => {
                await addNewAddress(addressForm, deliveryMode);
            })();
        } else {
            showErrorNotification("Please enter mandatory fields values!!");
        }
    }
}

async function saveSelectedAddress(selectedAddress, deliveryMode) {
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-address/select',
        data: {
            selectedAddressCode: selectedAddress
        },
        type: "GET",
        async: false,
        success: function (data) {
            if(data == 'SUCCESS') {
                saveDeliveryMode(deliveryMode);
            }
        },
        error: function (data) {
            console.log('error');
        }
    });
}

async function addNewAddress(addressForm, deliveryMode) {
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-address/add',
        data: JSON.stringify(addressForm),
        type: "POST",
        async: false,
        contentType : 'application/json; charset=utf-8',
        mimeType : 'application/json',
        cache : false,
        success: function (data) {
            if(data != null) {
                saveDeliveryMode(deliveryMode);
            }
        },
        error: function (data) {
            console.log('error');
        }
    });
}

async function saveDeliveryMode(deliveryMode) {
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/select',
        data: {
            delivery_method: deliveryMode
        },
        type: "GET",
        async: false,
        success: function (data) {
            window.location.reload();
            //TODO: Handle for payment method for checkout
        },
        error: function (data) {
            console.log('error');
        }
    });
}

function onAddNewAddressClicked() {
    $('#ship-it-shippingAddressFormDiv').show();
    //need to add things
}

function onSavedAddressChange() {
    $('#ship-it-shippingAddressFormDiv').hide();
}

function shipToUPSStoreLocationContinue(shippingMethod) {
    //

    //shipping
    if($('#ship-it-pickup-gear').find('#pickup-person').find('input[id="store-pickup-other"]').prop("checked")) {
        if($("#ship-it-pickup-person").css('display') == 'block') {
            var firstName = $("#ship-it-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]').val();
            var lastName = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]').val();
            var email = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]').val();
            var phone = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]').val();
            if(validateField(firstName) && validateField(lastName) && validateEmail(email) && validatePhone(phone)) {
                let blPickUpByForm = {
                    firstName : firstName,
                    lastName : lastName,
                    phone : phone,
                    email : email
                };
                (async() => {
                  await savePickUpByFormOnCart(blPickUpByForm);
                })();
            } else {
                 showErrorNotification("Please enter mandatory fields values!!");
            }
        }
    }
}

async function savePickUpByFormOnCart(blPickUpByForm) {
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-address/addPickUpDetails',
        data: JSON.stringify(blPickUpByForm),
        type: "POST",
        async: false,
        contentType : 'application/json; charset=utf-8',
        mimeType : 'application/json',
        cache : false,
        success: function (data) {
            if(data != null) {
                console.log(data);
            }
        },
        error: function (data) {
            console.log('error');
        }
    });
}

function onClickOfFindStore() {
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/checkValidZip',
        data: {
            pinCode: $('#ship-it-ups-zip-code').val()
        },
        type: "GET",
        dataType: 'json',
        async: false,
        success: function (data) {
            if(data != null && data == true) {
                let upsStores = '';
                /*for (let i = 0; i < data.length; i++) {*/
                    upsStores += '<div id="ups-location-1" class="row store-location mb-3">' +
                                         '<div class="col-1">' +
                                             '<input type="radio" id="location-1" name="ups-location" checked="checked"><label for="location-1"></label>' +
                                         '</div>' +
                                         '<div class="col-11 col-md-7">' +
                                             '<p>The UPS Store<br>' +
                                             '<a href="#" target="_blank">123 Broadway San Carlos, CA 94070</a><br>' +
                                             '0.1 mi  •  555-456-7894</p>' +
                                         '</div>'
                                         '<div class="col-11 offset-1 col-md-4 offset-md-0">' +
                                             '<p class="mb-0"><span class="gray80">M-F</span>&emsp;9am-5pm</p>' +
                                             '<p class="mb-0"><span class="gray80">Sat</span>&emsp;10:30am-2pm</p>' +
                                             '<p class="mb-0"><span class="gray80">Sun</span>&emsp;CLOSED</p>' +
                                         '</div>' +
                                    '</div>';
                /*}*/
                $('#ship-it-SHIP_HOLD_UPS_OFFICE').html(upsStores);
                $('#changeUPSStoreButton').show();
                $('#ship-it-pickup-gear').show();
            }
            },
            error: function (data) {
                console.log('error');
            }
         });
}

function changeUPSStore() {
    $('#ship-it-ups-zip-code').val('');
    $('#changeUPSStoreButton').hide();
    $('#ship-it-pickup-gear').hide();
    $("#ship-it-pickup-person").hide();
    $('#ship-it-SHIP_HOLD_UPS_OFFICE').html("");
    $('#store-pickup-me').prop("checked", true);
    $("#ship-it-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]').val('');
    $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]').val('');
    $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]').val('');
    $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]').val('');
}

function showPickUpBySomeoneForm() {
    $("#ship-it-pickup-person").show()
}

function showPickUpByMeClick() {
    $("#ship-it-pickup-person").hide()
}

function validateField(attribute, fieldName) {
    if(attribute && attribute.trim() != '' && attribute.length < 255) {
        fieldName.removeClass('formControlErrorClass');
        return true;
    }
    fieldName.addClass('formControlErrorClass');
    return false;
}

function validateEmail(email, fieldName) {
    if(email && email.trim() != '' && null != email.match(/^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$/)) {
        fieldName.removeClass('formControlErrorClass');
        return true;
    }
    fieldName.addClass('formControlErrorClass');
    return false;
}

function validatePhone(phone, fieldName) {
    if(phone && phone.trim() != '' && null != phone.match(/^[\+]?[(]?[0-9]{3}[/)]?[-\s\.]?[0-9]{3}[-\s\.]?[0-9]{4,6}$/im)) {
        fieldName.removeClass('formControlErrorClass');
        return true;
    }
    fieldName.addClass('formControlErrorClass');
    return false;
}

 function onSelectPartnerPickup(event) {
    /*$.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/chooseShippingDelivery',
        data: {
            shippingGroup: "BL_PARTNER_PICKUP",
            partnerZone: event.name,
            pinCode: null},
        type: "GET",
        success: function (data) {
            if(data=='success') {
                alert('Success');
            }
        },
        error: function (error) {

        }
    });*/
 }

 function showErrorNotification(msg) {
    /*<div class="notification notification-warning">They must show ID at time of pickup</div>
           <div class="notification notification-warning">AM delivery is only available to business addresses.
           Not at the office? Select Ship and Hold at a UPS Store for AM delivery options!</div>*/
    let notification = '<div class="notification notification-warning">' + msg + '</div>';
    $('#ship-it-notification').html(notification);
 }