 $(document).ready(function() {
    defaultShipIt();
    hideLabelsFromForm();
    shipToHomeShippingMethods();
    changeUPSStore();
    hideShippingForm();
});

 $('#ship-it-select-box').change(function () {
   dropdown = $('#ship-it-select-box').val();
   $('.ship-it-tab-content').hide();
   $('#' + "tab-" + dropdown).show();
 });

 //MainContinueMethod
 function shippingMethodContinue() {
    var shippingCategory = $('input[name="shipProduct"]:checked').attr('id');
    if(shippingCategory == 'ship-it') {
        $('#ship-it-notification').html("");
        var shippingMethod = $('#ship-it-select-box').val();
        if(shippingMethod == 'SHIP_HOME_HOTEL_BUSINESS') {
            shipToHomeShippingContinue(shippingMethod);
        } else {
            shipToUPSStoreLocationContinue(shippingMethod);
        }
    } else if(shippingCategory == 'pickup') {
        pickUpPartnerLocationContinue();
    } else {
        SFOrNYCShippingSectionContinue();
    }
 }

 //ShipIt
 $("input[id='ship-it']").click(function() {
    defaultShipIt();
    resetSelectBox('ship-it-select-box');
    hideShippingForm();
    resetSelectBox('ship-it-savedAddresses');
    resetSelectBox('ship-it-shipping-methods-select-box');
    emptyAddressFormAttributes();
    changeUPSStore();
    $('#ship-it-notification').val('');
    $('#ship-it-notification').hide();
    $('#shipToHomeShippingMethods').html('');
    shipToHomeShippingMethods();
    $('#ship-it-save-address').prop("checked", true);
});

 function onChangeOfShipItShippingMethod() {
    $('#ship-it-notification').html("");
    var shippingMethod = $('#ship-it-select-box').val();
    if(shippingMethod == 'SHIP_HOME_HOTEL_BUSINESS') {
        resetSelectBox('ship-it-savedAddresses');
        resetSelectBox('ship-it-shipping-methods-select-box');
        emptyAddressFormAttributes();
        hideShippingForm();
        shipToHomeShippingMethods();
    } else if(shippingMethod == 'SHIP_HOLD_UPS_OFFICE') {
        fetchUPSDeliveryMethods();
        changeUPSStore();
    }
 }

 function shipToHomeShippingMethods() {
     $.ajax({
         url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/chooseShippingDelivery',
         data: {
             shippingGroup: "SHIP_HOME_HOTEL_BUSINESS",
             partnerZone: null
         },
         type: "GET",
         dataType: 'json',
         beforeSend: function(){
            $('.page-loader-new-layout').show();
         },
         success: function (data) {
             if(data != null && data.length != 0) {
                 let shippingModes = '<select id="ship-it-shipping-methods-select-box" class="btn btn-block btn-outline text-start my-4" '+
                                        'onchange="onChangeOfShipItShipToHome(this)">';
                 let numberSelected = 0;
                 for (let i = 0; i < data.length; i++) {
                     shippingModes += '<option value="' + data[i].code + '">' + data[i].name;
                     if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                         shippingModes += '<span class="float-end">' + data[i].deliveryCost.formattedValue + '</span>';
                     }
                     shippingModes += '</option>';
                     if(i == 0) {
                        $('#cart-shipping-cost').text(data[i].deliveryCost.formattedValue);
                        calculateCartTotal();
                     }
                 }
                 shippingModes += '</select>';
                 $('#shipToHomeShippingMethods').html(shippingModes);
             } else {
             	 $('#cart-shipping-cost').text('$0.00');
                 showErrorNotification('Rental Dates not eligible for the selected shipping option!!', false);
             }
         },
         complete: function() {
             $('.page-loader-new-layout').hide();
         },
         error: function (data) {
             $('.page-loader-new-layout').hide();
         }
     });
  }

 function shipToHomeShippingContinue(shippingMethod) {
      var savedAddress = null;
      var deliveryMode = $('#shipToHomeShippingMethods').find('select[id="ship-it-shipping-methods-select-box"]').val();
      var isAvailable = checkAvailability(deliveryMode);
      if(isAvailable)
      {
      if($('#delivery-shippingAddressFormDiv').css('display') == "none") {
          saveSelectedAddress($('select[id="ship-it-savedAddresses"]').val(), 'SHIP_HOME_HOTEL_BUSINESS', deliveryMode, null);
      } else {
          var firstName = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]');
          var lastName = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]');
          var line1 = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]');
          var line2 = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]');
          var townCity = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]');
          var postcode = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]');
          var regionIso = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]');
          var email = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]');
          var phone = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]');
          if(validateFormData(firstName, lastName, line1, townCity, postcode, regionIso, email, phone)) {
              addressValidationService(createAddressFormObject(firstName.val(), lastName.val(), line1.val(), line2.val(), townCity.val(),regionIso.val(),
                                                                 'US', postcode.val(), $('.ship-it-tab-content').find('input[id="ship-it-save-address"]').prop("checked"),
                                                                 phone.val(), email.val(), false, null, 'UNKNOWN'), deliveryMode, 'SHIP');
          } else {
              showErrorNotification("Please enter mandatory fields values!!", true);
          }
      }
      }
      else
      {
      	window.location.reload();
      }
      
  }

 //UPS-Store
 function fetchUPSDeliveryMethods() {
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/chooseShippingDelivery',
        data: {
            shippingGroup: "SHIP_HOLD_UPS_OFFICE",
            partnerZone: null
        },
        type: "GET",
        dataType: 'json',
        beforeSend: function(){
            $('.page-loader-new-layout').show();
        },
        success: function (data) {
            if(data != null && data.length != 0) {
                let shippingModes = '<b class="mt-4">Shipping Methods</b>';
                shippingModes += '<select id="ship-UPS-shipping-methods-select-box" class="btn btn-block btn-outline text-start my-4"' +
                                    'onchange="onChangeOfShipItShipToUPS()">';
                let numberSelected = 0;
                for (let i = 0; i < data.length; i++) {
                    shippingModes += '<option value="' + data[i].code + '">' + data[i].name;
                    if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                        shippingModes += '<span class="float-end">' + data[i].deliveryCost.formattedValue + '</span>';
                    }
                    shippingModes += '</option>';
                    if(i == 0) {
                        $('#cart-shipping-cost').text(data[i].deliveryCost.formattedValue);
                        calculateCartTotal();
                    }
                }
                shippingModes += '</select>';
                $('#shipToUPSShippingMethods').html(shippingModes);
                $('#checkZipForUPSPickup').show();
            } else {
            	$('#cart-shipping-cost').text('$0.00');
                showErrorNotification('Rental Dates not eligible for the selected shipping option!!', false);
            }
        },
        complete: function() {
            $('.page-loader-new-layout').hide();
        },
        error: function (data) {
            $('.page-loader-new-layout').hide();
        }
    });
 }

 function shipToUPSStoreLocationContinue(shippingMethod) {
     //shipping by someone form data
     if($('#ship-it-pickup-gear').find('#pickup-person').find('input[id="store-pickup-other"]').prop("checked")) {
         if($("#ship-it-pickup-person").css('display') == 'block') {
             var firstName = $("#ship-it-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]');
             var lastName = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]');
             var email = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]');
             var phone = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]');
             if(validateFormData(firstName, lastName, null, null, null, null, email, phone)) {
                 savePickUpByFormOnCart(createPickUPFormObject(firstName.val(), lastName.val(), email.val(), phone.val()),
                        $('#shipToUPSShippingMethods').find('#ship-UPS-shipping-methods-select-box').val(), false, createUPSStoreAddress());
             } else {
                  showErrorNotification("Please enter mandatory fields values!!", true);
             }
         }
     } else {
         savePickUpByFormOnCart(createPickUPFormObject(null, null, null, null), $('#shipToUPSShippingMethods')
                         .find('#ship-UPS-shipping-methods-select-box').val(), false, createUPSStoreAddress());
     }
 }

 function onClickOfFindStore() {
     $('#ship-it-SHIP_HOLD_UPS_OFFICE').html('');
     $('#ship-it-notification').html('');
     $('#ship-it-notification').hide();
     let pinCode = $('#ship-it-ups-zip-code').val();
     if(pinCode != '' && validateZip(pinCode)) {
         $.ajax({
             url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/checkValidZip',
             data: {
                 pinCode: pinCode
             },
             type: "GET",
             dataType: 'json',
             beforeSend: function(){
                $('.page-loader-new-layout').show();
             },
             success: function (data) {
                 if(data != null && data.length != 0 && data.statusMessage == 'Success') {
                     if(data.result != null && data.result.length != 0) {
                         let upsStores = '';
                         sessionStorage.setItem("UPSStores", JSON.stringify(data.result));
                         for (let i = 0; i < data.result.length; i++) {
                             upsStores += '<div id="ups-location-1" class="row store-location mb-3">' +
                                                  '<div class="col-1">' +
                                                      '<input type="radio" id="' + data.result[i].locationId + '" name="ups-location"><label for="' +
                                                         data.result[i].locationId + '" onClick="onSelectOfUPSStore('+ data.result[i].locationId +')"></label>' +
                                                  '</div>' +
                                                  '<div class="col-11 col-md-7">' +
                                                      '<p>' + data.result[i].consigneeName + '<br>' +
                                                         '<a href="https://maps.google.com/maps?q='+
                                                            data.result[i].addressLine + ',' + data.result[i].politicalDivision1 + ' ' +
                                                            data.result[i].politicalDivision2 + ' ' + data.result[i].postcodePrimaryLow +
                                                            '" target="_blank">' +
                                                             data.result[i].addressLine + ',' + data.result[i].politicalDivision1 + ' ' +
                                                             data.result[i].politicalDivision2 + ' ' + data.result[i].postcodePrimaryLow +
                                                         '</a><br>' ;
                                           if(data.result[i].distance != null) {
                                               upsStores +=  data.result[i].distance.value + ' ' +data.result[i].distance.unitCode + '•';
                                           }
                                           if(data.result[i].distance != null) {
                                               upsStores +=  data.result[i].contactNumber;
                                           }
                                           upsStores += '</p>' +
                                                  '</div>' +
                                                  '<div class="col-11 offset-1 col-md-4 offset-md-0">';
                                                  if(data.result[i].latestGroundDropOffTime != null && data.result[i].latestGroundDropOffTime.length != 0) {
                                         upsStores += '<p class="mb-0"><span class="gray80">M-F</span>&emsp;' + data.result[i].latestGroundDropOffTime[0].split(': ')[1] + '</p>' +
                                                      '<p class="mb-0"><span class="gray80">' +data.result[i].latestGroundDropOffTime[1].split(':')[0] + '</span>&emsp;&nbsp;' +
                                                                 data.result[i].latestGroundDropOffTime[1].split(': ')[1] + '</p>' +
                                                      '<p class="mb-0"><span class="gray80">' + data.result[i].latestGroundDropOffTime[2].split(':')[0] + '</span>&emsp;' +
                                                                 data.result[i].latestGroundDropOffTime[2].split(': ')[1] + '</p>';
                                                  }
                                    upsStores += '</div>' +
                                             '</div>';
                         }
                         $('#ship-it-SHIP_HOLD_UPS_OFFICE').html(upsStores);
                     }
                 } else {
                     showErrorNotification('Whoops! Something went wrong, please try again to Find UPS store later.', false);
                 }
             },
             complete: function() {
                 $('.page-loader-new-layout').hide();
             },
             error: function (data) {
                 $('.page-loader-new-layout').hide();
             }
         });
     } else {
         showErrorNotification('Please enter valid zipCode!!', false);
         $('.page-loader-new-layout').hide();
     }
  }

 function onSelectOfUPSStore(upsSelectedStoreId) {
     let stores = JSON.parse(sessionStorage.getItem("UPSStores"));
     if(stores != null && stores.length != 0) {
         for (let i = 0; i < stores.length; i++) {
            let upsStores = '';
             if(stores[i].locationId == upsSelectedStoreId) {
                $('#ship-it-SHIP_HOLD_UPS_OFFICE').html('');
                upsStores += '<div id="ups-location-1" class="row store-location mb-3">' +
                                  '<div class="col-1">' +
                                      '<input type="hidden" id="' + stores[i].locationId + '" name="ups-location"><label for="' +
                                         stores[i].locationId + '"></label>' +
                                  '</div>' +
                                  '<div class="col-11 col-md-7">' +
                                      '<p>' + stores[i].consigneeName + '<br>' +
                                         '<a href="#" target="_blank">' +
                                             stores[i].addressLine + ',' + stores[i].politicalDivision1 + ' ' +
                                             stores[i].politicalDivision2 + ' ' + stores[i].postcodePrimaryLow +
                                         '</a><br>' +
                                         '0.1 mi  •  555-456-7894' +
                                      '</p>' +
                                  '</div>' +
                                  '<div class="col-11 offset-1 col-md-4 offset-md-0">';
                                  if(stores[i].latestGroundDropOffTime != null && stores[i].latestGroundDropOffTime.length != 0) {
                         upsStores += '<p class="mb-0"><span class="gray80">M-F</span>&emsp;' + stores[i].latestGroundDropOffTime[0].split(': ')[1] + '</p>' +
                                      '<p class="mb-0"><span class="gray80">' +stores[i].latestGroundDropOffTime[1].split(':')[0] + '</span>&emsp;' +
                                                 stores[i].latestGroundDropOffTime[1].split(':')[1] + '</p>' +
                                      '<p class="mb-0"><span class="gray80">' + stores[i].latestGroundDropOffTime[2].split(':')[0] + '</span>&emsp;' +
                                                 stores[i].latestGroundDropOffTime[2].split(':')[1] + '</p>';
                                  }
                    upsStores += '</div>' +
                             '</div>';
                $('#ship-it-SHIP_HOLD_UPS_OFFICE').html(upsStores);
                $('#changeUPSStoreButton').show();
                $('#ship-it-pickup-gear').show();
                $('#ship-it-ups-zip-code').val('');
                $('#checkZipForUPSPickup').hide();
             }
         }
     }
 }

 function createUPSStoreAddress() {
     let upsSelectedStoreId = $('input[name="ups-location"]').attr('id');
     let stores = JSON.parse(sessionStorage.getItem("UPSStores"));
     if(stores != null && stores.length != 0) {
         for (let i = 0; i < stores.length; i++) {
             if(stores[i].locationId == upsSelectedStoreId) {
                 return createAddressFormObject(stores[i].consigneeName, "UPS", stores[i].addressLine, null, stores[i].politicalDivision2,
                         stores[i].politicalDivision1, stores[i].countryCode, stores[i].postcodePrimaryLow, false, null, null, true,
                         stores[i].latestGroundDropOffTime, 'BUSINESS')
             }
         }
     }
  }

 function changeUPSStore() {
     $('#checkZipForUPSPickup').show();
     $('#ship-it-ups-zip-code').val('');
     $('#changeUPSStoreButton').hide();
     $('#ship-it-pickup-gear').hide();
     $("#ship-it-pickup-person").hide();
     $('#ship-it-SHIP_HOLD_UPS_OFFICE').html("");
     $('#store-pickup-me').prop("checked", true);
     $("#ship-it-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]').val('');
     $("#ship-it-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]').removeClass('error');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]').val('');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]').removeClass('error');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]').val('');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]').removeClass('error');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]').val('');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]').removeClass('error');
     $('#ship-it-notification').val('');
     $('#ship-it-notification').hide();
 }

 function createPickUPFormObject(firstName, lastName, phone, email) {
      let blPickUpByForm = {
          firstName : firstName,
          lastName : lastName,
          phone : phone,
          email : email
      };
      return blPickUpByForm;
  }

 //PickUp
 $("input[id='pickup']").click(function() {
     resetSelectBox('pick-up-select-box');
     resetPartnerPickUpSection();
     showPartnerPickUpDeliveryModes($('#pick-up-select-box').val());
     $('#pickup-me').prop("checked", true);
     $('#pick-up-notification').val('');
 });

 function pickUpPartnerLocationContinue(shippingMethod) {
     if($('#pick-up-pickup-gear').find('#pickup-person').find('input[id="pickup-other"]').prop("checked")) {
         if($("#store-pickup-person").css('display') == 'block') {
             var firstName = $("#store-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]');
             var lastName = $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]');
             var email = $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]');
             var phone = $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]');
             if(validateFormData(firstName, lastName, null, null, null, null, email, phone)) {
                   savePickUpByFormOnCart(createPickUPFormObject(firstName.val(), lastName.val(), email.val(), phone.val()),
                             $('#partnerPickUpShippingMethods #pickup-nyc').find('input[name="pickup-locations"]:checked').attr('id'), true, null);
             } else {
                  showErrorNotification("Please enter mandatory fields values!!", true);
             }
         }
     } else {
           savePickUpByFormOnCart(createPickUPFormObject(null, null, null, null),
                 $('#partnerPickUpShippingMethods #pickup-nyc').find('input[name="pickup-locations"]:checked').attr('id'), true, null);
     }
 }

 function resetPartnerPickUpSection() {
     $('#partnerPickUpShippingMethods').html('');
     $('#pick-up-pickup-gear').hide();
     $('#store-pickup-person').hide();
     $('#pick-up-notification').val('')
     $('#pick-up-notification').hide();
     $("#store-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]').val('');
     $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]').val('');
     $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]').val('');
     $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]').val('');
     $('#pick-up-notification').hide();
 }

 function showPickUpBySomeoneForm() {
    $("#ship-it-pickup-person").show();
 }

 function showPickUpByMeClick() {
     $("#ship-it-pickup-person").hide()
     $('#pickup-me').prop("checked", true);
     $("#ship-it-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]').val('');
     $("#ship-it-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]').removeClass('error');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]').val('');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]').removeClass('error');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]').val('');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]').removeClass('error');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]').val('');
     $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]').removeClass('error');
     $('#ship-it-notification').val('');
     $('#ship-it-notification').hide();
 }

 function showPartnerPickUpDeliveryModes(partnerZone) {
    $('#pickup-me').prop("checked", true);
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/chooseShippingDelivery',
        data: {
            shippingGroup: "BL_PARTNER_PICKUP",
            partnerZone: partnerZone,
        },
        type: "GET",
        beforeSend: function(){
            $('.page-loader-new-layout').show();
        },
        success: function (data) {
            if(data != null && data.length != 0) {
                let partnerDelivery = '';
                for (let i = 0; i < data.length; i++) {
                    if(i == 0 && data.length == 1) {
                        $('#cart-shipping-cost').text(data[i].deliveryCost.formattedValue);
                        calculateCartTotal();
                    } else {
                        $('#cart-shipping-cost').text('$0.00');
                        calculateCartTotal();
                    }
                    partnerDelivery += '<div id="pickup-nyc" class="row store-location mb-3">' +
                                            '<div class="col-1">' +
                                                '<input type="radio" id="' +
                                                data[i].code +
                                                '" name="pickup-locations"><label for="' + data[i].code + '" onClick="onSelectOfPartnerAddress(' + data[i].code + ')"></label>';
                         partnerDelivery += '</div>' +
                                            '<div class="col-11">' +
                                                '<p>' + data[i].name + '- <span id="' + data[i].code + '-pickUpCost">' + data[i].deliveryCost.formattedValue + ' </span><br>' +
                                                    '<a href="' + data[i].internalStoreAddress.url + '" target="_blank">' +
                                                        data[i].internalStoreAddress.formattedAddress +
                                                    '</a><br>' +
                                                    data[i].internalStoreAddress.phone +
                                                '</p>' ;
                         if(data[i].internalStoreAddress.openingDaysDetails != null) {
                            partnerDelivery += '<p class="mb-0 mt-3"><span class="gray80">M-F</span> ' +
                                                    data[i].internalStoreAddress.openingDaysDetails["M-F"] +
                                                '</p>' +
                                                '<p class="mb-0"><span class="gray80">Sat</span> ' +
                                                    data[i].internalStoreAddress.openingDaysDetails["Sat"] +
                                                '</p>' +
                                                '<p class="mb-0"><span class="gray80">Sun</span> ' +
                                                    data[i].internalStoreAddress.openingDaysDetails["Sun"] +
                                                '</p>' ;
                         }
                         partnerDelivery += '</div>' +
                                        '</div>';
                }
                $('#partnerPickUpShippingMethods').html(partnerDelivery);
                $('#pick-up-pickup-gear').show();
                if(data.length == 1) {
                    $('#partnerPickUpShippingMethods #pickup-nyc').first().find('input[name="pickup-locations"]').prop("checked", true);
                }
                showErrorNotificationPickUp('They must show ID at time of pickup');
            } else {
            	$('#cart-shipping-cost').text('$0.00');
                showErrorNotificationPickUp('Rental Dates not eligible for the selected shipping option!!');
            }
        },
        complete: function() {
            $('.page-loader-new-layout').hide();
        },
        error: function (error) {
            $('.page-loader-new-layout').hide();
        }
    });
 }

 function onAddNewAddressClicked() {
    if($('input[name="shipProduct"]:checked').attr('id') == 'ship-it') {
        $('#delivery-shippingAddressFormDiv').show();
        $('#ship-it-save-address-div').show();
    } else {
        $('#same-day-address-div #delivery-shippingAddressFormDiv').show();
        $('#same-day-save-address-div').show();
        $('#same-day-status-updates-div').show();
        if($('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]').length != 0) {
            $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]')
                        .val($('#sameDayZipCheckText').val());
            $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]')
                .prop("disabled", true);
        }
    }
 }

 function onSavedAddressChange() {
    if($('input[name="shipProduct"]:checked').attr('id') == 'ship-it') {
        $('#delivery-shippingAddressFormDiv').hide();
        $('#ship-it-save-address-div').hide();
        $('#ship-it-save-address-div #ship-it-save-address').prop("checked", true);
        emptyAddressFormAttributes();
        $('#ship-it-notification').val('');
        $('#ship-it-notification').hide();
    } else {
        $('#same-day-address-div #delivery-shippingAddressFormDiv').hide();
        $('#same-day-save-address-div').hide();
        $('#same-day-save-address-div #same-day-save-address').prop("checked", false);
        $('#same-day-status-updates-div').hide();
        emptySFOrNYCAddressForm();
        $('#same-day-notification').val('');
        $('#same-day-notification').hide();
    }
 }

 function pickUpBySomeoneForm() {
     $("#store-pickup-person").show()
     $('#store-pickup-person #blPickUpByForm').find('.control-label').hide();
     $('#pick-up-notification').val('')
     $('#pick-up-notification').hide();
     $("#store-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]').val('');
     $("#store-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]').removeClass('error');
     $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]').val('');
     $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]').removeClass('error');
     $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]').val('');
     $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]').removeClass('error');
     $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]').val('');
     $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]').removeClass('error');
     showErrorNotificationPickUp('They must show ID at time of pickup');
 }

 function pickUpByMeClick() { $("#store-pickup-person").hide(); }

  function onSelectPartnerPickup(event) {
     resetPartnerPickUpSection();
     showPartnerPickUpDeliveryModes(event.value);
  }

  //SF Or NYC
  $("input[id='sameday']").click(function() {
      if($('#same-day-address-div #delivery-saved-addresses-dropdown').length == 1) {
          $('#same-day-address-div #delivery-shippingAddressFormDiv').hide();
      }
      $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.control-label').hide();
      resetSelectBox('same-day-select-box');
      $('#same-day-notification').val('');
      $('#same-day-notification').hide();
      $('#same-day-address-div').hide();
      $('#sameDayShippingMethodsNotification').hide();
      $('#sameDayShippingMethods').html('');
      $('#sameDayZipCheckText').val('');
      $('#cart-shipping-cost').text('$0.00');
      calculateCartTotal();
  });

  function onChangeOfSameDayShippingMethod() {
       $('#same-day-notification').val('');
       $('#same-day-notification').hide('');
       $('#same-day-address-div').hide();
       emptySFOrNYCAddressForm();
       $('#sameDayShippingMethodsNotification').hide();
       $('#sameDayShippingMethods').html('');
       $('#sameDayZipCheckText').val('');
  }

  function sameDayZipCheck() {
    $('#same-day-notification').val('');
    $('#same-day-notification').hide();
    $('#same-day-address-div').hide();
    $('#sameDayShippingMethodsNotification').hide();
    $('#sameDayShippingMethods').html('');
    if(validateZip($('#sameDayZipCheckText').val())) {
        $.ajax({
            url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/checkValidZipFromFedexService',
            data: {
                pinCode: $('#sameDayZipCheckText').val(),
                shippingGroup: $('#same-day-select-box').val()
            },
            type: "GET",
            dataType: 'json',
            beforeSend: function(){
                $('.page-loader-new-layout').show();
            },
            success: function (data) {
                if(data) {
                    $.ajax({
                       url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/chooseShippingDelivery',
                       data: {
                           shippingGroup: $('#same-day-select-box').val(),
                           partnerZone: null
                       },
                       type: "GET",
                       dataType: 'json',
                       beforeSend: function(){
                            $('.page-loader-new-layout').show();
                       },
                       success: function (data) {
                           if(data != null && data.length != 0) {
                               let sameDayShippingModes = '<b>Delivery Window</b>';
                               sameDayShippingModes += '<select id="same-day-shipping-methods-select-box" class="btn btn-block btn-outline text-start my-4"' +
                                                        ' onChange="onChangeOfSameDayShippingMethodForCost()">';
                               let numberSelected = 0;
                               for (let i = 0; i < data.length; i++) {
                                    if(i == 0) {
                                        $('#cart-shipping-cost').text(data[i].deliveryCost.formattedValue);
                                        calculateCartTotal();
                                    }
                                    var selectionText = data[i].name.split("_");
                                   sameDayShippingModes += '<option value="' + data[i].code + '">' + selectionText[0];
                                   if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                                       sameDayShippingModes += '<span class="float-end">' + data[i].deliveryCost.formattedValue + '</span>';
                                   }
                                   sameDayShippingModes += '</option>';
                               }
                               sameDayShippingModes += '</select>';
                               $('#sameDayShippingMethods').html(sameDayShippingModes);
                               $('#sameDayShippingMethodsNotification').show();
                               $('#same-day-address-div').show();
                               if($('#same-day-address-div #delivery-saved-addresses-dropdown').length == 1) {
                                   $('#same-day-save-address-div').hide();
                                   $('#same-day-save-address-div #same-day-save-address').prop("checked", false);
                                   $('#same-day-status-updates-div').hide();
                                   $('#same-day-status-updates-div #same-day-status-updates').prop("checked", false);
                               }
                           } else {
                           	   $('#cart-shipping-cost').text('$0.00');
                               showErrorNotificationSameDay('No delivery windows are available for this date. Please change your shipping method or rental date to continue.', false);
                           }
                       },
                       complete: function() {
                           $('.page-loader-new-layout').hide();
                       },
                       error: function (data) {
                           $('.page-loader-new-layout').hide();
                       }
                   });
                } else {
                    showErrorNotificationSameDay('Whoops! We were unable to get shipping information back from FedEx, please change your shipping method or try again in a few minutes', false);
                }
            },
            complete: function() {
                $('.page-loader-new-layout').hide();
            },
            error: function (data) {
                   $('.page-loader-new-layout').hide();
            }
        });
    } else {
        showErrorNotificationSameDay('Sorry, Same Day delivery is not available for your zipcode!', false);
        $('.page-loader-new-layout').hide();
    }
  }

  function SFOrNYCShippingSectionContinue() {
    $('#same-day-notification').val('');
    $('#same-day-notification').hide();
    var savedAddress = null;
    var sameDayDeliveryNote = $('#sameDayDeliveryNote').val();
    var deliveryMode = $('#sameDayShippingMethods').find('select[id="same-day-shipping-methods-select-box"]').val();
    var isAvailable = checkAvailability(deliveryMode);
      if(isAvailable)
      {
    if($('#same-day-address-div #delivery-shippingAddressFormDiv').css('display') == "none") {
        savedAddress = $('#same-day-address-div #delivery-saved-addresses-dropdown').find('select[id="ship-it-savedAddresses"]').val();
        $.ajax({
           url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/saveDeliveryDetails',
           data: {
               deliveryNote: sameDayDeliveryNote,
               statusUpdate: false
           },
           type: "GET",
           dataType: 'json',
           beforeSend: function(){
                $('.page-loader-new-layout').show();
           },
           success: function (data) {
                if(data == 'SUCCESS') {
                    saveSelectedAddress(savedAddress, $('#same-day-select-box').val(), deliveryMode, $('#sameDayZipCheckText').val());
                }
           },
           error: function (data) {
                $('.page-loader-new-layout').hide();
           }
        });
    } else {
        var firstName = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]');
        var lastName = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]');
        var line1 = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]');
        var line2 = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]');
        var townCity = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]');
        var postcode = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]');
        var regionIso = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]');
        var email = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]');
        var phone = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]');
        if(validateFormData(firstName, lastName, line1, townCity, postcode, regionIso, email, phone)) {
            $.ajax({
               url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/saveDeliveryDetails',
               data: {
                   deliveryNote: sameDayDeliveryNote,
                   statusUpdate: $('#same-day-status-updates').prop("checked")
               },
               type: "GET",
               dataType: 'json',
               beforeSend: function(){
                    $('.page-loader-new-layout').show();
               },
               success: function (data) {
                    if(data == 'SUCCESS') {
                        addressValidationService(createAddressFormObject(firstName.val(), lastName.val(), line1.val(), line2.val(),
                                                    townCity.val(),regionIso.val(), 'US', postcode.val(),
                                                    $('#same-day-address-div').find('input[id="same-day-save-address"]').prop("checked"),
                                                    phone.val(), email.val(), false, null, 'UNKNOWN'), deliveryMode, 'RUSH');
                    }
               },
               complete: function() {
                   $('.page-loader-new-layout').hide();
               },
               error: function (data) {
                    $('.page-loader-new-layout').hide();
               }
            });
        } else {
            showErrorNotificationSameDay("Please enter mandatory fields values!!", true);
            $('.page-loader-new-layout').hide();
        }
    }
      }
      else
      {
      	window.location.reload();
    }
  }

  function emptySFOrNYCAddressForm() {
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.addressType"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.addressType"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]').removeClass('error');
  }

 //Error Notifications
 function showErrorNotification(msg, status) {
    let notification = '<div class="notification notification-warning">' + msg + '</div>';
    $('#ship-it-notification').html(notification);
    $('#ship-it-notification').show();
    if(status) {
        $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group .error')[0].scrollIntoView(true);
    }
 }

 function showErrorNotificationPickUp(msg) {
     let notification = '<div class="notification notification-warning">' + msg + '</div>';
     $('#pick-up-notification').html(notification);
     $('#pick-up-notification').show();
 }

 function showErrorNotificationSameDay(msg, status) {
    let notification = '<div class="notification notification-warning">' + msg + '</div>';
    $('#same-day-notification').html(notification);
    $('#same-day-notification').show();
    if(status) {
        $('#same-day-address-div #delivery-shippingAddressFormDiv #addressForm').find('.form-group .error')[0].scrollIntoView(true);
    }
 }

 //Methods
 function hideShippingForm() {
     if($('#delivery-saved-addresses-dropdown').length == 1) {
         $('#delivery-shippingAddressFormDiv').hide();
         $('#ship-it-save-address-div').hide();
     }
 }

 function defaultShipIt() {
     $('#ship-it').prop("checked", true);
     $('.ship-it-tab-content').hide();
     $('#tab-SHIP_HOME_HOTEL_BUSINESS').show();
 }

 function hideLabelsFromForm() {
     $('#addressForm').find('.control-label').hide();
     $('#blPickUpByForm').find('.control-label').hide();
 }

 function emptyAddressFormAttributes() {
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]').removeClass('error');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]').removeClass('error');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]').removeClass('error');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]').removeClass('error');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]').removeClass('error');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]').removeClass('error');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]').removeClass('error');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.addressType"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.addressType"]').removeClass('error');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]').removeClass('error');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]').removeClass('error');
 }

 function resetSelectBox(id) {
     var dropDown = document.getElementById(id);
     if(dropDown != null) {
         dropDown.selectedIndex = 0;
     }
 }

 function validateFormData(firstName, lastName, line1, townCity, postcode, regionIso, email, phone) {
    let firstNameStatus = validateField(firstName.val(), firstName);
    let lastNameStatus = validateField(lastName.val(), lastName);
    let line1Status = '';
    let line2Status = '';
    let postcodeStatus = '';
    let regionIsoStatus = '';
    if(line1 != null) {
        line1Status = validateField(line1.val(), line1);
    }
    if(townCity != null) {
        line2Status = validateField(townCity.val(), townCity);
    }
    if(postcode != null) {
        postcodeStatus = validateField(postcode.val(), postcode);
    }
    if(regionIso != null) {
        regionIsoStatus = validateField(regionIso.val(), regionIso);
    }
    let emailStatus = validateEmail(email.val(), email);
    let phoneStatus = validatePhone(phone.val(), phone);
    if(line1 != null && townCity != null && postcode != null && regionIso != null) {
        if(firstNameStatus && lastNameStatus && line1Status && line2Status && postcodeStatus && regionIsoStatus && emailStatus && phoneStatus) {
            return true;
        }
        return false;
    } else {
        if(firstNameStatus && lastNameStatus && emailStatus && phoneStatus) {
            return true;
        }
        return false;
    }
 }

 function createAddressFormObject(firstName, lastName, line1, line2, townCity, regionIso, countryIso, postcode, status, phone, email,
    upsStoreAddress, openingDaysDetails, addressType) {
    if(openingDaysDetails != null) {
        let openingDaysDetailsMap = new Map([
          [openingDaysDetails[0].split(': ')[0],  openingDaysDetails[0].split(':')[1]],
          [openingDaysDetails[1].split(': ')[0], openingDaysDetails[1].split(':')[1]],
          [openingDaysDetails[2].split(': ')[0], openingDaysDetails[2].split(':')[1]],
        ])
        openingDaysDetails = openingDaysDetailsMap;
    }

    if(regionIso.includes('-')) {
        regionIso = regionIso;
    } else {
       regionIso = countryIso+ '-' +regionIso;
    }

    let addressForm = {
        firstName : firstName,
        lastName : lastName,
        line1 : line1,
        line2 : line2,
        townCity : townCity,
        regionIso : regionIso,
        countryIso : countryIso,
        postcode : postcode,
        saveInAddressBook : status,
        phone : phone,
        email : email,
        upsStoreAddress: upsStoreAddress,
        openingDaysDetails: openingDaysDetails,
        addressType: addressType
    };
    return addressForm;
 }

 function validateZip(zipCode) {
    let zipRegex = /(^\d{5}$)|(^\d{5}-\d{4}$)/;
    return zipRegex.test(zipCode);
 }

 function validateField(attribute, fieldName) {
      if(attribute && attribute.trim() != '' && attribute.length < 255) {
          fieldName.removeClass('error');
          return true;
      }
      fieldName.addClass('error');
      return false;
  }

 function validateEmail(email, fieldName) {
      if(email && email.trim() != '' && null != email.match(/^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$/)) {
          fieldName.removeClass('error');
          return true;
      }
      fieldName.addClass('error');
      return false;
 }

 function validatePhone(phone, fieldName) {
      if(phone && phone.trim() != '' && null != phone.match(/^[\+]?[(]?[0-9]{3}[/)]?[-\s\.]?[0-9]{3}[-\s\.]?[0-9]{4,6}$/im)) {
          fieldName.removeClass('error');
          return true;
      }
      fieldName.addClass('error');
      return false;
 }

 //AJAX
 function addressValidationService(addressForm, deliveryMode, section) {
    sessionStorage.setItem("enteredAddressForm", JSON.stringify(addressForm));
    sessionStorage.setItem("section", JSON.stringify(section));
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/avsCheck',
        data: JSON.stringify(addressForm),
        type: "POST",
        contentType : 'application/json; charset=utf-8',
        mimeType : 'application/json',
        cache : false,
        beforeSend: function(){
           $('.page-loader-new-layout').show();
        },
        success: function (data) {
            let addressForm = JSON.parse(sessionStorage.getItem("enteredAddressForm"));
            if(data != null && data.statusMessage == 'Success' && data.result.length > 0) {
                 sessionStorage.setItem("avsFlowDeliveryMode", JSON.stringify(deliveryMode));
                 let whatYouEntered = addressForm.line1 + '<br/>' + addressForm.townCity + ', ' + addressForm.regionIso.split('-')[1] + ' ' +
                                         addressForm.postcode ;
                 $('#whatYouEntered').html(whatYouEntered);
                 sessionStorage.setItem("suggestedAddressForm", JSON.stringify(data.result[0]));
                 let whatWeSuggest = data.result[0].line1 + '<br/>' + data.result[0].town + ', ' + data.result[0].region.isocodeShort +
                                     ' ' + data.result[0].postalCode;
                 $('#whatWeSuggest').html(whatWeSuggest);
                 $('#avsCheck').modal('show');
            } else {
                 //suggested state not supported error from response
                 addNewAddress(addressForm, deliveryMode)
                     .then((data) => {
                         sessionStorage.removeItem("enteredAddressForm");
                         saveDeliveryMode(deliveryMode, false)
                             .then((data) => {
                                 $('.page-loader-new-layout').hide();
                                 window.location.reload();
                             })
                             .catch((error) => {
                               console.log(error)
                             })
                     })
                     .catch((error) => {
                       console.log(error)
                     })
            }
        },
        complete: function() {
            $('.page-loader-new-layout').hide();
        },
        error: function (data) {
            $('.page-loader-new-layout').hide();
        }
    });
 }

 function onClickOfSaveSuggestedAddress() {
     let addressForm = JSON.parse(sessionStorage.getItem("suggestedAddressForm"));
     let enteredAddressForm = JSON.parse(sessionStorage.getItem("enteredAddressForm"));
     let deliveryMode = JSON.parse(sessionStorage.getItem("avsFlowDeliveryMode"));
     let section = JSON.parse(sessionStorage.getItem("section"));
     if(section == 'SHIP') {
        if(deliveryMode.includes('AM') && addressForm.addressType != 'BUSINESS') {
            showAMDeliveryErrorMessage(section);
            $('#avsCheck').modal('hide');
        } else {
            callAddNewAddress(enteredAddressForm, addressForm, deliveryMode);
        }
     } else {
        if((deliveryMode.includes('AM') || deliveryMode.includes('SAVER')) && addressForm.addressType != 'BUSINESS') {
            showAMDeliveryErrorMessage(section);
            $('#avsCheck').modal('hide');
        } else {
            callAddNewAddress(enteredAddressForm, addressForm, deliveryMode);
        }
     }
 }

 function callAddNewAddress(enteredAddressForm, addressForm, deliveryMode) {
    addNewAddress(createAddressFormObject(enteredAddressForm.firstName, enteredAddressForm.lastName, addressForm.line1, addressForm.line2,
         addressForm.town, addressForm.region.isocode, 'US', addressForm.postalCode, enteredAddressForm.saveInAddressBook,
         enteredAddressForm.phone, enteredAddressForm.email, false, null, addressForm.addressType),
         JSON.parse(sessionStorage.getItem("avsFlowDeliveryMode")))
              .then((data) => {
                  sessionStorage.removeItem("suggestedAddressForm");
                  sessionStorage.removeItem("enteredAddressForm");
                  sessionStorage.removeItem("avsFlowDeliveryMode");
                  sessionStorage.removeItem("rushStatus");
                  saveDeliveryMode(deliveryMode, false)
                       .then((data) => {
                           $('.page-loader-new-layout').hide();
                           window.location.reload();
                       })
                       .catch((error) => {
                         console.log(error)
                       })
              })
              .catch((error) => {
                console.log(error)
              })
 }

 function onClickOfSaveEnteredAddress() {
    let deliveryMode = JSON.parse(sessionStorage.getItem("avsFlowDeliveryMode"));
    let addressForm = JSON.parse(sessionStorage.getItem("suggestedAddressForm"));
    let section = JSON.parse(sessionStorage.getItem("section"));
    if(section == 'SHIP') {
        if(deliveryMode.includes('AM') && addressForm.addressType != 'BUSINESS') {
            showAMDeliveryErrorMessage(section);
            $('#avsCheck').modal('hide');
        } else {
            callEnteredAddNewAddress(addressForm, deliveryMode);
        }
    } else {
        if((deliveryMode.includes('AM') || deliveryMode.includes('SAVER')) && addressForm.addressType != 'BUSINESS') {
            showAMDeliveryErrorMessage(section);
            $('#avsCheck').modal('hide');
        } else {
            callEnteredAddNewAddress(addressForm, deliveryMode);
        }
    }
 }

 function callEnteredAddNewAddress(addressForm, deliveryMode) {
    let newAddressForm = JSON.parse(sessionStorage.getItem("enteredAddressForm"));
    newAddressForm['addressType'] = addressForm.addressType;
    addNewAddress(newAddressForm, JSON.parse(sessionStorage.getItem("avsFlowDeliveryMode")))
          .then((data) => {
              sessionStorage.removeItem("suggestedAddressForm");
              sessionStorage.removeItem("enteredAddressForm");
              sessionStorage.removeItem("avsFlowDeliveryMode");
              sessionStorage.removeItem("rushStatus");
              saveDeliveryMode(deliveryMode, false)
                 .then((data) => {
                     $('.page-loader-new-layout').hide();
                     window.location.reload();
                 })
                 .catch((error) => {
                   console.log(error)
                 })
          })
          .catch((error) => {
            console.log(error)
          })
 }

 function showAMDeliveryErrorMessage(section) {
    if(section == 'RUSH') {
        showErrorNotificationSameDay('AM delivery is only available to business addresses. Not at the office? Select Ship and Hold at a UPS Store for AM delivery options!', false);
    } else {
        showErrorNotification('AM delivery is only available to business addresses. Not at the office? Select Ship and Hold at a UPS Store for AM delivery options!', false);
    }
 }

 function saveSelectedAddress(selectedAddress, shippingGroup, deliveryMode, rushZip) {
     $.ajax({
         url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/selectAddress',
         data: {
             selectedAddressCode: selectedAddress,
             shippingGroup: shippingGroup,
             deliveryMode: deliveryMode,
             rushZip: rushZip
         },
         type: "GET",
         beforeSend: function(){
            $('.page-loader-new-layout').show();
         },
         success: function (data) {
             if(data == 'SUCCESS') {
                saveDeliveryMode(deliveryMode, false)
                    .then((data) => {
                        $('.page-loader-new-layout').hide();
                        window.location.reload();
                    })
                    .catch((error) => {
                      console.log(error)
                    })
             } else if(data == 'AM-ERROR') {
                if(shippingGroup == 'SHIP_HOME_HOTEL_BUSINESS') {
                    showAMDeliveryErrorMessage('SHIP');
                } else {
                    showAMDeliveryErrorMessage('RUSH');
                }
                $('.page-loader-new-layout').hide();
             } else {
                showErrorNotificationSameDay('Your saved address must match the zipcode you used for shipping options, please change your address or zipcode and try again.', false);
                $('.page-loader-new-layout').hide();
             }
         },
         error: function (data) {
             $('.page-loader-new-layout').hide();
         }
     });
 }

 async function addNewAddress(addressForm, deliveryMode) {
    return new Promise((resolve, reject) => {
         $.ajax({
             url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/add',
             data: JSON.stringify(addressForm),
             type: "POST",
             contentType : 'application/json; charset=utf-8',
             mimeType : 'application/json',
             cache : false,
             beforeSend: function(){
                $('.page-loader-new-layout').show();
             },
             success: function (data) {
                 if(data == 'SUCCESS') {
                    resolve(data);
                 }
             },
             error: function (data) {
                 reject(data);
             }
         });
    });
 }

 async function saveDeliveryMode(deliveryMode, internalAddressIndicator) {
    return new Promise((resolve, reject) => {
         $.ajax({
             url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/select',
             data: {
                 delivery_method: deliveryMode,
                 internalStoreAddress: internalAddressIndicator
             },
             type: "GET",
             async: false,
             beforeSend: function(){
                $('.page-loader-new-layout').show();
             },
             success: function (data) {
                 resolve(data);
                 //TODO: Handle for payment method for checkout
             },
             error: function (data) {
                 reject(data);
             }
         });
    });
 }

 function savePickUpByFormOnCart(blPickUpByForm, deliveryMethod, status, upsStoreAddress) {
 var isAvailable = checkAvailability(deliveryMethod);
      if(isAvailable)
      {
     $.ajax({
         url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/addPickUpDetails',
         data: JSON.stringify(blPickUpByForm),
         type: "POST",
         contentType : 'application/json; charset=utf-8',
         mimeType : 'application/json',
         cache : false,
         beforeSend: function(){
            $('.page-loader-new-layout').show();
         },
         success: function (data) {
             if(data != null) {
                 if(upsStoreAddress != null) {
                    addNewAddress(upsStoreAddress, deliveryMethod)
                        .then((data) => {
                            saveDeliveryMode(deliveryMethod, status)
                                .then((data) => {
                                    $('.page-loader-new-layout').hide();
                                    window.location.reload();
                                })
                                .catch((error) => {
                                  console.log(error)
                                })
                        })
                        .catch((error) => {
                          console.log(error)
                        })
                 } else {
                    saveDeliveryMode(deliveryMethod, status)
                        .then((data) => {
                            $('.page-loader-new-layout').hide();
                            window.location.reload();
                        })
                        .catch((error) => {
                          console.log(error)
                        })
                 }
             }
         },
         complete: function() {

         },
         error: function (data) {
            $('.page-loader-new-layout').hide();
         }
     });
     }
     else
     {
     	window.location.reload();
     }
  }

  //show Price of shipping on cart
  function onChangeOfShipItShipToHome(event) {
      $('#cart-shipping-cost').text('$'+ $('option[value="' + $('#shipToHomeShippingMethods').find('select[id="ship-it-shipping-methods-select-box"]')
        .val() + '"]').text().split('$')[1]);
      calculateCartTotal();
  }

  function onChangeOfShipItShipToUPS() {
     $('#cart-shipping-cost').text('$'+ $('option[value="' + $('#shipToUPSShippingMethods').find('#ship-UPS-shipping-methods-select-box')
            .val()+ '"]').text().split('$')[1]);
     calculateCartTotal();
  }

  function onSelectOfPartnerAddress(event) {
    $('#cart-shipping-cost').text($('#'+event.getAttribute('id')+'-pickUpCost').text().trim());
    calculateCartTotal();
  }

  function onChangeOfSameDayShippingMethodForCost() {
    $('#cart-shipping-cost').text('$'+ $('option[value="' + $('#sameDayShippingMethods').find('select[id="same-day-shipping-methods-select-box"]')
                    .val()+ '"]').text().split('$')[1]);
    calculateCartTotal();
  }

  function calculateCartTotal() {
    let total = checkNaN(parseFloat($('#cart-shipping-subTotal').text().split('$')[1])) +
                checkNaN(parseFloat($('#cart-shipping-waiver').text().split('$')[1])) +
                checkNaN(parseFloat($('#cart-shipping-cost').text().split('$')[1])) +
                checkNaN(parseFloat($('#cart-shipping-tax').text().split('$')[1]));
    $('#cart-shipping-total').text('$' + total.toFixed(2));
  }

  function checkNaN(attribute) {
    if(isNaN(attribute)) {
        return 0.00;
    } else {
        return attribute;
    }
  }
  
  //checks the product availability for the selected delivery mode
  function checkAvailability(deliveryMode){
  var isAvailable;
 	$.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/checkAvailability',
        async: false,
        data: {
            	deliveryMethod : deliveryMode
            },
        type: "GET",
        success: function (data) {
            if(data=='success') {
          		isAvailable = true;
            }
            else
            {
            	isAvailable = false;
            }
        },
        error: function (error) {
        	isAvailable = false;
			console.log(error);
        }
    });
    return isAvailable;
 }