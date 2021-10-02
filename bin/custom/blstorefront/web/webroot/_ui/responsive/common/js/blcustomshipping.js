 $(document).ready(function() {
    resetSelectBox('ship-it-select-box');
    defaultShipIt();
    hideLabelsFromForm();
    changeUPSStore();
    hideShippingForm();
    hideErrorForInputValidation();
    emptyAddressFormAttributes();
    var isNewGearShipping= $('.js-new-gear-shipping-page').val();
    if(isNewGearShipping == undefined || isNewGearShipping == 'false'){
    shipToHomeShippingMethods();
    }
    $('#ship-it-savedAddresses option').each(function() {
        var optionText = this.text;
        var newOption = optionText.substring(0,52);
        if(screen.width<600){
            var newOption = optionText.substring(0,35);
        }
        if(optionText > optionText.substring(0,52)){
            jQuery(this).text(newOption + '..');
        }
    });
    reverseTraverseOnShipping();
});

function reverseTraverseOnShipping() {
    if(document.getElementById('previousPage').value == "true") {
        var shippingMethodAttr = document.getElementById('shippingMethod').value;
        var shippingGroup = shippingMethodAttr.split("-")[0];
        if(shippingGroup == "SHIP_HOME_HOTEL_BUSINESS") {
            var shippingMethod = shippingMethodAttr.split("-")[1];
            $('#ship-it-shipping-methods-select-box > option').each(function() {
                if(this.value == shippingMethod) {
                    $(this).prop('selected', true).change();
                }
            });
            populateAddress();
        }
    }
}

 function populateAddress() {
    $.ajax({
         url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/checkAddressWithCartAddress',
         type: "GET",
         dataType: 'json',
         async: false,
         beforeSend: function(){
            $('.page-loader-new-layout').show();
         },
         success: function (data) {
             if(data != null) {
                var status = false;
                $('#ship-it-savedAddresses').find('option').each(function() {
                    if(this.value == data.id) {
                        status = true;
                        $(this).prop('selected', true).change();
                    }
                });

                if(!status) {
                    select = document.getElementById('ship-it-savedAddresses');
                    var opt = document.createElement('option');
                    opt.value = data.id;
                    opt.innerHTML = data.line1 + ", " + data.town + ", " + data.country.isocode + ", " + data.postalCode;
                    select.appendChild(opt);
                    $("#ship-it-savedAddresses option[value='"+data.id+"']").attr('selected', 'selected');
                }
             }
         },
         complete: function() {
             $('.page-loader-new-layout').hide();
         },
         error: function (data) {
            if(data != null && data.statusText == 'parsererror') {
                window.location.reload();
            }
            $('.page-loader-new-layout').hide();
         }
     });
 }

 function removeClass(){
    $('.bootstrap-select').on('click', function(){   
       $(this).removeClass('open');
   });
  };

  function hidedropdown(){ 
    $('.container').on('click', function(){        
       $(".sub-option").find(".bootstrap-select").removeClass('open')
   });
  }; 

 $('#ship-it-select-box').change(function () {
   dropdown = $('#ship-it-select-box').val();
   $('.ship-it-tab-content').hide();
   $('#' + "tab-" + dropdown).show();
 });

 $(".newAddressAdd").click(function(){
   $("#ship-it-savedAddresses option[value='newAddress']").attr('selected', 'selected');
 });

 //MainContinueMethod
 function shippingMethodContinue() {
 	$("#validationMessage").empty();
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
    $('#ship-it-am-notification').html('');
    $('#ship-it-am-notification').hide();
    hideErrorForInputValidation();
    $("#ship-it-savedAddresses option[value='newAddress']").removeAttr("selected");
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
 	$("#validationMessage").empty();
    hideErrorForInputValidation();
    $('#ship-it-notification').html("");
    $('#ship-it-am-notification').html("");
    var shippingMethod = $('#ship-it-select-box').val();
    if(shippingMethod == 'SHIP_HOME_HOTEL_BUSINESS') {
        resetSelectBox('ship-it-savedAddresses');
        resetSelectBox('ship-it-shipping-methods-select-box');
        emptyAddressFormAttributes();
        hideShippingForm();
        shipToHomeShippingMethods();
    } else if(shippingMethod == 'SHIP_UPS_OFFICE') {
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
         async: false,
         beforeSend: function(){
            $('.page-loader-new-layout').show();
         },
         success: function (data) {
             if(data != null && data.length != 0) {
                 let shippingModes = '<select id="ship-it-shipping-methods-select-box" class="selectpicker mt-2" '+
                                        'onchange="onChangeOfShipItShipToHome(this)">';
                 let numberSelected = 0;
                 for (let i = 0; i < data.length; i++) {
                     if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                       shippingModes += '<option value="' + data[i].code + '" businesstype="' + data[i].businessTypeDelivery + '" data-subtext="' + data[i].deliveryCost.formattedValue + '">' + data[i].name;
                       shippingModes += '</option>';
                       if(i == 0) {
                          $('#cart-shipping-cost').text(data[i].deliveryCost.formattedValue);
                          calculateCartTotal();
                          if(data[i].businessTypeDelivery == true) {
                             let notification = '<div class="notification notification-warning">AM delivery is only available to business addresses. Not at the office? Select Ship and Hold at a UPS Store for AM delivery options!</div>';
                             $('#ship-it-am-notification').html(notification);
                             $('#ship-it-am-notification').show();
                          }
                       }
                     }
                 }
                 
                 shippingModes += '</select>';
                 $('#shipToHomeShippingMethods').html(shippingModes);
                 $('.selectpicker').selectpicker('refresh');
                 removeClass();
                 hidedropdown();
             } 
           
            else {
             	 $('#cart-shipping-cost').text('-');
                 showErrorNotification('Rental Dates not eligible for the selected shipping option!!', false);
             }
         },
         complete: function() {
             $('.page-loader-new-layout').hide();
         },
         error: function (data) {
            if(data != null && data.statusText == 'parsererror') {
                window.location.reload();
            }
            $('.page-loader-new-layout').hide();
         }
     });
  }

 function shipToHomeShippingContinue(shippingMethod) {
      hideErrorForInputValidation();
      var savedAddress = null;
      var deliveryMode = $('#shipToHomeShippingMethods').find('select[id="ship-it-shipping-methods-select-box"]').val();
      var businessType = $('#shipToHomeShippingMethods').find('select[id="ship-it-shipping-methods-select-box"]').find(':selected').attr('businesstype');
      if(typeof businessType == "string") {
        businessType = JSON.parse(businessType);
      }
	  if(checkShippingBlackout(deliveryMode))
	  {
	  	var validationDiv = $('<div class="notification notification-error mb-4" />').html(ACC.blackoutError.blockedShipping);
		$('#validationMessage').append(validationDiv);
	  }
      else if(checkAvailability(deliveryMode))
      {
          if($('#delivery-shippingAddressFormDiv').css('display') == "none") {
              saveSelectedAddress($('select[id="ship-it-savedAddresses"]').val(), 'SHIP_HOME_HOTEL_BUSINESS', deliveryMode, null, businessType);
          } else {
              var firstName = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]');
              var lastName = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]');
              var companyName = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.companyName"]');
              var line1 = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]');
              var line2 = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]');
              var townCity = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]');
              var postcode = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]');
              var regionIso = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]');
              var email = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]');
              var phone = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]');
              if(validateFormData(firstName, lastName, line1, townCity, postcode, regionIso, email, phone, "Ship")) {
                  if($('#showErrorForInvalidZipInputValidation').css('display') == "none" &&
                        $('#showErrorForInvalidEmailInputValidation').css('display') == "none" &&
                        $('#showErrorForInvalidPhoneInputValidation').css('display') == "none") {
                  addressValidationService(createAddressFormObject(firstName.val(), lastName.val(), companyName.val(), line1.val(), line2.val(), townCity.val(),regionIso.val(),
                                                                     'US', postcode.val(), $('.ship-it-tab-content').find('input[id="ship-it-save-address"]').prop("checked"),
                                                                     phone.val(), email.val(), false, null, 'UNKNOWN'), deliveryMode, 'SHIP', businessType);
                  }
              } else {
                  showErrorForInputValidation('Ship');
              }
          }

       // track Tealium event on continue shipping.
       utag.link({
             "tealium_event"    : "continue_shipping_click",
             "shipping_method"   : "Ship It-Ship to home",
             "shipping_method_not_available"     : "0"
         });
        ACC.track.trackShippingSelection('Ship It','Ship to home','Item In Stock');
      }
      else
      {
        // track Tealium event on continue shipping.
//              utag.link({
//              "tealium_event"    : "continue_shipping_click",
//              "shipping_method"   : "Ship It-Ship to home",
//              "shipping_method_not_available"     : "1"
//               });
        ACC.track.trackShippingSelection('Ship It','Ship to home','Item Out of Stock');
      	window.location.reload();
      }
      
  }

 //UPS-Store
 function fetchUPSDeliveryMethods() {
    $.ajax({
        url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/chooseShippingDelivery',
        data: {
            shippingGroup: "SHIP_UPS_OFFICE",
            partnerZone: null
        },
        type: "GET",
        dataType: 'json',
        beforeSend: function(){
            $('.page-loader-new-layout').show();
        },
        success: function (data) {
            if(data != null && data.length != 0) {
                let shippingModes = '<b class="mt-4">Shipping Method</b>';
                shippingModes += '<select id="ship-UPS-shipping-methods-select-box" class="selectpicker mt-2"' +
                                    'onchange="onChangeOfShipItShipToUPS()">';
                let numberSelected = 0;
                for (let i = 0; i < data.length; i++) {
                    if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                        shippingModes += '<option value="' + data[i].code + '" businesstype="' + data[i].businessTypeDelivery + '" data-subtext="' + data[i].deliveryCost.formattedValue + '">' + data[i].name;
                        shippingModes += '</option>';
                        if(i == 0) {
                            $('#cart-shipping-cost').text(data[i].deliveryCost.formattedValue);
                            calculateCartTotal();
                            if(data[i].businessTypeDelivery == true) {
                                let notification = '<div class="notification notification-warning">AM delivery is only available to business addresses. Not at the office? Select Ship and Hold at a UPS Store for AM delivery options!</div>';
                                $('#ship-it-am-notification').html(notification);
                                $('#ship-it-am-notification').show();
                            }
                        }
                    }
                }
                shippingModes += '</select>';
                $('#shipToUPSShippingMethods').html(shippingModes);
                $('.selectpicker').selectpicker('refresh');
                removeClass();
                hidedropdown();
                $('#checkZipForUPSPickup').show();
            } else {
            	$('#cart-shipping-cost').text('-');
                showErrorNotification('Rental Dates not eligible for the selected shipping option!!', false);
                $('#checkZipForUPSPickup').hide();
            }
        },
        complete: function() {
            $('.page-loader-new-layout').hide();
        },
        error: function (data) {
            if(data != null && data.statusText == 'parsererror') {
                window.location.reload();
            }
            $('.page-loader-new-layout').hide();
        }
    });
 }

 function shipToUPSStoreLocationContinue(shippingMethod) {
     if($('#changeUPSStoreButton').is(":visible")) {
//         utag.link({
//           "tealium_event"    : "continue_shipping_click",
//           "shipping_method"   : "Ship It-Ship to UPS",
//           "shipping_method_not_available"     : "0"
//         });
         ACC.track.trackShippingSelection('Ship It','Ship to UPS','Item In Stock');
         var deliveryMethod = $('#shipToUPSShippingMethods').find('#ship-UPS-shipping-methods-select-box').val();
         if(checkShippingBlackout(deliveryMethod))
{
	var validationDiv = $('<div class="notification notification-error mb-4" />').html(ACC.blackoutError.blockedShipping);
	$('#validationMessage').append(validationDiv);
}
else
{
	addNewAddress(createUPSStoreAddress(), deliveryMethod)
             .then((data) => {
                 saveDeliveryMode(deliveryMethod, false)
                     .then((data) => {
                         $('.page-loader-new-layout').hide();
                         window.location = ACC.config.encodedContextPath + '/checkout/multi/delivery-method/next';
                     })
                     .catch((error) => {
                       console.log(error)
                     })
             })
             .catch((error) => {
               console.log(error)
             })
}         
     } else {
        showErrorForUPSOrPickAddressError();
     }
     //shipping by someone form data
     if($('#ship-it-pickup-gear').find('#pickup-person').find('input[id="store-pickup-other"]').prop("checked")) {
         if($("#ship-it-pickup-person").css('display') == 'block') {
             var firstName = $("#ship-it-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]');
             var lastName = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]');
             var email = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]');
             var phone = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]');
             if(validateFormData(firstName, lastName, null, null, null, null, email, phone, "UPS")) {
                 savePickUpByFormOnCart(createPickUPFormObject(firstName.val(), lastName.val(), email.val(), phone.val()),
                        $('#shipToUPSShippingMethods').find('#ship-UPS-shipping-methods-select-box').val(), false, createUPSStoreAddress());
             } else {
                  showErrorForInputValidationPick('Ship');
             }
         }
     } else {
//         savePickUpByFormOnCart(createPickUPFormObject(null, null, null, null), $('#shipToUPSShippingMethods')
//                         .find('#ship-UPS-shipping-methods-select-box').val(), false, createUPSStoreAddress());
    	 var firstName = $("#ship-it-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]');
         var lastName = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]');
         var email = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]');
         var phone = $('#ship-it-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]');
         if(validateFormData(firstName, lastName, null, null, null, null, email, phone, "UPS")) {
             savePickUpByFormOnCart(createPickUPFormObject(firstName.val(), lastName.val(), email.val(), phone.val()),
                    $('#shipToUPSShippingMethods').find('#ship-UPS-shipping-methods-select-box').val(), false, createUPSStoreAddress());
         } else {
              showErrorForInputValidationPick('Ship');
         }
     }
 }

 function onClickOfFindStore() {
     $('#showErrorForInputValidation').html('');
     $('#showErrorForInputValidation').hide();
     $('#ship-it-SHIP_UPS_OFFICE').html('');
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
                                               upsStores +=  data.result[i].distance.value + ' ' +data.result[i].distance.unitCode + ' • ';
                                           }
                                           if(data.result[i].distance != null) {
                                               upsStores +=  data.result[i].contactNumber;
                                           }
                                           upsStores += '</p>' + '</div>' + '<div class="col-11 offset-1 col-md-4 offset-md-0">';
                                                  if(data.result[i].latestGroundDropOffTime != null && data.result[i].latestGroundDropOffTime.length != 0) {
                                                        if(data.result[i].latestGroundDropOffTime[0] != null && data.result[i].latestGroundDropOffTime[0].split(': ')[0] == 'Mon-Fri') {
                                         upsStores += '<p class="mb-0"><span class="gray80">M-F</span>&emsp;' + data.result[i].latestGroundDropOffTime[0].split(': ')[1] + '</p>' ;
                                                        } else {
                                         upsStores += '<p class="mb-0"><span class="gray80">M-S</span>&emsp;' + data.result[i].latestGroundDropOffTime[0].split(': ')[1] + '</p>' ;
                                                        }
                                                      if(data.result[i].latestGroundDropOffTime[1] != null) {
                                         upsStores += '<p class="mb-0"><span class="gray80">' +data.result[i].latestGroundDropOffTime[1].split(':')[0] + '</span>&emsp;&nbsp;' +
                                                                                 data.result[i].latestGroundDropOffTime[1].split(': ')[1] + '</p>' ;
                                                      }
                                                      if(data.result[i].latestGroundDropOffTime[2] != null) {
                                         upsStores += '<p class="mb-0"><span class="gray80">' + data.result[i].latestGroundDropOffTime[2].split(':')[0] + '</span>&emsp;' +
                                                                                 data.result[i].latestGroundDropOffTime[2].split(': ')[1] + '</p>';
                                                      }
                                                  }
                                    upsStores += '</div>' +
                                             '</div>';
                         }
                         $('#ship-it-SHIP_UPS_OFFICE').html(upsStores);
                         if(data.length == 1) {
                             $('#ship-it-SHIP_UPS_OFFICE #ups-location-1').first().find('input[name="ups-location"]').prop("checked", true);
                         }
                     }
                 } else {
                     showErrorNotification('Whoops! Something went wrong, please try again to Find UPS store later.', false);
                 }
             },
             complete: function() {
                 $('.page-loader-new-layout').hide();
             },
             error: function (data) {
                if(data != null && data.statusText == 'parsererror') {
                    window.location.reload();
                }
                 $('.page-loader-new-layout').hide();
             }
         });
     } else {
         showErrorNotification('Please enter a valid zipcode.', false);
         $('.page-loader-new-layout').hide();
     }
  }

 function onSelectOfUPSStore(upsSelectedStoreId) {
     $('#showErrorForUPSOrPickAddressError').html('');
     $('#showErrorForUPSOrPickAddressError').hide();
     let stores = JSON.parse(sessionStorage.getItem("UPSStores"));
     if(stores != null && stores.length != 0) {
         for (let i = 0; i < stores.length; i++) {
            let upsStores = '';
             if(stores[i].locationId == upsSelectedStoreId) {
                $('#ship-it-SHIP_UPS_OFFICE').html('');
                upsStores += '<div id="ups-location-1" class="row store-location mb-3">' +
                                  '<div class="col-1 on-select-hide">' +
                                      '<input type="hidden" id="' + stores[i].locationId + '" name="ups-location"><label for="' +
                                         stores[i].locationId + '"></label>' +
                                  '</div>' +
                                  '<div class="col-11 col-md-8 mt-5">' +
                                      '<p>' + stores[i].consigneeName + '<br>' +
                                      '<a href="https://maps.google.com/maps?q='+
                                          stores[i].addressLine + ',' + stores[i].politicalDivision1 + ' ' +
                                          stores[i].politicalDivision2 + ' ' + stores[i].postcodePrimaryLow +
                                         '" target="_blank">' +
                                             stores[i].addressLine + ',' + stores[i].politicalDivision1 + ' ' +
                                             stores[i].politicalDivision2 + ' ' + stores[i].postcodePrimaryLow +
                                         '</a><br>' ;
                                         if(stores[i].distance != null) {
                                            upsStores +=  stores[i].distance.value + ' ' +stores[i].distance.unitCode + ' • ';
                                         }
                                         if(stores[i].distance != null) {
                                             upsStores += stores[i].contactNumber;
                                         }
                        upsStores += '</p>' + '</div>' + '<div class="col-11 offset-1 col-md-4 offset-md-0 mt-5">';
                                  if(stores[i].latestGroundDropOffTime != null && stores[i].latestGroundDropOffTime.length != 0) {
                                                      if(stores[i].latestGroundDropOffTime[0] != null && stores[i].latestGroundDropOffTime[0].split(': ')[0] == 'Mon-Fri') {
                                       upsStores += '<p class="mb-0"><span class="gray80">M-F</span>&emsp;' + stores[i].latestGroundDropOffTime[0].split(': ')[1] + '</p>' ;
                                                      } else {
                                       upsStores += '<p class="mb-0"><span class="gray80">M-S</span>&emsp;' + stores[i].latestGroundDropOffTime[0].split(': ')[1] + '</p>' ;
                                                      }
                                                    if(stores[i].latestGroundDropOffTime[1] != null) {
                                       upsStores += '<p class="mb-0"><span class="gray80">' +stores[i].latestGroundDropOffTime[1].split(':')[0] + '</span>&emsp;&nbsp;' +
                                                                               stores[i].latestGroundDropOffTime[1].split(': ')[1] + '</p>' ;
                                                    }
                                                    if(stores[i].latestGroundDropOffTime[2] != null) {
                                       upsStores += '<p class="mb-0"><span class="gray80">' + stores[i].latestGroundDropOffTime[2].split(':')[0] + '</span>&emsp;' +
                                                                               stores[i].latestGroundDropOffTime[2].split(': ')[1] + '</p>';
                                                    }
                                  }
                    upsStores += '</div>' +
                             '</div>';
                $('#ship-it-SHIP_UPS_OFFICE').html(upsStores);
                $('#changeUPSStoreButton').show();
              //  $('#ship-it-pickup-gear').show();
                $('#ship-it-pickup-person').show();
                $('#ship-it-ups-zip-code').val('');
                $('#checkZipForUPSPickup').hide();
                $('.on-select-hide').hide();
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
                 return createAddressFormObject(stores[i].consigneeName, "UPS", "", stores[i].addressLine, null, stores[i].politicalDivision2,
                         stores[i].politicalDivision1, stores[i].countryCode, stores[i].postcodePrimaryLow, false, stores[i].contactNumber, null, true,
                         stores[i].latestGroundDropOffTime, 'BUSINESS')
             }
         }
     }
  }

 function changeUPSStore() {
 	 $("#validationMessage").empty();
     $('#showErrorForUPSOrPickAddressError').html('');
     $('#showErrorForUPSOrPickAddressError').hide();
     $('#showErrorForInputValidation').html('');
     $('#showErrorForInputValidation').hide();
     $('#checkZipForUPSPickup').show();
     $('#ship-it-ups-zip-code').val('');
     $('#changeUPSStoreButton').hide();
     $('#ship-it-pickup-gear').hide();
     $("#ship-it-pickup-person").hide();
     $('#ship-it-SHIP_UPS_OFFICE').html("");
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
     if($('#shipToUPSShippingMethods').find('#ship-UPS-shipping-methods-select-box').find(':selected').attr('businesstype') == "false") {
        $('#ship-it-am-notification').html('');
        $('#ship-it-am-notification').hide();
     }
 }

 function createPickUPFormObject(firstName, lastName, email, phone) {
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
     hideErrorForInputValidation();
     resetSelectBox('pick-up-select-box');
     resetPartnerPickUpSection();
     $('#pickup-me').prop("checked", true);
     $('#pick-up-notification').val('');
 });

 function pickUpPartnerLocationContinue(shippingMethod) {
//     if($('#pick-up-pickup-gear').find('#pickup-person').find('input[id="pickup-other"]').prop("checked")) {
//         if($("#store-pickup-person").css('display') == 'block') {
             var firstName = $("#store-pickup-person #blPickUpByForm").find('.form-group').find('input[id="blPickUpBy.firstName"]');
             var lastName = $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.lastName"]');
             var email = $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.email"]');
             var phone = $('#store-pickup-person #blPickUpByForm').find('.form-group').find('input[id="blPickUpBy.phone"]');
             if(validateFormData(firstName, lastName, null, null, null, null, email, phone, "Pick")) {
                   savePickUpByFormOnCart(createPickUPFormObject(firstName.val(), lastName.val(), email.val(), phone.val()),
                             $('#partnerPickUpShippingMethods #pickup-nyc').find('input[name="pickup-locations"]:checked').attr('id'), true, null);
             } else {
                  showErrorForInputValidationPick('Pick');
             }
//         }
//     } else {
//           savePickUpByFormOnCart(createPickUPFormObject(null, null, null, null),
//                 $('#partnerPickUpShippingMethods #pickup-nyc').find('input[name="pickup-locations"]:checked').attr('id'), true, null);
//     }

     if($('#partnerPickUpShippingMethods #pickup-nyc').find('input[name="pickup-locations"]:checked').attr('id') != undefined) {
        // track Tealium event on continue shipping.
//        utag.link({
//         "tealium_event"    : "continue_shipping_click",
//         "shipping_method"   : "PickUP",
//         "shipping_method_not_available"     : "0"
//        });
        ACC.track.trackShippingSelection('PickUP','','Item In Stock');
        var deliveryMode = $('#partnerPickUpShippingMethods #pickup-nyc').find('input[name="pickup-locations"]:checked').attr('id');
        if(checkShippingBlackout(deliveryMode))
{
	var validationDiv = $('<div class="notification notification-error mb-4" />').html(ACC.blackoutError.blockedShipping);
	$('#validationMessage').append(validationDiv);
}
else
{
	saveDeliveryMode(deliveryMode, true)
            .then((data) => {
                $('.page-loader-new-layout').hide();
                window.location = ACC.config.encodedContextPath + '/checkout/multi/delivery-method/next';
            })
            .catch((error) => {
              console.log(error)
            })
}        
     } else {
        showErrorForUPSOrPickAddressError();
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
     $('#showErrorForInputValidation').html('');
     $('#showErrorForInputValidation').hide();
 }

 function showPickUpBySomeoneForm() {
    $("#ship-it-pickup-person").show();
    $('#showErrorForInputValidation').html('');
    $('#showErrorForInputValidation').hide();
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
     $('#showErrorForInputValidation').html('');
     $('#showErrorForInputValidation').hide();
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
            if(data != null && data.length != 0 && (typeof data == "object")) {
                let partnerDelivery = '';
                for (let i = 0; i < data.length; i++) {
                    if(i == 0 && data.length == 1) {
                        if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                            $('#cart-shipping-cost').text(data[i].deliveryCost.formattedValue);
                            calculateCartTotal();
                        }
                    } else {
                        $('#cart-shipping-cost').text('-');
                        calculateCartTotal();
                    }
                    partnerDelivery += '<div id="pickup-nyc" class="row store-location mb-3">' +
                                            '<div class="col-1">' +
                                                '<input type="radio" id="' +
                                                data[i].code +
                                                '" name="pickup-locations"><label for="' + data[i].code + '" onClick="onSelectOfPartnerAddress(' + data[i].code + ')"></label>';
                         partnerDelivery += '</div>' +
                                            '<div class="col-11">' +
                                                '<p>' + data[i].name + '- <span id="' + data[i].code + '-pickUpCost">' ;
                                                if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                                 partnerDelivery += data[i].deliveryCost.formattedValue ;
                                                }
                         if(data[i].internalStoreAddress != null) {
                                 partnerDelivery += ' </span><br>' +
                                                    '<a href="' + data[i].internalStoreAddress.url + '" target="_blank">' +
                                                        data[i].internalStoreAddress.formattedAddress +
                                                    '</a><br>';

                             if(data[i].internalStoreAddress.phone != null) {
                                 partnerDelivery += data[i].internalStoreAddress.phone;
                             }
                            partnerDelivery += '</p>' ;
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
                }
                $('#partnerPickUpShippingMethods').html(partnerDelivery);
                // $('#pick-up-pickup-gear').show();
                $('#store-pickup-person').show();
                if(data.length == 1) {
                    $('#partnerPickUpShippingMethods #pickup-nyc').first().find('input[name="pickup-locations"]').prop("checked", true);
                }
                showErrorNotificationForPickUpId('They must show ID at time of pickup');
            } else {
            	$('#cart-shipping-cost').text('-');
            	 calculateCartTotal();
            	 if($('.js-new-gear-shipping-page').val() == 'true'){
            	  showErrorNotificationPickUp('Selected shipping option not available for new gear pickup!!');
            	 }else {
                showErrorNotificationPickUp('Rental Dates not eligible for the selected shipping option!!');
                }
            }
        },
        complete: function(data) {
            if((typeof data == "string")) {
                window.location.reload();
            }
            if(data != null && (typeof data != "string") && data.statusText == 'parsererror') {
                window.location.reload();
            }
            $('.page-loader-new-layout').hide();
        },
        error: function (error) {
            $('.page-loader-new-layout').hide();
        }
    });
 }

 function onAddNewAddressClicked() {
    hideErrorForInputValidation();
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
    $("#ship-it-savedAddresses option[value='newAddress']").removeAttr("selected");
    if($('input[name="shipProduct"]:checked').attr('id') == 'ship-it') {
        $('#delivery-shippingAddressFormDiv').hide();
        $('#ship-it-save-address-div').hide();
        $('#ship-it-save-address-div #ship-it-save-address').prop("checked", true);
        emptyAddressFormAttributes();
        if($('#shipToHomeShippingMethods').html() != '') {
            $('#ship-it-notification').val('');
            $('#ship-it-notification').hide();
        }

    } else {
        $('#same-day-address-div #delivery-shippingAddressFormDiv').hide();
        $('#same-day-save-address-div').hide();
        $('#same-day-save-address-div #same-day-save-address').prop("checked", false);
        $('#same-day-status-updates-div').hide();
        emptySFOrNYCAddressForm();
        $('#same-day-notification').val('');
        $('#same-day-notification').hide();
    }
    hideErrorForInputValidation();
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
     showErrorNotificationForPickUpId('They must show ID at time of pickup');
 }

 function pickUpByMeClick() {
    $("#store-pickup-person").hide();
    $('#showErrorForInputValidation').html('');
    $('#showErrorForInputValidation').hide();
 }

  function onSelectPartnerPickup(event) {
  	 $("#validationMessage").empty();
     resetPartnerPickUpSection();
     showPartnerPickUpDeliveryModes(event.value);
     $('#showErrorForUPSOrPickAddressError').html('');
     $('#showErrorForUPSOrPickAddressError').hide();
  }

  //SF Or NYC
  $("input[id='sameday']").click(function() {
      hideErrorForInputValidation();
      $("#ship-it-savedAddresses option[value='newAddress']").removeAttr("selected");
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
      $('#cart-shipping-cost').text('-');
      calculateCartTotal();
  });

  function onChangeOfSameDayShippingMethod() {
       hideErrorForInputValidation();
       $('#same-day-notification').val('');
       $('#same-day-notification').hide('');
       $('#same-day-address-div').hide();
       emptySFOrNYCAddressForm();
       $('#sameDayShippingMethodsNotification').hide();
       $('#sameDayShippingMethods').html('');
       $('#sameDayZipCheckText').val('');
  }

  $("#sameDayZipCheckText").on('change keydown paste input', function(){
    if(JSON.parse(sessionStorage.getItem("currentZIP")) != $('#sameDayZipCheckText').val().trim()) {
        hideErrorForInputValidation();
        $('#same-day-notification').val('');
        $('#same-day-notification').hide();
        $('#same-day-address-div').hide();
        $('#sameDayShippingMethodsNotification').hide();
        $('#sameDayShippingMethods').html('');
    }
  });

  function sameDayZipCheck() {
    sessionStorage.setItem("currentZIP", JSON.stringify($('#sameDayZipCheckText').val()));
    hideErrorForInputValidation();
    $('#same-day-notification').val('');
    $('#same-day-notification').hide();
    $('#same-day-address-div').hide();
    $('#sameDayShippingMethodsNotification').hide();
    $('#sameDayShippingMethods').html('');
    if(validateZip($('#sameDayZipCheckText').val())) {
        $.ajax({
            url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/checkValidZipFromFedexService',
            data: {
                pinCode: $('#sameDayZipCheckText').val().trim(),
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
                           if(data != null && data.length != 0 && (typeof data == "object")) {
                               let sameDayShippingModes = '<b>Delivery Window</b>';
                               sameDayShippingModes += '<select id="same-day-shipping-methods-select-box" class="selectpicker mt-2"' +
                                                        ' onChange="onChangeOfSameDayShippingMethodForCost()">';
                                                        
                               let numberSelected = 0;
                               for (let i = 0; i < data.length; i++) {
                                    if(i == 0) {
                                        if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                                            $('#cart-shipping-cost').text(data[i].deliveryCost.formattedValue);
                                            calculateCartTotal();
                                        }
                                    }
                                    var selectionText = data[i].name.split("_");
                                   if(data[i].deliveryCost != null && data[i].deliveryCost.formattedValue != null) {
                                       sameDayShippingModes += '<option value="' + data[i].code + '" data-subtext="' + data[i].deliveryCost.formattedValue + '">' + selectionText[0];
                                       sameDayShippingModes;
                                   }
                                   sameDayShippingModes += '</option>';
                               }
                               sameDayShippingModes += '</select>';
                               $('#sameDayShippingMethods').html(sameDayShippingModes);
                               $('.selectpicker').selectpicker('refresh');
                               removeClass();
                               hidedropdown();
                               $('#sameDayShippingMethodsNotification').show();
                               $('#same-day-address-div').show();
                               if($('#same-day-address-div #delivery-saved-addresses-dropdown').length == 1) {
                                   $('#same-day-save-address-div').hide();
                                   $('#same-day-save-address-div #same-day-save-address').prop("checked", false);
                                   $('#same-day-status-updates-div').hide();
                                   $('#same-day-status-updates-div #same-day-status-updates').prop("checked", false);
                               } else {
                                    $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]')
                                        .val($('#sameDayZipCheckText').val());
                                    $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]')
                                        .prop("disabled", true);
                               }
                           } else {
                           	   $('#cart-shipping-cost').text('-');
                               showErrorNotificationSameDay('No delivery windows are available for this date. Please change your shipping method or rental date to continue.', false);
                           }
                       },
                       complete: function() {
                            if((typeof data == "string")) {
                                window.location.reload();
                            }
                           $('.page-loader-new-layout').hide();
                       },
                       error: function (data) {
                            if(data != null && data.statusText == 'parsererror') {
                                window.location.reload();
                            }
                           $('.page-loader-new-layout').hide();
                       }
                   });
                } else {
                    showErrorNotificationSameDay('Whoops! We were unable to get shipping information back from FedEx, please change your shipping method or try again in a few minutes', false);
                }
            },
            complete: function(data) {
                if(data != null && data.statusText == 'parsererror') {
                    window.location.reload();
                }
                $('.page-loader-new-layout').hide();
            },
            error: function (data) {
                if(data != null && data.statusText == 'parsererror') {
                    window.location.reload();
                }
                $('.page-loader-new-layout').hide();
            }
        });
    } else {
        showErrorNotificationSameDay('Sorry, Same Day delivery is not available for your zipcode!', false);
        $('.page-loader-new-layout').hide();
    }
  }

  function SFOrNYCShippingSectionContinue() {
    hideErrorForInputValidation();
    $('#same-day-notification').val('');
    $('#same-day-notification').hide();
    var savedAddress = null;
    var sameDayDeliveryNote = $('#sameDayDeliveryNote').val();
    var deliveryMode = $('#sameDayShippingMethods').find('select[id="same-day-shipping-methods-select-box"]').val();
    if(checkShippingBlackout(deliveryMode))
	  {
	  	var validationDiv = $('<div class="notification notification-error mb-4" />').html(ACC.blackoutError.blockedShipping);
		$('#validationMessage').append(validationDiv);
	  }
    else if(checkAvailability(deliveryMode))
    {
        // track Tealium event on continue shipping.
//           utag.link({
//           "tealium_event"    : "continue_shipping_click",
//           "shipping_method"   : "Same Day Delivery",
//           "shipping_method_not_available"     : "0"
//           });
        ACC.track.trackShippingSelection('Same Day Delivery','','Item In Stock');
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
                        saveSelectedAddress(savedAddress, $('#same-day-select-box').val(), deliveryMode, $('#sameDayZipCheckText').val(), false);
                    }
               },
               error: function (data) {
                    $('.page-loader-new-layout').hide();
               }
            });
        } else {
            var firstName = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]');
            var lastName = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]');
            var companyName = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.companyName"]');
            var line1 = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]');
            var line2 = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]');
            var townCity = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]');
            var postcode = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]');
            var regionIso = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]');
            var email = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]');
            var phone = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]');
            if(validateFormData(firstName, lastName, line1, townCity, postcode, regionIso, email, phone, "Rush")) {
                if($('#showErrorForInvalidZipInputValidation').css('display') == "none" &&
                    $('#showErrorForInvalidEmailInputValidation').css('display') == "none" &&
                    $('#showErrorForInvalidPhoneInputValidation').css('display') == "none") {
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
                                addressValidationService(createAddressFormObject(firstName.val(), lastName.val(), companyName.val(), line1.val(), line2.val(),
                                                            townCity.val(),regionIso.val(), 'US', postcode.val(),
                                                            $('#same-day-address-div').find('input[id="same-day-save-address"]').prop("checked"),
                                                            phone.val(), email.val(), false, null, 'UNKNOWN'), deliveryMode, 'RUSH', null);
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
            } else {
                showErrorForInputValidation('Rush');
                $('.page-loader-new-layout').hide();
            }
        }
      }
      else
      {
       // track Tealium event on continue shipping.
          utag.link({
          "tealium_event"    : "continue_shipping_click",
          "shipping_method"   : "Same Day Delivery",
          "shipping_method_not_available"     : "1"
         });
       ACC.track.trackShippingSelection('Same Day Delivery','','Item Out of Stock');
      	window.location.reload();
    }
  }

  function emptySFOrNYCAddressForm() {
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]').removeClass('error');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.companyName"]').val('');
        $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.companyName"]').removeClass('error');
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
 function showErrorForInputValidation(section) {
    let notification = '';
    var len = 0;
    if(section == 'Ship') {
        $($('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group .error')).each(function(k, v) {
            if(v.value == "") {
                len = len + 1;
            }
        });
        notification += '<div class="notification notification-error" style="margin-top: 0px;"> You are missing ' + len + ' required fields.' +
                            '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForError(this)"> Scroll up.</a>';
    } else {
        $($('#same-day-address-div #delivery-shippingAddressFormDiv #addressForm').find('.form-group .error')).each(function(k, v) {
            if(v.value == "") {
                len = len + 1;
            }
        });
        notification += '<div class="notification notification-error" style="margin-top: 0px;"> You are missing ' + len + ' required fields.' +
                            '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForError(this)"> Scroll up.</a>';
    }
    notification += '</div>' + '<br>';
    $('#showErrorForInputValidation').html(notification);
    $('#showErrorForInputValidation').show();
 }

 function showErrorForZipInputInvalidValidation(section) {
     let notification = '';
     if(section == 'Ship') {
         notification += '<div class="notification notification-error" style="margin-top: 0px;"> You have entered invalid Zip ' +
                             '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForInvalidError(this, \'postcode\')">' +
                              'Scroll up.</a>';
     } else {
         notification += '<div class="notification notification-error" style="margin-top: 0px;"> You have entered invalid Zip ' +
                             '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForInvalidError(this, \'postcode\')">' +
                             ' Scroll up.</a>';
     }
     notification += '</div>' + '<br>';
     $('#showErrorForInvalidZipInputValidation').html(notification);
     $('#showErrorForInvalidZipInputValidation').show();
  }

 function showErrorForEmailInputInvalidValidation(section) {
     let notification = '';
     if(section == 'Ship') {
         notification += '<div class="notification notification-error" style="margin-top: 0px;"> You have entered invalid Email' +
                             '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForInvalidError(this,\'email\')">' +
                             ' Scroll up.</a>';
     } else {
         notification += '<div class="notification notification-error" style="margin-top: 0px;"> You have entered invalid Email' +
                             '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForInvalidError(this,\'email\')">' +
                             ' Scroll up.</a>';
     }
     notification += '</div>' + '<br>';
     $('#showErrorForInvalidEmailInputValidation').html(notification);
     $('#showErrorForInvalidEmailInputValidation').show();
 }

 function showErrorForPhoneInputInvalidValidation(section) {
     let notification = '';
     if(section == 'Ship') {
         notification += '<div class="notification notification-error" style="margin-top: 0px;"> You have entered invalid Phone Number' +
                             '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForInvalidError(this, \'phone\' )">' +
                             ' Scroll up.</a>';
     } else {
         notification += '<div class="notification notification-error" style="margin-top: 0px;"> You have entered invalid Phone Number' +
                             '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForInvalidError(this, \'phone\' )">' +
                             ' Scroll up.</a>';
     }
     notification += '</div>' + '<br>';
     $('#showErrorForInvalidPhoneInputValidation').html(notification);
     $('#showErrorForInvalidPhoneInputValidation').show();
 }

 function scrollUpForError(event) {
     if(event.getAttribute('class') == 'Ship') {
         return $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group .error')[0].scrollIntoView(true);
     } else {
         return $('#same-day-address-div #delivery-shippingAddressFormDiv #addressForm').find('.form-group .error')[0].scrollIntoView(true);
     }
  }

  function scrollUpForInvalidError(event, fieldName) {
       if(event.getAttribute('class') == 'Ship') {
           return $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group')
                    .find('input[id="address.'+fieldName+'"]')[0].scrollIntoView(true);
       } else {
           return $('#same-day-address-div #delivery-shippingAddressFormDiv #addressForm').find('.form-group')
                    .find('input[id="address.'+fieldName+'"]')[0].scrollIntoView(true);
       }
    }

    function showErrorForUPSOrPickAddressError() {
         let notification = '';
         notification += '<div class="notification notification-error"> You need to select a Pickup location';
         notification += '</div>' + '<br>';
         $('#showErrorForUPSOrPickAddressError').html(notification);
         $('#showErrorForUPSOrPickAddressError').show();
     }

  function showErrorForInputValidationPick(section) {
     let notification = '';
     var len = 0;
     if(section == 'Ship') {
        $($("#ship-it-pickup-person #blPickUpByForm").find('.form-group .error')).each(function(k, v) {
            if(v.value == "") {
                len = len + 1;
            }
        });
         notification += '<div class="notification notification-error"> You are missing ' + len + ' required fields.' +
                             '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForErrorPick(this)"> Scroll up.</a>';
     } else {
        $($("#store-pickup-person #blPickUpByForm").find('.form-group .error')).each(function(k, v) {
            if(v.value == "") {
                len = len + 1;
            }
        });
        notification += '<div class="notification notification-error"> You are missing ' + len + ' required fields.' +
                             '<a href="javascript:void(0)" class="'+ section +'" onClick="return scrollUpForErrorPick(this)"> Scroll up.</a>';
     }
     notification += '</div>' + '<br>';
     $('#showErrorForInputValidation').html(notification);
     $('#showErrorForInputValidation').show();
  }

 function scrollUpForErrorPick(event) {
    if(event.getAttribute('class') == 'Ship') {
        return $("#ship-it-pickup-person #blPickUpByForm").find('.form-group .error')[0].scrollIntoView(true);
    } else {
        return $("#store-pickup-person #blPickUpByForm").find('.form-group .error')[0].scrollIntoView(true);
    }
 }

 function hideErrorForInputValidation() {
     $('#showErrorForInputValidation').html('');
     $('#showErrorForInputValidation').hide();
     $('#showErrorForInvalidZipInputValidation').html('');
     $('#showErrorForInvalidZipInputValidation').hide();
     $('#showErrorForInvalidEmailInputValidation').html('');
     $('#showErrorForInvalidEmailInputValidation').hide();
     $('#showErrorForInvalidPhoneInputValidation').html('');
     $('#showErrorForInvalidPhoneInputValidation').hide();
     $('#showErrorForUPSOrPickAddressError').html('');
     $('#showErrorForUPSOrPickAddressError').hide();
 }

 function showErrorNotificationForPickUpId(msg, status) {
     let notification = '<div class="notification notification-warning">' + msg + '</div>';
     $('#pick-up-notification').html(notification);
     $('#pick-up-notification').show();
 }

 function showErrorNotification(msg, status) {
    let notification = '<div class="notification notification-error">' + msg + '</div>';
    $('#ship-it-notification').html(notification);
    $('#ship-it-notification').show();
 }

 function showErrorNotificationPickUp(msg) {
     let notification = '<div class="notification notification-error">' + msg + '</div>';
     $('#pick-up-notification').html(notification);
     $('#pick-up-notification').show();
 }

 function showErrorNotificationSameDay(msg, status) {
    let notification = '<div class="notification notification-error">' + msg + '</div>';
    $('#same-day-notification').html(notification);
    $('#same-day-notification').show();
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
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.companyName"]').val('');
     $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.companyName"]').removeClass('error');
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

 function validateFormData(firstName, lastName, line1, townCity, postcode, regionIso, email, phone, section) {
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
        postcodeStatus = validateZip(postcode.val());
        if(!postcodeStatus) {
            if(postcode.val().length > 0) {
                postcode.addClass('error');
                showErrorForZipInputInvalidValidation(section);
                postcodeStatus = true;
            } else {
                postcode.addClass('error');
            }
        } else {
            postcode.removeClass('error');
        }
    }
    
    if(regionIso != null) {
        regionIsoStatus = validateField(regionIso.val(), regionIso);
    }

    let emailStatus = validateEmail(email.val(), email, section);
    let phoneStatus = validatePhone(phone.val(), phone, section);
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

 function createAddressFormObject(firstName, lastName, companyName, line1, line2, townCity, regionIso, countryIso, postcode, status, phone, email,
    upsStoreAddress, openingDays, addressType) {
    let openingDaysDetails = '';
    if(openingDays != null) {
        if(openingDays[0] != null) {
            openingDaysDetails += openingDays[0].trim();
        }
        if(openingDays[1] != null) {
            openingDaysDetails += ';' + openingDays[1].trim();
        }
        if(openingDays[2] != null) {
            openingDaysDetails += ';' + openingDays[2].trim();
        }
    }
    if(regionIso.includes('-')) {
        regionIso = regionIso;
    } else {
       regionIso = countryIso+ '-' +regionIso;
    }

    if(addressType == null) {
        addressType = 'UNKNOWN';
    }

    if(line2 == null) {
        line2 = '';
    }

    if(companyName == null) {
        companyName = '';
    }

    if(email == null) {
        email = '';
    }

    let addressForm = {
        firstName : firstName.trim(),
        lastName : lastName.trim(),
        companyName : companyName.trim(),
        line1 : line1.trim(),
        line2 : line2.trim(),
        townCity : townCity.trim(),
        regionIso : regionIso.trim(),
        countryIso : countryIso.trim(),
        postcode : postcode.trim(),
        saveInAddressBook : status,
        phone : phone.trim(),
        email : email.trim(),
        upsStoreAddress: upsStoreAddress,
        openingDaysDetails: openingDaysDetails,
        addressType: addressType.trim()
    };
    return addressForm;
 }

 function validateZip(zipCode) {
    if(zipCode != null && zipCode.trim() != '' && zipCode.length != 0) {
        let zipRegex = /(^\d{5}$)|(^\d{5}-\d{4}$)/;
        return zipRegex.test(zipCode.trim());
    } else {
        return false;
    }
 }

 function validateField(attribute, fieldName) {
      if(attribute && attribute.trim() != '' && attribute.length < 255) {
          fieldName.removeClass('error');
          return true;
      }
      fieldName.addClass('error');
      return false;
  }

 function validateEmail(email, fieldName, section) {
      if(email == "" && email.trim() == '') {
        fieldName.addClass('error');
        return true;
      } else {
        let re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        if(email && email.trim() != '' && re.test(String(email).toLowerCase())) {
          fieldName.removeClass('error');
          return true;
        } else {
          fieldName.addClass('error');
          showErrorForEmailInputInvalidValidation(section);
          return true;
        }
      }
 }

 function validatePhone(phone, fieldName, section) {
      if(phone == "" && phone.trim() == '') {
          fieldName.addClass('error');
          return true;
      } else {
          if(phone && phone.trim() != '' && null != phone.match(/^[\+]?[(]?[0-9]{3}[/)]?[-\s\.]?[0-9]{3}[-\s\.]?[0-9]{4,6}$/im)) {
            fieldName.removeClass('error');
            return true;
          } else {
            fieldName.addClass('error');
            showErrorForPhoneInputInvalidValidation(section);
            return true;
          }
      }
 }

 //AJAX
 function addressValidationService(addressForm, deliveryMode, section, businessType) {
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
            if(data != null && data.statusMessage == 'Success' && data.result != null && data.result.length > 0) {
                 sessionStorage.setItem("avsFlowDeliveryMode", JSON.stringify(deliveryMode));
                 sessionStorage.setItem("businessType", JSON.stringify(businessType));
                 let whatYouEntered = addressForm.line1 + '<br/>' + addressForm.townCity + ', ' + addressForm.regionIso.split('-')[1] + ' ' +
                                         addressForm.postcode ;
                 $('#whatYouEntered').html(whatYouEntered);
                 sessionStorage.setItem("suggestedAddressForm", JSON.stringify(data.result[0]));
                 let whatWeSuggest = data.result[0].line1 + '<br/>' + data.result[0].town + ', ' + data.result[0].region.isocodeShort +
                                     ' ' + data.result[0].postalCode;
                 $('#whatWeSuggest').html(whatWeSuggest);
                 $('#avsCheck').modal('show');
            } else {
                 if(section == 'SHIP' && businessType && data.addressType != 'BUSINESS') {
                    showAMDeliveryErrorMessage(section);
                 } else {
                    if(data.addressType != null) {
                        addressForm.addressType = data.addressType;
                    }
                    //suggested state not supported error from response
                     addNewAddress(addressForm, deliveryMode)
                         .then((data) => {
                             sessionStorage.removeItem("enteredAddressForm");
                             saveDeliveryMode(deliveryMode, false)
                                 .then((data) => {
                                     $('.page-loader-new-layout').hide();
                                     window.location = ACC.config.encodedContextPath + '/checkout/multi/delivery-method/next';
                                 })
                                 .catch((error) => {
                                   console.log(error)
                                 })
                         })
                         .catch((error) => {
                           console.log(error)
                         })
                 }
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
     let businessType = JSON.parse(sessionStorage.getItem("businessType"));
     let section = JSON.parse(sessionStorage.getItem("section"));
     if(section == 'SHIP') {
        if(businessType && addressForm.addressType != 'BUSINESS') {
            showAMDeliveryErrorMessage(section);
            $('#avsCheck').modal('hide');
        } else {
            callAddNewAddress(enteredAddressForm, addressForm, deliveryMode);
        }
     } else {
        callAddNewAddress(enteredAddressForm, addressForm, deliveryMode);
     }
 }

 function callAddNewAddress(enteredAddressForm, addressForm, deliveryMode) {
    addNewAddress(createAddressFormObject(enteredAddressForm.firstName, enteredAddressForm.lastName, enteredAddressForm.companyName, addressForm.line1, addressForm.line2,
         addressForm.town, addressForm.region.isocode, 'US', addressForm.postalCode, enteredAddressForm.saveInAddressBook,
         enteredAddressForm.phone, enteredAddressForm.email, false, null, addressForm.addressType),
         JSON.parse(sessionStorage.getItem("avsFlowDeliveryMode")))
              .then((data) => {
                  sessionStorage.removeItem("suggestedAddressForm");
                  sessionStorage.removeItem("enteredAddressForm");
                  sessionStorage.removeItem("avsFlowDeliveryMode");
                  sessionStorage.removeItem("businessType");
                  sessionStorage.removeItem("rushStatus");
                  saveDeliveryMode(deliveryMode, false)
                       .then((data) => {
                           $('.page-loader-new-layout').hide();
                           $('#avsCheck').modal('hide');
                           window.location = ACC.config.encodedContextPath + '/checkout/multi/delivery-method/next';
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
    let businessType = JSON.parse(sessionStorage.getItem("businessType"));
    if(section == 'SHIP') {
        if(businessType && addressForm.addressType != 'BUSINESS') {
            showAMDeliveryErrorMessage(section);
            $('#avsCheck').modal('hide');
        } else {
            callEnteredAddNewAddress(addressForm, deliveryMode);
        }
    } else {
        callEnteredAddNewAddress(addressForm, deliveryMode);
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
                     $('#avsCheck').modal('hide');
                     window.location = ACC.config.encodedContextPath + '/checkout/multi/delivery-method/next';
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
    showErrorNotificationSHIP('AM delivery is only available to business addresses. Not at the office? Select Ship and Hold at a UPS Store for AM delivery options!', false);
 }

 function showErrorNotificationSHIP(msg) {
    $('#ship-it-am-notification').html('');
    $('#ship-it-am-notification').hide();
    let notification = '<div class="notification notification-error">' + msg + '</div>';
    $('#ship-it-notification').html(notification);
    $('#ship-it-notification').show();
    document.getElementById('ship-it-am-notification').scrollIntoView();
 }

 function saveSelectedAddress(selectedAddress, shippingGroup, deliveryMode, rushZip, businessType) {
     $.ajax({
         url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/selectAddress',
         data: {
             selectedAddressCode: selectedAddress,
             shippingGroup: shippingGroup,
             deliveryMode: deliveryMode,
             rushZip: rushZip,
             businessType: businessType
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
                        window.location = ACC.config.encodedContextPath + '/checkout/multi/delivery-method/next';
                    })
                    .catch((error) => {
                      console.log(error)
                    })
             } else if(data == 'AM-ERROR') {
                showAMDeliveryErrorMessage('SHIP');
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
                if(data == 'success') {
                    resolve(data);
                    //TODO: Handle for payment method for checkout
                    window.location = ACC.config.encodedContextPath + '/checkout/multi/payment-method/add';
                } else {
                    reject(data);
                }
             },
             error: function (data) {
                 reject(data);
             }
         });
    });
 }

 function savePickUpByFormOnCart(blPickUpByForm, deliveryMethod, status, upsStoreAddress) {
 var deliveryMode = $('#partnerPickUpShippingMethods #pickup-nyc').find('input[name="pickup-locations"]:checked').attr('id');
 if(checkShippingBlackout(deliveryMode))
	  {
	  	var validationDiv = $('<div class="notification notification-error mb-4" />').html(ACC.blackoutError.blockedShipping);
		$('#validationMessage').append(validationDiv);
	  }
     else if(checkAvailability(deliveryMethod))
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
                        // track Tealium event on continue shipping.
                          utag.link({
                          "tealium_event"    : "continue_shipping_click",
                          "shipping_method"   : "Ship It-Ship to UPS",
                          "shipping_method_not_available"     : "0"
                                   });
                        ACC.track.trackShippingSelection('Ship It','Ship to UPS','Item In Stock');
                        addNewAddress(upsStoreAddress, deliveryMethod)
                            .then((data) => {
                                saveDeliveryMode(deliveryMethod, status)
                                    .then((data) => {
                                        $('.page-loader-new-layout').hide();
                                        window.location = ACC.config.encodedContextPath + '/checkout/multi/delivery-method/next';
                                    })
                                    .catch((error) => {
                                      console.log(error)
                                    })
                            })
                            .catch((error) => {
                              console.log(error)
                            })
                     } else {
                       // track Tealium event on continue shipping.
                         utag.link({
                         "tealium_event"    : "continue_shipping_click",
                         "shipping_method"   : "PickUP",
                         "shipping_method_not_available"     : "0"
                                  });
                        ACC.track.trackShippingSelection('PickUP','','Item In Stock');
                        saveDeliveryMode(deliveryMethod, status)
                            .then((data) => {
                                $('.page-loader-new-layout').hide();
                                window.location = ACC.config.encodedContextPath + '/checkout/multi/delivery-method/next';
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
     if(upsStoreAddress != null) {
        // track Tealium event on continue shipping.
             utag.link({
              "tealium_event"    : "continue_shipping_click",
              "shipping_method"   : "Ship It-Ship to UPS",
              "shipping_method_not_available"     : "1"
                                          });
       ACC.track.trackShippingSelection('Ship It','Ship to UPS','Item Out of Stock');
       }else{
         // track Tealium event on continue shipping.
          utag.link({
         "tealium_event"    : "continue_shipping_click",
         "shipping_method"   : "PickUP",
         "shipping_method_not_available"     : "1"
                });
        ACC.track.trackShippingSelection('PickUP','','Item Out of Stock');
       }
     	window.location.reload();
     }
  }

  //show Price of shipping on cart
  function onChangeOfShipItShipToHome(event) {    
	$("#validationMessage").empty();  	
     $('#cart-shipping-cost').text($ ($("#shipToHomeShippingMethods .selected").find(".text-muted")).html());
      calculateCartTotal();
      if($('#shipToHomeShippingMethods').find('select[id="ship-it-shipping-methods-select-box"]').find(':selected').attr('businesstype') == "true") {
          let notification = '<div class="notification notification-warning">AM delivery is only available to business addresses. Not at the office? Select Ship and Hold at a UPS Store for AM delivery options!</div>';
          $('#ship-it-am-notification').html(notification);
          $('#ship-it-am-notification').show();
      } else {
        $('#ship-it-am-notification').html('');
        $('#ship-it-am-notification').hide();
      }
      $('#ship-it-notification').html('');
      $('#ship-it-notification').hide();
  }

  function onChangeOfShipItShipToUPS() {
  	 $("#validationMessage").empty();
     $('#cart-shipping-cost').text($ ($("#shipToUPSShippingMethods .selected").find(".text-muted")).html());
     calculateCartTotal();
     if($('#shipToUPSShippingMethods').find('#ship-UPS-shipping-methods-select-box').find(':selected').attr('businesstype') == "true") {
        let notification = '<div class="notification notification-warning">AM delivery is only available to business addresses. Not at the office? Select Ship and Hold at a UPS Store for AM delivery options!</div>';
        $('#ship-it-am-notification').html(notification);
        $('#ship-it-am-notification').show();
     } else {
        $('#ship-it-am-notification').html('');
        $('#ship-it-am-notification').hide();
     }
  }

  function onSelectOfPartnerAddress(event) {
  	$("#validationMessage").empty();
    $('#cart-shipping-cost').text($('#'+event.getAttribute('id')+'-pickUpCost').text().trim());
    calculateCartTotal();
  }

  function onChangeOfSameDayShippingMethodForCost() {
  	$("#validationMessage").empty();
    $('#cart-shipping-cost').text($ ($("#sameDayShippingMethods .selected").find(".text-muted")).html());
    calculateCartTotal();
  }

  function calculateCartTotal() {
    let total = checkNaN(parseFloat($('#cart-shipping-subTotal').text().split('$')[1])) +
                checkNaN(parseFloat($('#cart-shipping-waiver').text().split('$')[1])) +
                checkNaN(parseFloat($('#cart-shipping-options').text().split('$')[1])) +
                checkNaN(parseFloat($('#cart-shipping-cost').text().split('$')[1])) +
                checkNaN(parseFloat($('#cart-shipping-tax').text().split('$')[1])) -
                checkNaN(parseFloat($('#cart-shipping-discount').text().split('$')[1]));
    $('#cart-shipping-total').text('$' + total.toFixed(2));
  }

  function checkNaN(attribute) {
    if(isNaN(attribute)) {
        return 0.00;
    } else {
        return attribute;
    }
  }

  function onChangeOfStatusUpdate() {
    if($('#same-day-status-updates-div #same-day-status-updates').prop("checked") == true) {
        $('#statusUpdateTestMessage').html('<p class="mt-5 body14 gray60"> *Standard text message and data rates may apply.</p>');
    } else {
        $('#statusUpdateTestMessage').html('');
    }

  }
  
  //checks the product availability for the selected delivery mode
  function checkAvailability(deliveryMode){
    var isAvailable;
    if(deliveryMode != undefined && deliveryMode != '') {
        $.ajax({
            url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/checkAvailability',
            async: false,
            data: { deliveryMethod : deliveryMode },
            type: "GET",
            success: function (data) {
                if(data=='success') {
                    isAvailable = true;
                } else {
                    isAvailable = false;
                }
            },
            error: function (error) {
                isAvailable = false;
            }
        });
    } else {
        isAvailable = false;
    }
    return isAvailable;
 }

function checkShippingBlackout(deliveryMode){
	var blockShipping;
	if(deliveryMode != undefined && deliveryMode != '') {
		 $.ajax({
            url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/checkShippingBlackout',
            async: false,
            data: { deliveryMethod : deliveryMode },
            type: "GET",
            success: function (data) {
                if(data=='error') {
                    blockShipping = true;
                } else {
                    blockShipping = false;
                }
            },
            error: function (error) {
                blockShipping = true;
            }
        });
	}  else {
        blockShipping = false;
    }
    return blockShipping;
}

 //Replacement place order , mai method.
 $(document).on("click", ".js-replacement-order", function (e) {
     e.preventDefault();
    var shippingCategory = $('input[name="shipProduct"]:checked').attr('id');
    if(shippingCategory == 'ship-it') {
        $('#ship-it-notification').html("");
        var shippingMethod = $('#ship-it-select-box').val();
        if(shippingMethod == 'SHIP_HOME_HOTEL_BUSINESS') {
            shipToHomeReplacementShippingContinue(shippingMethod);
        } else {
            shipToUPSStoreLocationContinueForReplacementOrder(shippingMethod);
        }
    } else if(shippingCategory == 'pickup') {
        pickUpPartnerLocationContinue();
    } else {
        SFOrNYCShippingSectionContinueForReplacementOrder();
    }
 });



function shipToHomeReplacementShippingContinue(shippingMethod) {
      hideErrorForInputValidation();
      var savedAddress = null;
      var deliveryMode = $('#shipToHomeShippingMethods').find('select[id="ship-it-shipping-methods-select-box"]').val();
      var businessType = $('#shipToHomeShippingMethods').find('select[id="ship-it-shipping-methods-select-box"]').find(':selected').attr('businesstype');
      if(typeof businessType == "string") {
        businessType = JSON.parse(businessType);
      }
		if(checkShippingBlackout(deliveryMode))
	  {
	  	var validationDiv = $('<div class="notification notification-error mb-4" />').html(ACC.blackoutError.blockedShipping);
		$('#validationMessage').append(validationDiv);
	  }
      if(checkAvailability(deliveryMode))
      {
          if($('#delivery-shippingAddressFormDiv').css('display') == "none") {
              saveSelectedAddressForReplacementOrder($('select[id="ship-it-savedAddresses"]').val(), 'SHIP_HOME_HOTEL_BUSINESS', deliveryMode, null, businessType);
          } else {
              var firstName = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]');
              var lastName = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]');
              var companyName = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.companyName"]');
              var line1 = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]');
              var line2 = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]');
              var townCity = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]');
              var postcode = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]');
              var regionIso = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]');
              var email = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]');
              var phone = $('.ship-it-tab-content #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]');
              if(validateFormData(firstName, lastName, line1, townCity, postcode, regionIso, email, phone, "Ship")) {
                  if($('#showErrorForInvalidZipInputValidation').css('display') == "none" &&
                        $('#showErrorForInvalidEmailInputValidation').css('display') == "none" &&
                        $('#showErrorForInvalidPhoneInputValidation').css('display') == "none") {
                  addressValidationServiceForOrderReplacement(createAddressFormObject(firstName.val(), lastName.val(), companyName.val(), line1.val(), line2.val(), townCity.val(),regionIso.val(),
                                                                     'US', postcode.val(), $('.ship-it-tab-content').find('input[id="ship-it-save-address"]').prop("checked"),
                                                                     phone.val(), email.val(), false, null, 'UNKNOWN'), deliveryMode, 'SHIP', businessType);
                  }
              } else {
                  showErrorForInputValidation('Ship');
              }
          }

       // track Tealium event on continue shipping.
       utag.link({
             "tealium_event"    : "continue_shipping_click",
             "shipping_method"   : "Ship It-Ship to home",
             "shipping_method_not_available"     : "0"
         });
        ACC.track.trackShippingSelection('Ship It','Ship to home','Item In Stock');
      }
      else
      {
        // track Tealium event on continue shipping.
              utag.link({
              "tealium_event"    : "continue_shipping_click",
              "shipping_method"   : "Ship It-Ship to home",
              "shipping_method_not_available"     : "1"
               });
        ACC.track.trackShippingSelection('Ship It','Ship to home','Item Out of Stock');
      	window.location.reload();
      }

  }


   function saveSelectedAddressForReplacementOrder(selectedAddress, shippingGroup, deliveryMode, rushZip, businessType) {
       $.ajax({
           url: ACC.config.encodedContextPath + '/checkout/multi/delivery-method/selectReplacementAddress',
           data: {
               selectedAddressCode: selectedAddress,
               shippingGroup: shippingGroup,
               deliveryMode: deliveryMode,
               rushZip: rushZip,
               businessType: businessType
           },
           type: "GET",
           beforeSend: function(){
              $('.page-loader-new-layout').show();
           },
           success: function (data) {
               if(data == 'SUCCESS') {
                  saveDeliveryModeForOrderReplacement(deliveryMode, false);
               } else if(data == 'AM-ERROR') {
                  showAMDeliveryErrorMessage('SHIP');
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



 async function saveDeliveryModeForOrderReplacement(deliveryMode, internalAddressIndicator) {
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
                if(data == 'success') {
                 $("#replaceMentplaceOrderForm").submit();
                    resolve(data);
                } else {
                    reject(data);
                }
             },
             error: function (data) {
                 reject(data);
             }
         });
    });
 }


  function shipToUPSStoreLocationContinueForReplacementOrder(shippingMethod) {
      if($('#changeUPSStoreButton').is(":visible")) {
          utag.link({
            "tealium_event"    : "continue_shipping_click",
            "shipping_method"   : "Ship It-Ship to UPS",
            "shipping_method_not_available"     : "0"
          });
          ACC.track.trackShippingSelection('Ship It','Ship to UPS','Item In Stock');
          var deliveryMethod = $('#shipToUPSShippingMethods').find('#ship-UPS-shipping-methods-select-box').val();
          if(checkShippingBlackout(deliveryMethod))
	  {
	  	var validationDiv = $('<div class="notification notification-error mb-4" />').html(ACC.blackoutError.blockedShipping);
		$('#validationMessage').append(validationDiv);
	  }
	  else
	  {
	  	addNewAddress(createUPSStoreAddress(), deliveryMethod)
              .then((data) => {
                  saveDeliveryModeForOrderReplacement(deliveryMethod, false);
              })
              .catch((error) => {
                console.log(error)
              })
	  }
          
      } else {
         showErrorForUPSOrPickAddressError();
      }
  }


   function SFOrNYCShippingSectionContinueForReplacementOrder() {
      hideErrorForInputValidation();
      $('#same-day-notification').val('');
      $('#same-day-notification').hide();
      var savedAddress = null;
      var sameDayDeliveryNote = $('#sameDayDeliveryNote').val();
      var deliveryMode = $('#sameDayShippingMethods').find('select[id="same-day-shipping-methods-select-box"]').val();
      if(checkShippingBlackout(deliveryMode))
	  {
	  	var validationDiv = $('<div class="notification notification-error mb-4" />').html(ACC.blackoutError.blockedShipping);
		$('#validationMessage').append(validationDiv);
	  }
      else if(checkAvailability(deliveryMode))
      {
          // track Tealium event on continue shipping.
             utag.link({
             "tealium_event"    : "continue_shipping_click",
             "shipping_method"   : "Same Day Delivery",
             "shipping_method_not_available"     : "0"
             });
          ACC.track.trackShippingSelection('Same Day Delivery','','Item In Stock');
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
                          saveSelectedAddressForReplacementOrder(savedAddress, $('#same-day-select-box').val(), deliveryMode, $('#sameDayZipCheckText').val(), false);
                      }
                 },
                 error: function (data) {
                      $('.page-loader-new-layout').hide();
                 }
              });
          } else {
              var firstName = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.firstName"]');
              var lastName = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.lastName"]');
              var companyName = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.companyName"]');
              var line1 = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line1"]');
              var line2 = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.line2"]');
              var townCity = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.townCity"]');
              var postcode = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.postcode"]');
              var regionIso = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('select[id="address.countryIso"]');
              var email = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.email"]');
              var phone = $('#same-day-address-div #delivery-shippingAddressForm #addressForm').find('.form-group').find('input[id="address.phone"]');
              if(validateFormData(firstName, lastName, line1, townCity, postcode, regionIso, email, phone, "Rush")) {
                  if($('#showErrorForInvalidZipInputValidation').css('display') == "none" &&
                      $('#showErrorForInvalidEmailInputValidation').css('display') == "none" &&
                      $('#showErrorForInvalidPhoneInputValidation').css('display') == "none") {
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
                                  addressValidationServiceForOrderReplacement(createAddressFormObject(firstName.val(), lastName.val(), companyName.val(), line1.val(), line2.val(),
                                                              townCity.val(),regionIso.val(), 'US', postcode.val(),
                                                              $('#same-day-address-div').find('input[id="same-day-save-address"]').prop("checked"),
                                                              phone.val(), email.val(), false, null, 'UNKNOWN'), deliveryMode, 'RUSH', null);
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
              } else {
                  showErrorForInputValidation('Rush');
                  $('.page-loader-new-layout').hide();
              }
          }
        }
        else
        {
         // track Tealium event on continue shipping.
            utag.link({
            "tealium_event"    : "continue_shipping_click",
            "shipping_method"   : "Same Day Delivery",
            "shipping_method_not_available"     : "1"
           });
         ACC.track.trackShippingSelection('Same Day Delivery','','Item Out of Stock');
        	window.location.reload();
      }
    }


//AJAX
 function addressValidationServiceForOrderReplacement(addressForm, deliveryMode, section, businessType) {
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
            if(data != null && data.statusMessage == 'Success' && data.result != null && data.result.length > 0) {
                 sessionStorage.setItem("avsFlowDeliveryMode", JSON.stringify(deliveryMode));
                 sessionStorage.setItem("businessType", JSON.stringify(businessType));
                 let whatYouEntered = addressForm.line1 + '<br/>' + addressForm.townCity + ', ' + addressForm.regionIso.split('-')[1] + ' ' +
                                         addressForm.postcode ;
                 $('#whatYouEnteredForReplacementOrder').html(whatYouEntered);
                 sessionStorage.setItem("suggestedAddressForm", JSON.stringify(data.result[0]));
                 let whatWeSuggest = data.result[0].line1 + '<br/>' + data.result[0].town + ', ' + data.result[0].region.isocodeShort +
                                     ' ' + data.result[0].postalCode;
                 $('#whatWeSuggestForReplacementOrder').html(whatWeSuggest);
                 $('#avsCheckReplacementOrder').modal('show');
            } else {
                 if(section == 'SHIP' && businessType && data.addressType != 'BUSINESS') {
                    showAMDeliveryErrorMessage(section);
                 } else {
                    if(data.addressType != null) {
                        addressForm.addressType = data.addressType;
                    }
                    //suggested state not supported error from response
                     addNewAddress(addressForm, deliveryMode)
                         .then((data) => {
                             sessionStorage.removeItem("enteredAddressForm");
                             saveDeliveryModeForOrderReplacement(deliveryMode, false);
                         })
                         .catch((error) => {
                           console.log(error)
                         })
                 }
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


function onClickOfSaveSuggestedAddressForReplacementOrder() {
     let addressForm = JSON.parse(sessionStorage.getItem("suggestedAddressForm"));
     let enteredAddressForm = JSON.parse(sessionStorage.getItem("enteredAddressForm"));
     let deliveryMode = JSON.parse(sessionStorage.getItem("avsFlowDeliveryMode"));
     let businessType = JSON.parse(sessionStorage.getItem("businessType"));
     let section = JSON.parse(sessionStorage.getItem("section"));
     if(section == 'SHIP') {
        if(businessType && addressForm.addressType != 'BUSINESS') {
            showAMDeliveryErrorMessage(section);
            $('#avsCheck').modal('hide');
        } else {
            callAddNewAddressForOrderReplacement(enteredAddressForm, addressForm, deliveryMode);
        }
     } else {
        callAddNewAddressForOrderReplacement(enteredAddressForm, addressForm, deliveryMode);
     }
 }



 function callAddNewAddressForOrderReplacement(enteredAddressForm, addressForm, deliveryMode) {
    addNewAddress(createAddressFormObjectForReplacementOrder(enteredAddressForm.firstName, enteredAddressForm.lastName, enteredAddressForm.companyName, addressForm.line1, addressForm.line2,
         addressForm.town, addressForm.region.isocode, 'US', addressForm.postalCode, enteredAddressForm.saveInAddressBook,
         enteredAddressForm.phone, enteredAddressForm.email, false, null, addressForm.addressType),
         JSON.parse(sessionStorage.getItem("avsFlowDeliveryMode")))
              .then((data) => {
                  sessionStorage.removeItem("suggestedAddressForm");
                  sessionStorage.removeItem("enteredAddressForm");
                  sessionStorage.removeItem("avsFlowDeliveryMode");
                  sessionStorage.removeItem("businessType");
                  sessionStorage.removeItem("rushStatus");
                  saveDeliveryModeForOrderReplacement(deliveryMode, false);
              })
              .catch((error) => {
                console.log(error)
              })
 }


  function onClickOfSaveEnteredAddressForOrderReplacement() {
     let deliveryMode = JSON.parse(sessionStorage.getItem("avsFlowDeliveryMode"));
     let addressForm = JSON.parse(sessionStorage.getItem("suggestedAddressForm"));
     let section = JSON.parse(sessionStorage.getItem("section"));
     let businessType = JSON.parse(sessionStorage.getItem("businessType"));
     if(section == 'SHIP') {
         if(businessType && addressForm.addressType != 'BUSINESS') {
             showAMDeliveryErrorMessage(section);
             $('#avsCheck').modal('hide');
         } else {
             callEnteredAddNewAddressForOrderReplacement(addressForm, deliveryMode);
         }
     } else {
         callEnteredAddNewAddressForOrderReplacement(addressForm, deliveryMode);
     }
  }


  function callEnteredAddNewAddressForOrderReplacement(addressForm, deliveryMode) {
      let newAddressForm = JSON.parse(sessionStorage.getItem("enteredAddressForm"));
      newAddressForm['addressType'] = addressForm.addressType;
      addNewAddress(newAddressForm, JSON.parse(sessionStorage.getItem("avsFlowDeliveryMode")))
            .then((data) => {
                sessionStorage.removeItem("suggestedAddressForm");
                sessionStorage.removeItem("enteredAddressForm");
                sessionStorage.removeItem("avsFlowDeliveryMode");
                sessionStorage.removeItem("rushStatus");
                saveDeliveryModeForOrderReplacement(deliveryMode, false);
            })
            .catch((error) => {
              console.log(error)
            })
   }


 function createAddressFormObjectForReplacementOrder(firstName, lastName, companyName, line1, line2, townCity, regionIso, countryIso, postcode, status, phone, email,
    upsStoreAddress, openingDays, addressType) {
    let openingDaysDetails = '';
    if(openingDays != null) {
        if(openingDays[0] != null) {
            openingDaysDetails += openingDays[0].trim();
        }
        if(openingDays[1] != null) {
            openingDaysDetails += ';' + openingDays[1].trim();
        }
        if(openingDays[2] != null) {
            openingDaysDetails += ';' + openingDays[2].trim();
        }
    }
    if(regionIso.includes('-')) {
        regionIso = regionIso;
    } else {
       regionIso = countryIso+ '-' +regionIso;
    }

    if(addressType == null) {
        addressType = 'UNKNOWN';
    }

    if(line2 == null){
    line2 = " ";
    }

    let addressForm = {
        firstName : firstName.trim(),
        lastName : lastName.trim(),
        companyName : companyName.trim(),
        line1 : line1.trim(),
        line2 : line2.trim(),
        townCity : townCity.trim(),
        regionIso : regionIso.trim(),
        countryIso : countryIso.trim(),
        postcode : postcode.trim(),
        saveInAddressBook : status,
        phone : phone.trim(),
        email : email.trim(),
        upsStoreAddress: upsStoreAddress,
        openingDaysDetails: openingDaysDetails,
        addressType: addressType.trim()
    };
    return addressForm;
 }











    
   