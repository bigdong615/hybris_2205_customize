//BL-467 clear cart functionality from cart page.
$('.clear-cart-continue').on("click", function(event) {
	$.ajax({
		url : ACC.config.encodedContextPath + '/cart/emptyCart',
		type : "GET",
		success : function(data) {
			window.location.reload();
		},
		error : function(xht, textStatus, ex) {
			console.log("Error while removing cart entries");
		}
	});
});

// BL-458 update quantity from cart page.
$('.update').on("change", function (e){
       			  var newCartQuantity = $(this).find(":selected").val();
       				e.preventDefault();
       				var entryNumber = $(this).attr('id').split("_");
       				var form = $('#updateCartForm' + entryNumber[1]);
       				var productCode = form.find('input[name=productCode]').val();
       				var initialCartQuantity = form.find('input[name=initialQuantity]').val();
       				var entryNumber = form.find('input[name=entryNumber]').val();
       				form.find('input[name=quantity]').val(newCartQuantity);
       				if(initialCartQuantity != newCartQuantity)
       				{
       					ACC.track.trackUpdateCart(productCode, initialCartQuantity, newCartQuantity);
                form.submit();
       				}
       });

// BL-458 remove product from cart page.
$('.shopping-cart__item-remove').on("click", function (e){
            	e.preventDefault();
            	var entryNumber = $(this).attr('id').split("_");
            	var form = $('#removeCartForm' + entryNumber[1]);

            	var productCode = form.find('input[name=productCode]').val();
            	var initialCartQuantity = form.find('input[name=initialQuantity]');
            	var cartQuantity = form.find('input[name=quantity]');

            	ACC.track.trackRemoveFromCart(productCode, initialCartQuantity.val());
            	cartQuantity.val(0);
            	initialCartQuantity.val(0);
            	$(".shopping-cart__item-remove").attr("disabled", "disabled");
            	form.submit();
       });

// Script to apply the selected damage Waiver from the dropdown on the cart page
 $('ul.damage-Waiver-update').on('click','li',function(e){
 	e.preventDefault();
 	var entryNumber = $(this).find("a").data('entry');
 	var damageWaiverType = $(this).find("a").data('id');
 	var damageWaiverUpdateForm = $('#updateDamageWaiverForm');
 	damageWaiverUpdateForm.find('input[name=entryNumber]:hidden').val(entryNumber);
 	damageWaiverUpdateForm.find('input[name=damageWaiverType]:hidden').val(damageWaiverType);
 	damageWaiverUpdateForm.submit();
 });

 //BL-454 add to cart
 $('.js-add-to-cart').on("click",function(e) {
                       e.preventDefault();
                        var productCode = $(this).attr('data-product-code');
                        var serialCode = $(this).attr('data-serial');
                        if(serialCode == '' || serialCode == undefined){
                        serialCode = "serialCodeNotPresent";
                        }
                        $.ajax({
                                   url: ACC.config.encodedContextPath + "/cart/add",
                                   type: 'POST',
                                   data: {productCodePost: productCode,serialProductCodePost:serialCode},
                                   beforeSend: function(){
                                        $('.page-loader-new-layout').show();
                                   },
                                   success: function (response) {
                                      $('#addToCartModalDialog').html(response.addToCartLayer);
                                      if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
                                         ACC.minicart.updateMiniCartDisplay();
                                      }
                                      mixedProductInterception(productCode, serialCode);
                                      updateQuantity();
                                      addToCartFromModal();
                                      if(document.getElementById("addToCart-gear-sliders") != null){
                                        setTimeout(modalBodyContent,100);
                                      };
                                   },
                                   complete : function() {
                                        $('.page-loader-new-layout').hide();
                                   },
                                   error: function (jqXHR, textStatus, errorThrown) {
                                         $('.page-loader-new-layout').hide();
                                         $('.modal-backdrop').addClass('remove-popup-background');
                                         // log the error to the console
                                         console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
                                   }
                        });

 });

 // BL-454 update quantity from rental add to cart popup.
 function updateQuantity() {
    $('.btn-number').click(function(e) {
    	e.preventDefault();
    	fieldName = $(this).attr('data-field');
    	type = $(this).attr('data-type');
    	var input = $("input[name='" + fieldName + "']");
    	var currentVal = parseInt(input.val());
    	if (!isNaN(currentVal)) {
    		if (type == 'minus') {
    			if (currentVal > input.attr('min')) {
    				input.val(currentVal - 1).change();
    			}
    			if (parseInt(input.val()) == input.attr('min')) {
    				$(this).attr('disabled', true);
    			}
    		} else if (type == 'plus') {
    			if (currentVal < input.attr('max')) {
    				input.val(currentVal + 1).change();
    			}
    			if (parseInt(input.val()) == input.attr('max')) {
    				$(this).attr('disabled', true);
    			}
    		}
    	} else {
    		input.val(0);
    	}
    });
    $('.input-number').focusin(function() {
    	$(this).data('oldValue', $(this).val());
    });

    $('.input-number').change(function() {
    			minValue = parseInt($(this).attr('min'));
    			maxValue = parseInt($(this).attr('max'));
    			valueCurrent = parseInt($(this).val());
    			name = $(this).attr('name');
    			if (valueCurrent >= minValue) {
    				$(
    						".btn-number[data-type='minus'][data-field='"
    						+ name + "']").removeAttr('disabled')
    			} else {
    				$(this).val($(this).data('oldValue'));
    			}
    			if (valueCurrent <= maxValue) {
    				$(
    						".btn-number[data-type='plus'][data-field='"
    						+ name + "']").removeAttr('disabled')
    			} else {
    				$(this).val($(this).data('oldValue'));
    			}
    			// update cart quantity
    			var entryNumber = parseInt($(this).attr('entryNumber'));
    			var form = $('#updateCartForm' + entryNumber);
    			var productCode = form.find('input[name=productCode]')
    			.val();
    			var initialCartQuantity = form.find(
    			'input[name=initialQuantity]').val();
    			var entryNumber = form.find('input[name=entryNumber]')
    			.val();
    			form.find('input[name=quantity]').val(valueCurrent);
    			if (valueCurrent >= minValue && valueCurrent <= maxValue) {
    			$.ajax({
    				url : ACC.config.encodedContextPath
    				+ "/cart/updateQuantity",
    				type : 'POST',
    				data : form.serialize(),
    				beforeSend : function() {
    					$('.page-loader-new-layout').show();
    				},
    				success : function(response) {
    					if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
    						ACC.minicart.updateMiniCartDisplay();
    					}
    				},
    				complete : function() {
    					$('.page-loader-new-layout').hide();
    				},
    				error : function(jqXHR, textStatus, errorThrown) {
    					$('.page-loader-new-layout').hide();
    					console.log(
    							"The following error occurred: "
    							+ jqXHR, textStatus,
    							errorThrown);
    				}
    			});
    			}
    });

    $(".input-number").keydown(function(e) {
    			// Allow: backspace, delete, tab, escape, enter and .
    			if ($.inArray(e.keyCode, [ 46, 8, 9, 27, 13, 190 ]) !== -1 ||
    					// Allow: Ctrl+A
    					(e.keyCode == 65 && e.ctrlKey === true) ||
    					// Allow: home, end, left, right
    					(e.keyCode >= 35 && e.keyCode <= 39)) {
    				// let it happen, don't do anything
    				return;
    			}
    			// Ensure that it is a number and stop the keypress
    			if ((e.shiftKey || (e.keyCode < 48 || e.keyCode > 57))
    					&& (e.keyCode < 96 || e.keyCode > 105)) {
    				e.preventDefault();
    			}
    });
 }

//BL-461,465 Product Availability
$('#cart-continue').on("click", function (e) {
	e.preventDefault();

	$.ajax({
		url: ACC.config.encodedContextPath + "/cart/checkDateAndStock",
		type: 'GET',
    success: function (response) {

			if (response == 'success') {
				var url = ACC.config.encodedContextPath + "/checkout/multi/delivery-method/chooseShipping";
				window.location.href = url;
			} else if (response == 'rentalDateNotSelected') {
				$('#cart-warning').css('display', 'block');
			}else{
			  window.location.href = ACC.config.encodedContextPath + "/cart";
			}
		},
		error: function (jqXHR, textStatus, errorThrown) {
			console.log("The following error occurred: " + jqXHR, textStatus, errorThrown);
		}
	});
});

//BL-563 Gift Card Apply
$('#applyGcCode').click(function (e) {
	e.preventDefault();
  var giftCardForm = $("#giftCardForm");
  var $form = $(this);
	var gcCode = $("#gift-card-apply-gift-card-number").val();
	$.ajax({
		url: giftCardForm.attr('action'),
		type: giftCardForm.attr("method"),
		data: {
			code: gcCode
		},
		beforeSend: function(){
        $('.page-loader-new-layout').show();
    },
    success: function (data) {
      window.location.reload();
    },
    complete: function() {
        $('.page-loader-new-layout').hide();
    },
    error: function (jqXHR, textStatus, errorThrown) {
       $('.page-loader-new-layout').hide();
       console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
    }
	});
});

//BL-563 Remove Gift Card
$('.remove-gift-card').on("click", function(e) {
     e.preventDefault();
     var itemIndex = $(this).data("index");
     var $form = $(document).find('#removeGiftCardForm' + itemIndex);
     var method = $form.attr("method") ? $form.attr("method").toUpperCase() : "POST";
     $.ajax({
         url: $form.attr("action"),
         data: $form.serialize(),
         async: false,
         type: method,
         success: function(data, status, xhr) {
              window.location.reload();
         },
         error: function(error) {
             console.log("Error while removing gift card");
         }
     });
});

// BL-581
$(".js-hr-tag").last().hide();

//BL-688 changes

if($("#addToCartButton").hasClass("js-disable-btn"))
{
    $("#product-litepicker").addClass("date-notAvail");
    $("#mobile-product-litepicker").addClass("date-notAvail");
    $("#pickupDelivery .pickupDeliveryLink").addClass("d-none");
    $(" #productDates .input-group").addClass("red-border");
}

if($(".arrival").hasClass("nextAvailDate") && !$("#addToCartButton").hasClass("js-disable-btn")){
    $("#product-litepicker").addClass("date-notAvail");
    $("#mobile-product-litepicker").addClass("date-notAvail");
    $(" #productDates .input-group").addClass("red-border");
    $("#pickupDelivery .pickupDeliveryLink").addClass("d-none");
}

 //BL-455  Changes for Add To Cart POP Up
   function modalBodyContent(){
   	new Splide( '#addToCart-gear-sliders', {
   					perPage: 3,

   					breakpoints: {
   						'991': {
   							perPage: 2,
   						},
   						'640': {
   							perPage: 1,
   						},
   					},
   					rewind : true,
   					gap: 20,
   					padding: 10,
   					arrows : true,
   					pagination :true,
   					keyboard: false,
   				} ).mount();
   				 document.querySelectorAll('.card-sliders').forEach(carousel => new Splide( carousel, {
   					type   : 'loop',
   					perPage: 1,
   					pagination: true,
   					drag   : false,
   					fixedHeight: 140,
   					breakpoints: {
   						'991': {
   							pagination: false,
   						},
   					},
   					keyboard: false,
   				} ).mount());
 //BL-455  ends  here
   		   let modalCardQty =  document.getElementsByClassName("card-sliders").length;
   		   if (modalCardQty!=0){
   			if(modalCardQty<=3 && screen.width>991){
   				document.querySelector("#addToCart-gear-sliders .splide__arrows").style.display="none";
   				 document.querySelector("#addToCart-gear-sliders > .splide__pagination").style.display="none";
   			}
   			 if(modalCardQty<=2 && screen.width<=991 && screen.width>767){
   				document.querySelector("#addToCart-gear-sliders .splide__arrows").style.display="none";
   				 document.querySelector("#addToCart-gear-sliders > .splide__pagination").style.display="none";
   			}
     	}
    }

  //BL-455 add to cart
    function addToCartFromModal(){
   $('.js-add-to-cart-popup').on('click',function(e) {
                         e.preventDefault();
                         let popUpId = this.getAttribute("id");
                          var productCode = $(this).attr('data-product-code');
                          var serialCode = $(this).attr('data-serial');
                          if(serialCode == '' || serialCode == undefined){
                         serialCode = "serialCodeNotPresent";
                         }
                          $.ajax({
                                     url: ACC.config.encodedContextPath + "/cart/add",
                                     type: 'POST',
                                     data: {productCodePost: productCode,serialProductCodePost:serialCode},
                                     beforeSend: function(){
                                       $('.page-loader-new-layout').show();
                                     },
                                     success: function (response) {
                                      var index = $( ".js-add-to-cart-popup" ).index( this );
                                      document.getElementById(popUpId).innerHTML= "Added";
                                      if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
                                           ACC.minicart.updateMiniCartDisplay();
                                      }
                                     },
                                      complete: function() {
                                        $('.page-loader-new-layout').hide();
                                       },
                                     error: function (jqXHR, textStatus, errorThrown) {
                                           $('.modal-backdrop').addClass('remove-popup-background');
                                           // log the error to the console
                                           console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
                                     }
                          });

   });
  }

//BL-458 Quantity Change
$('.btn-number').click(function(e){
	e.preventDefault();
	fieldName = $(this).attr('data-field');
	type = $(this).attr('data-type');
	var input = $("input[name='"+fieldName+"']");
	var currentVal = parseInt(input.val());
	if (!isNaN(currentVal)) {
		if(type == 'minus') {
      if(currentVal > input.attr('min')) {
				input.val(currentVal - 1).change();
			}
			if(parseInt(input.val()) == input.attr('min')) {
				$(this).attr('disabled', true);
			}
    } else if(type == 'plus') {
      if(currentVal < input.attr('max')) {
				input.val(currentVal + 1).change();
			}
			if(parseInt(input.val()) == input.attr('max')) {
				$(this).attr('disabled', true);
			}
		}
	} else {
		input.val(0);
	}
});
$('.input-number').focusin(function(){
	$(this).data('oldValue', $(this).val());
});

$('.input-number').change(function() {
	minValue = parseInt($(this).attr('min'));
	maxValue = parseInt($(this).attr('max'));
	valueCurrent = parseInt($(this).val());
  name = $(this).attr('name');
	if(valueCurrent >= minValue) {
		$(".btn-number[data-type='minus'][data-field='"+name+"']").removeAttr('disabled')
	} else {
		$(this).val($(this).data('oldValue'));
	}
	if(valueCurrent <= maxValue) {
		$(".btn-number[data-type='plus'][data-field='"+name+"']").removeAttr('disabled')
	} else {
		$(this).val($(this).data('oldValue'));
	}
	//update cart quantity
	if (valueCurrent >= minValue && valueCurrent <= maxValue) {
  	var entryNumber = parseInt($(this).attr('entryNumber'));
  	var form = $('#updateCartForm' + entryNumber);
  	var productCode = form.find('input[name=productCode]').val();
  	var initialCartQuantity = form.find('input[name=initialQuantity]').val();
  	var entryNumber = form.find('input[name=entryNumber]').val();
  	form.find('input[name=quantity]').val(valueCurrent);
  	if (initialCartQuantity != valueCurrent) {
  		ACC.track.trackUpdateCart(productCode, initialCartQuantity,
  				valueCurrent);
  		form.submit();
  	}
  }
});

$(".input-number").keydown(function (e) {
	// Allow: backspace, delete, tab, escape, enter and .
	if ($.inArray(e.keyCode, [46, 8, 9, 27, 13, 190]) !== -1 ||
			// Allow: Ctrl+A
			(e.keyCode == 65 && e.ctrlKey === true) ||
			// Allow: home, end, left, right
			(e.keyCode >= 35 && e.keyCode <= 39)) {
		// let it happen, don't do anything
		return;
	}
	// Ensure that it is a number and stop the keypress
	if ((e.shiftKey || (e.keyCode < 48 || e.keyCode > 57)) && (e.keyCode < 96 || e.keyCode > 105)) {
		e.preventDefault();
	}
});

//BL-454 It triggers, when user clicks on continue button from mixed product interception modal.
function mixedProductInterception(productCode, serialCode){
$('#mixedProductInterception').on("click", function(event) {
  $.ajax({
		url : ACC.config.encodedContextPath + '/cart/emptyCart',
		type : "GET",
		beforeSend: function(){
         $('.page-loader-new-layout').show();
     },
		success : function(data) {
			addProductToCart(productCode, serialCode);
		},
		error : function(xht, textStatus, ex) {
		  $('.page-loader-new-layout').hide();
			console.log("Error while removing cart entries");
		}
	});
});
}

function addProductToCart(productCode, serialCode){
  $.ajax({
  	url : ACC.config.encodedContextPath + "/cart/add",
  	type : 'POST',
  	data : {
  		productCodePost : productCode,
  		serialProductCodePost : serialCode
  	},
  	success : function(response) {
  	  $('.page-loader-new-layout').hide();
  	  $('#addToCartModalDialog').addClass('modal-lg');
  		$('#addToCartModalDialog').html(response.addToCartLayer);
  		if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
  			ACC.minicart.updateMiniCartDisplay();
  		}
  		updateQuantity();
  		addToCartFromModal();
  		if (document.getElementById("addToCart-gear-sliders") != null) {
  			setTimeout(modalBodyContent, 100);
  		}
  	},
  	error : function(jqXHR, textStatus, errorThrown) {
  	  $('.page-loader-new-layout').hide();
  		$('.modal-backdrop').addClass('remove-popup-background');
  		// log the error to the console
  		console.log("The following error occurred: " + jqXHR, textStatus,
  				errorThrown);
  	}
  });
}

