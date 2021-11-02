jQuery(document).ready(function () {
	$(".hidebutton").hide();
	if($(".hidebutton").length <= 0){
		$(".hide-after-login").hide();
	}
    //BL-917: Replacement order
	var fvalue = []
        $(".return-qty-check").each(function (index) {
            if($(this).val()==0)
            {
                $('.return-button-cls').attr('disabled', true);
                $('.return-minus-btn'+index).attr('disabled', true);
            }
        }) ;
});

//BL-467 clear cart functionality from cart page.
 $('.clear-cart-continue').on("click", function(event) {
     clearInterval(z);
 	 localStorage.removeItem('saved_countdown');
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
              var productName = form.find('input[name=productName]').val();
            	ACC.track.trackRemoveFromCart(productCode, productName ,initialCartQuantity.val());
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
 	var productCode =$(this).find("a").data('product-code');
  ACC.track.trackChangeDamageWaiver(productCode, damageWaiverType);
 	var damageWaiverUpdateForm = $('#updateDamageWaiverForm');
 	damageWaiverUpdateForm.find('input[name=entryNumber]:hidden').val(entryNumber);
 	damageWaiverUpdateForm.find('input[name=damageWaiverType]:hidden').val(damageWaiverType);
 	damageWaiverUpdateForm.submit();
 });
 
//Script to apply the selected bl-options from the dropdown on the cart page
 $('ul.bl-options-update').on('click','li',function(e){
 	e.preventDefault();
 	var entryNumber = $(this).find("a").data('entry');
 	var blOptions = $(this).find("a").data('id');
 	var productCode =$(this).find("a").data('product-code');
 	var blOptionsUpdateForm = $('#updateBlOptionsForm');
 	blOptionsUpdateForm.find('input[name=entryNumber]:hidden').val(entryNumber);
 	blOptionsUpdateForm.find('input[name=bloptions]:hidden').val(blOptions);
 	blOptionsUpdateForm.submit();
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
                                	   var giftCardNotAllowedWarninLayer = response.giftCardNotAllowedWarninLayer;
                                	   if(giftCardNotAllowedWarninLayer != undefined && giftCardNotAllowedWarninLayer != '')
                                    	{
                                    		$('#addToCartModalDialog').html(response.giftCardNotAllowedWarninLayer);
                                    		 if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
                                                 ACC.minicart.updateMiniCartDisplay();
                                              }
                                    		onGiftCardCloseModal();
                                     		setTimeout(function(){
                                          	   $("#signUp").modal('hide');
                                          	   $("#addToCart").addClass("show");
                                                 $("#addToCart").show();
                                             },500);
                                     		mixedProductInterception(productCode, serialCode);
                                    	}
                                	   else{
                                      $('#addToCartModalDialog').html(response.addToCartLayer);
                                      if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
                                         ACC.minicart.updateMiniCartDisplay();
                                      }
                                       var productName = $('#productName').val();
                                       var productBrand =$('#productBrand').val();
                                       var productType = $('#productType').val();
                                       var productCategory = $('#productCategory').val()
                                       var quantity = $('#quantity').val();
                                       ACC.track.trackAddToCart(productCode, productName,productBrand,productType,productCategory,quantity);
                                      // Tealium event for ATC
                                       utag.link({
                                           "tealium_event" : "cart_add",
                                           "productSKU"    : '["'+productCode+'"]',
                                           "ProductName"   : '["'+productName+'"]',
                                           "quantity"      : '["'+quantity+'"]',
                                           "add_to_cart_rental"  : "1",
                                           "isBuy"            : "0"
                                       });

                                      //On empty cart page, add class on close & continue shopping button of add to rental modal.
                                      $(".js-emptyCartPage").find(".btn-close").addClass('emptyCart-modalClose');
                                      $(".js-emptyCartPage").find(".btn-outline").addClass('emptyCart-modalClose');
                                      reloadEmptyCartPageOnModalClose();
                                      mixedProductInterception(productCode, serialCode);
                                      updateQuantity();
                                      addToCartFromModal();
                                      if(document.getElementById("addToCart-gear-sliders") != null){
                                        setTimeout(modalBodyContent,100);
                                      };
                                	 }
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
 	let bouncer;
 	$('.btn-number').click(function(e){
 		e.preventDefault();
 		var entryNumber = parseInt($(this).attr('entryNumber'));
 		var form = $('#updateCartForm' + entryNumber);
 		var productCode = form.find('input[name=productCode]').val();
 		var initialCartQuantity = form.find('input[name=initialQuantity]').val();
 		var entryNumber = form.find('input[name=entryNumber]').val();
 		fieldName = $(this).attr('data-field');
 		type = $(this).attr('data-type');
 		var input = $("input[name='"+fieldName+"']");
 		var currentVal = parseInt(input.val());
 		let finalval;
 		if (!isNaN(currentVal)) {
 			if(type == 'minus') {
        if(currentVal > input.attr('min')) {
 					finalval=currentVal-1;
 				}
 				if(parseInt(input.val()) == input.attr('min')) {
 					$(this).attr('disabled', true);
 					finalval=currentVal = 1;
 				}
 			} else if(type == 'plus') {
        if(currentVal < input.attr('max')) {
 					finalval=currentVal+1;
 				}
 				if(parseInt(input.val()) == input.attr('max')) {
 					$(this).attr('disabled', true);
 					finalval=currentVal = 99;
 				}
 			}
 		} else {
 			input.val(0);
 		}
    input.val(finalval).change();
 		totalQuantityToUpdate=finalval + parseInt(initialCartQuantity);
 		form.find('input[name=quantity]').val(totalQuantityToUpdate-1);

 		if (bouncer)
 			clearTimeout(bouncer)
 			bouncer = setTimeout(() => {
        // update cart quantity
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
      }, 1000);
 	});

 	$('.input-number').focusin(function() {
 		$(this).data('oldValue', $(this).val());
 	});

 	$('.input-number').focusout(function(){
 		var currentValue = $(this).val();
 		var totalQuantityToUpdate;
 		var entryNumber = parseInt($(this).attr('entryNumber'));
 		var form = $('#updateCartForm' + entryNumber);
 		var productCode = form.find('input[name=productCode]').val();
 		var initialCartQuantity = form.find('input[name=initialQuantity]').val();
 		var entryNumber = form.find('input[name=entryNumber]').val();
 		totalQuantityToUpdate = parseInt(currentValue) + parseInt(initialCartQuantity);
 		form.find('input[name=quantity]').val(totalQuantityToUpdate-1);
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

//BL-927 Gift Card Purchase Amount
$('#add-to-gc').click(function(e) {
    $('.page-loader-new-layout').hide();
    var form = $('#giftCardPurchaseForm');
    var amount = form.find('input[name=amount]').val();
    if (amount < 25 || amount > 500) {
        $('.notification').show();
        return false;
    }
    else{
    	$('.notification').hide();
    	$('#signIn').modal('show');
    }
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

if($(".js-add-to-cart").hasClass("js-disable-btn"))
{
    $("#product-litepicker").addClass("date-notAvail");
    $("#mobile-product-litepicker").addClass("date-notAvail");
    $("#pickupDelivery .pickupDeliveryLink").addClass("d-none");
    $(" #productDates .input-group").addClass("red-border");
}

if($(".arrival").hasClass("nextAvailDate") && !$(".js-add-to-cart").hasClass("js-disable-btn")){
    $("#product-litepicker").addClass("date-notAvail");
    $("#mobile-product-litepicker").addClass("date-notAvail");
    $(" #productDates .input-group").addClass("red-border");
    $("#pickupDelivery .pickupDeliveryLink").addClass("d-none");
    if($(".js-add-to-cart").length == 1){
    	$(".js-add-to-cart").attr("disabled", true);
    }
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
					drag:false
   				} ).mount();
   				 document.querySelectorAll('.card-sliders').forEach(carousel => new Splide( carousel, {
   					type   : 'loop',
   					perPage: 1,
   					pagination: true,
   					drag   : true,
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
                                      document.getElementById(popUpId).setAttribute("disabled", true);
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
let bouncer;
$('.btn-number').click(function(e){
	e.preventDefault();
  fieldName = $(this).attr('data-field');
	type = $(this).attr('data-type');
	var input = $("input[name='"+fieldName+"']");
	var currentVal = parseInt(input.val());
	let finalval;
	if (!isNaN(currentVal)) {
		if(type == 'minus') {
      if(currentVal > input.attr('min')) {
        finalval=currentVal-1;
			}
			if(parseInt(input.val()) == input.attr('min')) {
				$(this).attr('disabled', true);
				finalval=currentVal = 1;
			}
    } else if(type == 'plus') {
      if(currentVal < input.attr('max')) {
        finalval=currentVal+1;
			}
			if(parseInt(input.val()) == input.attr('max')) {
				$(this).attr('disabled', true);
				finalval=currentVal = 99;
			}
    }
	} else {
		input.val(0);
	}

	input.val(finalval).change();
	var entryNumber = parseInt($(this).attr('entryNumber'));
	var form = $('#updateCartForm' + entryNumber);
	var productCode = form.find('input[name=productCode]').val();
	var initialCartQuantity = form.find('input[name=initialQuantity]').val();
	var entryNumber = form.find('input[name=entryNumber]').val();
	form.find('input[name=quantity]').val(finalval);
  if (bouncer)
		clearTimeout(bouncer)
		bouncer = setTimeout(() => {
      if (initialCartQuantity != finalval) {
				ACC.track.trackUpdateCart(productCode, initialCartQuantity,
						finalval);
				form.submit();
			}
    }, 1000);
});
$('.input-number').focusin(function(){
	$(this).data('oldValue', $(this).val());
});

$('.input-number').focusout(function(){
  var currentValue = $(this).val();
  var entryNumber = parseInt($(this).attr('entryNumber'));
  var form = $('#updateCartForm' + entryNumber);
  var productCode = form.find('input[name=productCode]').val();
  var initialCartQuantity = form.find('input[name=initialQuantity]').val();
  var entryNumber = form.find('input[name=entryNumber]').val();
  form.find('input[name=quantity]').val(currentValue);
   if (initialCartQuantity != currentValue) {
      	ACC.track.trackUpdateCart(productCode, initialCartQuantity,
      						currentValue);
      	form.submit();
   }
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
	var redirectToCart = false;
	$.ajax({
		url : ACC.config.encodedContextPath + '/cart/emptyCart',
		type : "GET",
		beforeSend: function(){
         $('.page-loader-new-layout').show();
     },
		success : function(data) {
			redirectToCart = addProductToCart(productCode, serialCode);
		},
		complete:function(){
			if(redirectToCart)
			{
				
				window.location.href = ACC.config.encodedContextPath + '/cart';
			}
		},
		error : function(xht, textStatus, ex) {
		  $('.page-loader-new-layout').hide();
			console.log("Error while removing cart entries");
		}
	});
});
}

function addProductToCart(productCode, serialCode){
	var redirectToCart = false;
	if(serialCode === 'serialCodeNotPresent')
	{
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
		return redirectToCart;
	}
	else
	{
		 $.ajax({
			  	url : ACC.config.encodedContextPath + "/cart/usedgearadd",
			  	async: false,
			  	type : 'GET',
			  	data : {
			  		productCodePost : productCode,
			  		serialProductCodePost : serialCode
			  	},
			  	success : function(response) {
			  	  $('.page-loader-new-layout').hide();
			  	  $('#addToCartModalDialog').addClass('modal-lg');
			  	  $('#addToCartModalDialog').html(response.addToCartLayer);
			  	  redirectToCart = true;
			  	},
			  	complete: function(){
			  		$("#addToCart").removeClass("show");
                    $("#addToCart").hide();
                    startUsedGearCartTimer();
			  	},
			  	error : function(jqXHR, textStatus, errorThrown) {
			  	  $('.page-loader-new-layout').hide();
			  		$('.modal-backdrop').addClass('remove-popup-background');
			  		// log the error to the console
			  		console.log("The following error occurred: " + jqXHR, textStatus,
			  				errorThrown);
			  	}
			  });
		 return redirectToCart;
	}
 return redirectToCart;
}

//BL-466 reload page, when user closes add to rental modal from empty cart page.
function reloadEmptyCartPageOnModalClose(){
$('.emptyCart-modalClose').click(function(e){
  window.location.reload();
});
}

$('.emailSubscr_btn').click(function (e){

		var email = document.getElementById("emailSubscr_txt").value;
		//validate email
			$.ajax({
				 url: ACC.config.encodedContextPath + "/subscribe-email/?emailId="+email,
				 type: "GET",
				success: function (data) {
					if (data == "success") {

					}
				}
			});
			$("#emailSubscr_txt").val("");
});

//Added code for used gear addToCart 
$('.bl-serial-add').click(function (e)
	{
	 e.preventDefault();
	 var submitForm = $('#serialSubmitForm');
	 var csrfTokan = createHiddenParameter("CSRFToken",$(ACC.config.CSRFToken));	
	 var productCode = createHiddenParameter("productCodePost",$(this).attr('data-product-code'));
     var serialCode = createHiddenParameter("serialProductCodePost", $(this).attr('data-serial'));
     
     if(serialCode == '' || serialCode == undefined){
    serialCode = "serialCodeNotPresent";
    }
    
     submitForm.append($(productCode)); 
     submitForm.append($(serialCode));
     submitForm.append($(csrfTokan));
     submitForm.submit();
    
     startUsedGearCartTimer();

  });

function createHiddenParameter(name, value) {
    var input = $(HTML.INPUT).attr(HTML.TYPE, "hidden").attr("name", name).val(
        value);
    return input;
}

//BL -471 Used Gear cart timer

$('.usedgear-signout').on("click", function (e) {
	fetchdata();
	clearInterval(z);
	localStorage.removeItem('saved_countdown');
	
});

function startUsedGearCartTimer() {
		
		localStorage.setItem('StartCartTimer',"StartCartTimer");	
	}
	var timeStop="false";

	var storeCartTime = localStorage.getItem('StartCartTimer');
	if(  storeCartTime == "StartCartTimer")
	{
	    localStorage.removeItem('saved_countdown');
	    localStorage.removeItem('StartCartTimer');
		var timercount= document.getElementById("timer-count").getAttribute("value") ;
		var newTimer =new Date().getTime() + (timercount)*1000 +2000;
	   
	}

	var saved_countdown = localStorage.getItem('saved_countdown');
	if(saved_countdown==null)
	{
	var new_countdown= new Date().getTime() + (timercount)*1000 +2000;
	localStorage.setItem('saved_countdown',new_countdown);
	}

	else{
		newTimer = saved_countdown ;
	}

	timeStop = localStorage.getItem('timeStop');
	if(timeStop=="true"){
	    clearInterval(z);
	    localStorage.removeItem('timeStop');
	    localStorage.removeItem('saved_countdown');
	}

	if(isNaN(newTimer) )
	{
		localStorage.removeItem('saved_countdown');
		 clearInterval(z);
	}
	else if(timeStop==null){
	var z= setInterval(updateCountdown,1000);
	}

	function updateCountdown(){
	  var now = new Date().getTime();


	// Find the distance between now and the allowed time
	var distance =newTimer - now;

	let minutes = Math.floor(distance/60000);

	// Time counter
	var counter = Math.floor((distance % (1000 * 60)) / 1000);
	counter =counter<10?'0'+counter:counter;

	// If the count down is over, write some text
	if (minutes<0) {
	clearInterval(z);
	localStorage.removeItem('saved_countdown');
	fetchdata();
	}
	
	

	if(minutes<0){
		document.getElementById("usedTimer").innerHTML = "Expired";
	}
	else{
	document.getElementById("usedTimer").innerHTML = `${minutes}:${counter}`;
	}

	};

	function fetchdata(){
	var timerStop = true;
	$.ajax({
	url: ACC.config.encodedContextPath + "/cart/cartTimerOut",
	type: "POST",
	data:{usedGearTimerEnd: timerStop},
	success: function(response){
	// Perform operation on the return value
	window.location.href = ACC.config.encodedContextPath + "/cart";
	localStorage.setItem("timeStop","true");
	},
	error: function (xht, textStatus, ex) {
	console.log("Error while removing cart entries");
	}
	});

	}
	
	$('.js-add-to-used-cart').on("click",function(e) {
        e.preventDefault();
        var form = $('#giftCardPurchaseForm');
        var amount = form.find('input[name=amount]').val();
        if (amount < 25 || amount > 500) {
            $('.notification').show();
            $("body").removeClass("modal-open");
            $("body").removeAttr("style");
            $('tn-gift-card-pdp').show();
            $(".modal-backdrop").remove();
            return false;
        }
        if (amount >25 || amount < 500){
        	 $('.notification').hide();
        }
         var form = $('#giftCardPurchaseForm');
    
         var productCode = $(this).attr('data-product-code');
         var serialCode = $(this).attr('data-serial');
         // This data used for GA
         var productName = $(this).attr('data-product-name');
         var productBrand =$(this).attr('data-product-brand');
         var productCategory = $(this).attr('data-product-category');
         var redirectToCart = false;
         if(serialCode == '' || serialCode == undefined){
         serialCode = "serialCodeNotPresent";
         }
         $.ajax({
                    url: ACC.config.encodedContextPath + "/cart/usedgearadd",
                    type: 'GET',
                    data: form.serialize() + "&productCodePost="+productCode+"&serialProductCodePost="+serialCode,
                    beforeSend: function(){
                        $('.page-loader-new-layout').show();
                    },
                    success: function (response) {
                    	var addToCartGiftCardLayer = response.addToCartGiftCardLayer;
                    	var giftCardNotAllowedWarninLayer = response.giftCardNotAllowedWarninLayer;
                    	var multipleGiftCardWarningLayer = response.multipleGiftCardWarningLayer;
                    	if(addToCartGiftCardLayer != undefined && addToCartGiftCardLayer != '')
                    	{
                    		$('#addToCartModalDialog').html(response.addToCartGiftCardLayer);
                    		 if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
                                 ACC.minicart.updateMiniCartDisplay();
                              }
                    		onGiftCardCloseModal();
                     		setTimeout(function(){
                          	   $("#signUp").modal('hide');
                          	   $("#addToCart").addClass("show");
                                 $("#addToCart").show();
                             },500);
                     		mixedProductInterception(productCode, serialCode);
                    	}
                    	 else if(giftCardNotAllowedWarninLayer != undefined && giftCardNotAllowedWarninLayer != '')
                     	{
                     		$('#addToCartModalDialog').html(response.giftCardNotAllowedWarninLayer);
                     		 if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
                                  ACC.minicart.updateMiniCartDisplay();
                               }
                     		onGiftCardCloseModal();
                      		setTimeout(function(){
                           	   $("#signUp").modal('hide');
                           	   $("#addToCart").addClass("show");
                                  $("#addToCart").show();
                              },500);
                      		mixedProductInterception(productCode, serialCode);
                     	}
                     	else if(multipleGiftCardWarningLayer != undefined && multipleGiftCardWarningLayer != '')
                     	{
                     		$('#addToCartModalDialog').html(response.multipleGiftCardWarningLayer);
                     		 if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
                                  ACC.minicart.updateMiniCartDisplay();
                               }
                     		onGiftCardCloseModal();
                      		setTimeout(function(){
                           	   $("#signUp").modal('hide');
                           	   $("#addToCart").addClass("show");
                                  $("#addToCart").show();
                              },500);
                      		mixedProductInterception(productCode, serialCode);
                     	}
                    	else{
                    	var addToCartLayer = response.addToUsedCartLayer;
                    	if(addToCartLayer == undefined || addToCartLayer == '')
                    	{
                       var productType ='used gear';
                       var quantity =1;
                       ACC.track.trackAddToCart(productCode, productName,productBrand,productType,productCategory,quantity);
                       // Tealium event for ATC
                       utag.link({
                             "tealium_event" : "cart_add",
                              "productSKU"    : '["'+productCode+'"]',
                              "ProductName"   : '["'+productName+'"]',
                              "quantity"      : '["'+quantity+'"]',
                               "add_to_cart_used"  : "1",
                               "isBuy"            : "1"
                                });
                    		redirectToCart = true;
                    		startUsedGearCartTimer();
                    	}
                    	else
                    	{
                    		$('#addToCartModalDialog').html(response.addToUsedCartLayer);
                    		onUsedCloseModal();
                    		setTimeout(function(){
                         	   $("#signUp").modal('hide');
                         	   $("#addToCart").addClass("show");
                                $("#addToCart").show();
                            },500);
                    	  mixedProductInterception(productCode, serialCode);
                    	}
                    	
                    	}
                    },
                    complete : function() {
                    	if(redirectToCart)
                    	{
                    		window.location.href = ACC.config.encodedContextPath + '/cart';
                    	}
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

	function onGiftCardCloseModal()
	{
		$("#closeGiftCardModal , #cancelGiftCardModal").on("click", function(event){
			event.preventDefault();
			var doReload = $("#doReload").val();
			if(doReload === 'true')
			{
				location.reload();
			}
			else
			{
				$("#addToCart").removeClass("show");
	            $("#addToCart").hide();
			}
		});
	}
	
function onUsedCloseModal()
{
	$("#closeUsedCartModal , #cancelUsedCartModal").on("click", function(event){
		event.preventDefault();
		var doReload = $("#doReload").val();
		if(doReload === 'true')
		{
			location.reload();
		}
		else
		{
			$("#addToCart").removeClass("show");
            $("#addToCart").hide();
		}
	});
}

//BL-625 place order with order notes.
$('#placeOrderSummary').one("click", function(e) {
	$('#placeOrder').click();
});

 var reviewPageError = $('.js-reviewPage-error').val();
   if( reviewPageError!= undefined &&  reviewPageError!=null){
    reviewPageError = reviewPageError.trim();
     	utag.link({
        "tealium_event"    : "place_order_error",
         "checkout_error"     : '"'+reviewPageError+'"'
          });
     window.mediator.publish('placeOrderClick', {
    	reviewPageError: reviewPageError
    	});
    }
 $('#placeOrder').one(
			"click",
			function(e) {
				var submitForm = $("#placeOrderForm1");
				
				var csrfTokan = createHiddenParameter("CSRFToken",
						$(ACC.config.CSRFToken));
				if($("#giftCardPurchaseForm").length > 0)
				{
					var giftCardForm = $("#giftCardPurchaseForm");
					var name = createHiddenParameter("name",giftCardForm.find('input[name="name"]').val());
					var email = createHiddenParameter("email",giftCardForm.find('input[name="email"]').val());
					var message = createHiddenParameter("message",giftCardForm.find('textarea[name="message"]').val());
					submitForm.append($(name));
					submitForm.append($(email));
					submitForm.append($(message));
				}			
				submitForm.append($(csrfTokan));			
				submitForm.submit();
});
   
//Handled min and max character for order notes.
var inputQuantity = [];
$(function() {
	$(".order-notes").on(
			"keyup",
			function(e) {
				var $field = $(this), val = this.value;
				$thisIndex=parseInt($field.data("idx"),10); 
				if (val.length > Number($field.attr("maxlength"))) {
					val = val.slice(0, 5);
					$field.val(val);
				}
				inputQuantity[$thisIndex] = val;
			});
});

//Print order confirmation page
$('#printOrderConfirmation').on("click",function(e) {
		e.preventDefault();
		var submitForm = $("#printOrderConfirmationForm");
		submitForm.submit();
});
$('.remove-doc').on("click",function(e) {
	
	e.preventDefault();
var clickedForm = this.getAttribute("data-code");
$("#clickedForm").val(clickedForm);
});

//remove document submit
$('#remove-doc-submit-button').on("click",function(e) {
	$('.page-loader-new-layout').show();
	e.preventDefault();
    var clickedForm = $("#clickedForm").val();
	var submitForm = $("#" + clickedForm );
	submitForm.submit();
});

$('.remove-document').on("click",function(e) {
	e.preventDefault();
var clickedFormDocument = this.getAttribute("data-code");
$("#clickedFormDocument").val(clickedFormDocument);
});

$('#remove-document-submit-button').on("click",function(e) {
	$('.page-loader-new-layout').show();
	e.preventDefault();
    var clickedForm = $("#clickedFormDocument").val();
	var submitForm = $("#" + clickedForm );
	submitForm.submit();
});
function hideShorting(){ 
    $('.container').on('click', function(){  
	   $(".product-sort").find(".bootstrap-select").removeClass('open')
   });
  }; 
  hideShorting();
  
  $("#submitCard").on("click",function(e) {
	  e.preventDefault();
		var submitForm = $("#giftCardPurchaseForm");
		submitForm.submit();
  });

  //BL-917: Replacement order
  $('.return-button-cls').on("click",function(e) {
    e.preventDefault();
	var label  = [];
	var fvalue = []

	$(".myLabel").each(function (index) {
	label.push($(this).text());
	});

	$(".Myqtynumber").each(function (index) {
		fvalue.push(label[index]+':'+$(this).val()+'%');
	}) ;

 	var productList=fvalue.join("");
         	var url=ACC.config.encodedContextPath + '/my-account/returnOrderRequest';
         	var orderCode=$('#order-code').val();
         		$.ajax({
                     		url : url,
                     		type : "POST",
                     		data: {productList:productList,orderCode:orderCode},
                     		success : function(data) {
                     			window.location.href = ACC.config.encodedContextPath + "/";
                     		},
                     		error : function(xht, textStatus, ex) {
                     			console.log("Error while order return");
                     		}
                     	});
	 
   });

   $('.replacement-minus').on("click",function(e) {
             e.preventDefault();
             $('.return-button-cls').attr('disabled', true);
             var fvalue = []
             $(".return-qty-check").each(function (index) {
                  if($(this).val()==0) {
                     $('.return-minus-btn'+index).prop('disabled', true);
                  }
                  if($(this).val()!=0) {
                    $('.return-button-cls').attr('disabled', false);
                  }
             }) ;
          });

      $('.replacement-plus').on("click",function(e) {
          e.preventDefault();
          $('.return-button-cls').attr('disabled', false);
          var fvalue = []
          $(".return-qty-check").each(function (index) {
               if($(this).val()==$('#originalQty'+index).val()) {
                  $('.return-plus-btn'+index).prop('disabled', true);
               }
          }) ;
       });
      
 $('.gift-card-add-t-cart-popup').on('click', function(){      	   
  	$(".modal").hide();  
  	$("body").removeClass("modal-open");     	   
  	$("body").removeAttr("style");         
    $(".modal-backdrop").remove();
  }); 
 
 
//BL-1134 Gift Card Apply
 $('#applyGcCodeForModifyPayment').click(function (e) {
	 
 	e.preventDefault();
   var giftCardForm = $("#giftCardForm");
   var $form = $(this);
 	var gcCode = $("#gift-card-apply-gift-card-number").val();
 	var gcOrderCode = $("#orderCode").val();
 	$.ajax({
 		url: giftCardForm.attr('action'),
 		type: giftCardForm.attr("method"),
 		data: {code: gcCode, orderCode : gcOrderCode},
 		
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
 
 
 
 //BL-1134 Remove Gift Card
 $('.remove-gift-card-modify-order').on("click", function(e) {
 	
      e.preventDefault();
    
      var itemIndex = $(this).data("index");
      var $form = $(document).find('#removeGiftCardForm' + itemIndex);
      var gcOrderCode = $("#orderCode").val();
      var gcCode = $("#gift-code"+ itemIndex).val();
      var method = $form.attr("method") ? $form.attr("method").toUpperCase() : "POST";
      $.ajax({
          url: $form.attr("action"),
          data: $form.serialize() + "&orderCode="+gcOrderCode + "&gcCode="+gcCode,
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
 
 const mediaQuery = window.matchMedia('(min-width: 769px)')
 if (mediaQuery.matches) {
	 var div = document.getElementById('pr-reviewsnippet');
	 if(div != undefined){
		 div.remove();
     }
 }
 setTimeout(function(){
	let hideArrow = document.querySelectorAll("#product-slider ul li").length;
	if (hideArrow < 2 ){                                              
	document.querySelector("#product-slider .splide__arrows").style.visibility="hidden";
	} },1000);
 
$('.bookmark-addToCart').on("click", function(e) {
	e.preventDefault();
	var formId = $(this).data("id");
	$.ajax({
        url: ACC.config.encodedContextPath + "/bookmark/onlyRentalCartPresent",
        type: 'POST',
        success: function (response) {
        	if(response.giftCardNotAllowedWarninLayer == 'success')
			{
				var formToSubmit = $("#" + formId);
				formToSubmit.submit();
			}
			else if(response != '')
			{        	
				var giftCardNotAllowedWarninLayer = response.giftCardNotAllowedWarninLayer;
		      	   if(giftCardNotAllowedWarninLayer != undefined && giftCardNotAllowedWarninLayer != '')
		          	{
		          		$('#addToCartModalDialog').html(response.giftCardNotAllowedWarninLayer);
		          		$('#gcAlreadyAddedWarning').modal("show");
		          	}
			}
        },
        error: function (jqXHR, textStatus, errorThrown) {
              $('.modal-backdrop').addClass('remove-popup-background');
              // log the error to the console
              console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
        }
	});
});
