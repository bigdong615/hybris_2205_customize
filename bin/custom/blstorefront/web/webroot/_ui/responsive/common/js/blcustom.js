//BL-467 clear cart functionality from cart page.
$('.clear-cart-page').on("click", function (event) {
            $.ajax({
                     url: ACC.config.encodedContextPath + '/cart/emptyCart',
                     type: "GET",
                     success: function (data) {
                     window.location.reload();
                     },
                     error: function (xht, textStatus, ex) {
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
                                   success: function (response) {
                                      $('#addToCartModalDialog').html(response.addToCartLayer);
                                      if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
                                         ACC.minicart.updateMiniCartDisplay();
                                      }
                                      updateQuantity();
                                   },
                                   error: function (jqXHR, textStatus, errorThrown) {
                                         $('.modal-backdrop').addClass('remove-popup-background');
                                         // log the error to the console
                                         console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
                                   }
                        });

 });

 // BL-454 update quantity from rental add to cart popup.
 function updateQuantity() {
    $('.js-update-quantity').on("change", function (e){
        			  var newCartQuantity = $(this).find(":selected").val();
        				e.preventDefault();
        				var entryNumber = $(this).attr('id').split("_");
        				var form = $('#updateCartForm' + entryNumber[1]);
        				var productCode = form.find('input[name=productCode]').val();
        				var initialCartQuantity = form.find('input[name=initialQuantity]').val();
        				var entryNumber = form.find('input[name=entryNumber]').val();
        				form.find('input[name=quantity]').val(newCartQuantity);
                $.ajax({
                          url: ACC.config.encodedContextPath + "/cart/updateQuantity",
                          type: 'POST',
                          data: form.serialize(),
                          beforeSend: function(){
                             $('.page-loader-new-layout').show();
                          },
                          success: function (response) {
                             if (typeof ACC.minicart.updateMiniCartDisplay == 'function') {
                                  ACC.minicart.updateMiniCartDisplay();
                             }
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
$(".gc-message input").focus(function () {
	$(this).siblings(".gc-message").hide();
});

$('#applyGcCode').click(function (e) {
	e.preventDefault();
  var giftCardForm = $("#giftCardForm");
  if (!giftCardForm.valid()) {
		return false
	}
	var $form = $(this);
	var gcCode = $("#gift-card-apply-gift-card-number").val();
	var formBtnSubmit = $(this).find('[type="submit"]');
	formBtnSubmit.prop("disabled", true).attr("disabled", "disabled");
  $.ajax({
		url: giftCardForm.attr('action'),
		type: giftCardForm.attr("method"),
		data: {
			code: gcCode
		},
    success: function (data) {
			formBtnSubmit.prop("disabled", false).removeAttr("disabled");
			window.location.reload();
    }
	});
});

$("#giftCardForm").validate({
	errorClass: "error",
	errorElement: "span",
	focusInvalid: false,
	rules: {
		giftCardNumber: {
			required: true
    },
  },
	messages: {
		giftCardNumber: {
			required: "Uh-oh, please enter a gift card code"
    },
  },
	errorPlacement: function (error,
		element) {
		if ($(element).is('select')) {
			element.parent().after(error);
		} else {
			error.insertAfter(element);
		}
	},
	highlight: function (element) {
		$(element).parent().addClass(
			"form-error");
	},
	unhighlight: function (element) {
		$(element).parent().removeClass(
			"form-error");
	}
});

//BL-563 Remove Gift Card
$('.remove-gift-card').on("click", function(e) {
     e.preventDefault();
     var method = "POST";
     var giftCardForm = {};
     var code = $(this).attr('id');
          giftCardForm["giftCardCode"] = $(this).attr('id');
          $.ajax({
              url: ACC.config.encodedContextPath + '/checkout/removeGiftCard',
              data: {
                  code: code
              },
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

}
$(".js-hr-tag").last().hide();

$(document).ready(function() {
    var accordianData = $('.accordion-data > .content').text();
    var aff_accordian = "";
    if (accordianData) {
        accordianData = accordianData.split(',');
        if (accordianData.length > 0) {
            accordianData.forEach(function(data, index) {
                if ((index + 1) % 2 == 0) {
                    aff_accordian += '<div class="accordion-content"><p>' + data + '</p></div>'
                } else {
                    aff_accordian += '<h4 class="accordion-title">' + data + '</h4>';
                }
            });
            $('.accordion-container').append(aff_accordian);
        }
    }
    $(".accordion-content").css("display", "none");
    $(".accordion-title").on('click', function() {
        $(".accordion-title").not(this).removeClass("open");
        $(".accordion-title").not(this).next().slideUp(300);
        $(this).toggleClass("open");
        $(this).next().slideToggle(300);
    });
		if($('.contact-live-chat').length > 0){
			liveagent.init(
			"https://d.la2-c1-iad.salesforceliveagent.com/chat",
			"5721I0000005VsD",
			"00D1I000002xB0t"
			);
		}
	  
});