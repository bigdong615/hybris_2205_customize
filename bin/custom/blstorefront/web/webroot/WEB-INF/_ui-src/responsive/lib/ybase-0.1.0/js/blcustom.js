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
 
 
 
 
 //BL - Gift Card
 
 
 $(document).on("click", "#applyCode-d", function(e) {
     var me = $(this);
     e.preventDefault();
     if ( me.prop('disabled') ) {
         return;
     }
     me.prop('disabled', true);
     var method = "POST";
     var code1;
     $('input[type="text"].inputCode-d').each(function() {
         if ($(this).val() != "") {
             code1 = $(this).val();
         }
     });
     var code = code1;

     // Trigger coupon code used GTM event
    /* if (typeof trackCouponCodeUsedGoogleTagManagerEvent === "function") {
         trackCouponCodeUsedGoogleTagManagerEvent(code);
     }*/

     $.ajax({
         url: ACC.config.encodedContextPath + '/checkout/apply',
         data: {
             code: code
         },
         type: method,
         success: function(data, status, xhr) {
             var ct = xhr.getResponseHeader("content-type") || "";
             if (ct.indexOf('html') > -1) {
                var homepage = "/";
                parent.window.location = homepage;
             }else if (data == "") {
                  var currentURL = window.location.href;
               // Trigger coupon code used GTM event
               //   trackCouponCodeUsedGoogleTagManagerEvent(code);
                  parent.window.location = currentURL;

             } else {
                 $('.tc-giftcard-coupon-alert').text(data);
                 $('.tc-giftcard-coupon-alert').removeAttr('hidden');
                 $('.promotion_message').attr('hidden','');
             }
         },
         complete: function() {
             me.prop('disabled', false);
         },
         error: function(error) {
             console.log("voucher error");
             me.prop('disabled', false);
         }
     });
 });


 $('.js-release-voucher-remove-btn').on("click", function(e) {
     e.preventDefault();
     var method = "POST";
     var voucherForm = {};
     var code = $(this).attr('id');

     voucherForm["voucherCode"] = $(this).attr('id');
     $.ajax({
         url: ACC.config.encodedContextPath + '/checkout/remove',
         data: {
             code: code
         },
         async: false,
         type: method,
         success: function(data, status, xhr) {
             var currentURL = window.location.href;
             parent.window.location = currentURL;
         },
         error: function(error) {
             console.log("voucher error");
         }
     });
 })
