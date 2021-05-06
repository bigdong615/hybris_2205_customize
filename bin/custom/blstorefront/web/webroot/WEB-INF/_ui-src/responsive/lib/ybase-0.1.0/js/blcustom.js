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
                        var recognise = $(this).attr('data-popup');
                        if(serialCode == '' || serialCode == undefined){
                        serialCode = "serialCodeNotPresent";
                        }
                        if(recognise == '' || recognise == undefined){
                        recognise = "notClickedFromModal";
                        }
                        $.ajax({
                                   url: ACC.config.encodedContextPath + "/cart/add",
                                   type: 'POST',
                                   data: {productCodePost: productCode,serialProductCodePost:serialCode, popUpRecognisePost:recognise},
                                   success: function (response) {
                                      $('#addToCartModalDialog').html(response.addToCartLayer);
                                      updateQuantity();
                                      addToCartFromModal();
                                      addId();
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
                          success: function (response) {
                             console.log("Quantity updated");
                          },
                          error: function (jqXHR, textStatus, errorThrown) {
                            console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
                          }
                });
    });
 }

function addId(){
let seemore = document.querySelectorAll(".SeeMore2");
   for(var i=0;i<seemore.length;i++){
          seemore[i].id ="abc-"+i;
          alert("for loop is running")
           }
}

let seemore = document.querySelectorAll(".SeeMore2");
for(var i=0;i<seemore.length;i++){
seemore[i].id ="abc-"+i;
}

  //BL-454 add to cart
   function addToCartFromModal(){
  $('.js-add-to-cart1').on('click',function(e) {
                        e.preventDefault();
                         let z= this.getAttribute("id");
                            var index = $( ".js-add-to-cart1" ).index( this );
                            document.getElementById(z).innerHTML= "Added";

                         var productCode = $(this).attr('data-product-code');
                         var serialCode = $(this).attr('data-serial');
                         var recognise = $(this).attr('data-popup');
                         if(serialCode == '' || serialCode == undefined){
                        serialCode = "serialCodeNotPresent";
                        }
                         if(recognise == '' || recognise == undefined){
                         recognise = "notClickedFromModal";
                         }
                         $.ajax({
                                    url: ACC.config.encodedContextPath + "/cart/add",
                                    type: 'POST',
                                    data: {productCodePost: productCode, popUpRecognisePost:recognise,serialProductCodePost:serialCode},
                                    success: function (response) {
                                    alert("product added to cart");
                                      //addToCartToAdded();
                                    },
                                    error: function (jqXHR, textStatus, errorThrown) {
                                          $('.modal-backdrop').addClass('remove-popup-background');
                                          // log the error to the console
                                          console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
                                    }
                         });

  });