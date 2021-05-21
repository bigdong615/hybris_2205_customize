ACC.account = {

	_autoload: [
		"accountLink"
	],

	accountLink: function () {
	        var bookmarkicons = document.querySelectorAll(".bookmarkicons");
           console.log(bookmarkicons.length);
              for(var i=0;i<bookmarkicons.length;i++){
                     bookmarkicons[i].id ="bookmarkicons-"+(i+1);
                     alert("for loop is running");
                      }


          $(document).on('click',".js-add-to-wishlist", function(e) {
          alert("inside wishlist");
                                  e.preventDefault();
  //                                  var productCode = $(this).attr('data-product-code');
  //                                  var targetUrl = $(".add_to_wishList_form").attr("action");

                                   var bookmarkId =this.getAttribute("id");
                                     console.log(bookmarkId);
                                     console.log(document.getElementById(bookmarkId));
                                     console.log(ACC.config.encodedContextPath + "/removewishlist");
                                   if(document.getElementById(bookmarkId).classList.contains("bookmark-checked")){
                                    var productCode = $(this).attr('data-product-code');
                                   $.ajax({
                                      url: ACC.config.encodedContextPath + "/removewishlist",
                                      type: 'POST',
                                      data: {removeproductCode: productCode},
                                      success: function (response) {
                                      if(response == success){
                                      alert("product removed from wishlist");
                                      //shd make button red here
                                      }
                                      else{
                                      //shd not make button red here
                                      }

                                        },
                                      error: function (jqXHR, textStatus, errorThrown) {
                                          console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
                                       }
                                   });
                                   document.getElementById(bookmarkId).classList.remove("bookmark-checked");
                                   }
                                   else{
                                    var productCode = $(this).attr('data-product-code');
                                    var targetUrl = $(".add_to_wishList_form").attr("action");
                                    $.ajax({
                                            url: targetUrl,
                                            type: 'POST',
                                            data: {productwishlistCode: productCode},
                                             success: function (response) {
                                                    alert("product added to wishlist");
                                                   },
                                             error: function (jqXHR, textStatus, errorThrown) {
                                                 // log the error to the console
                                             console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
                                             }
                                     });
                                   document.getElementById(bookmarkId).classList.add("bookmark-checked");
                                   }
  //
  //var productCode = $(this).attr('data-product-code');
  //                                 $.ajax({
  //                                            url: ACC.config.encodedContextPath + "/removewishlist",
  //                                            type: 'POST',
  //                                            data: {removeproductCode: productCode},
  //                                            success: function (response) {
  //                                            alert("product removed to wishlist");
  //                                              //addToCartToAdded();
  //                                            },
  //                                            error: function (jqXHR, textStatus, errorThrown) {
  //                                                  // log the error to the console
  //                                                  console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
  //                                            }
  //                                 });

            });



  // $(document).on('click',".js-remove-from-wishlist", function(e) {
  //        alert("inside wishlist");
  //                                e.preventDefault();
  //                                 var productCode = $(this).attr('data-product-code');
  //                                 var targetUrl = $(".remove_from_wishList_").attr("action");
  //                                 $.ajax({
  //                                            url: targetUrl,
  //                                            type: 'POST',
  //                                            data: {removeproductCode: productCode},
  //                                            success: function (response) {
  //                                            alert("product added to cart");
  //                                              //addToCartToAdded();
  //                                            },
  //                                            error: function (jqXHR, textStatus, errorThrown) {
  //                                                  // log the error to the console
  //                                                  console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
  //                                            }
  //                                 });
  //
  //          });


  	}
  	};



		/** Added for BL-31 to make validation for registration **/






	}
};