ACC.account = {

	_autoload: [
		"accountLink"
	],

	accountLink: function () {
		/* This function is responsible for providing form object for the login popup*/
		$(document).on("click", ".js-login-popup", function (e) {
			e.preventDefault();
			$('#signIn').html("");
			$.ajax({
				url: $(this).data("link"),
				success: function (result) {
					$('#signIn').html(result);
					setTimeout(function(){$("#signIn").modal('show');},500); 
				}
			})
		});

		/*This function is responsible for providing form object for the sign up popup*/
		$(document).on("click", ".js-signUp-popup", function (e) {
			e.preventDefault();
			$('#signUp').html("");
			$.ajax({
				url: $(this).data("link"),
				success: function (result) {
					$('#signUp').html(result);
					setTimeout(function(){$("#signUp").modal('show');},500)
				}
			})
		});

		$(document).on("click", ".js-forgot-password", function (e) {
			e.preventDefault();
					setTimeout(function(){$("#forgotPass").modal('show');},500)
		});

		/** Added for BL-31 to make validation for registration **/
		$(document).on("click", ".js-signUp-popup-validation", function (e) {
			e.preventDefault();
			var formValues = $('#signUppopup-validation').serialize();
			var targetUrl = $(this).val();
			if ($('#register-form-id').val() !== '' && $('#password').val() !== '' && $('#checkPwd-form-id').val() !== '') {
			if(!$("#errorMessages_sigin_errorbox").hasClass("d-none")){
            	$("#errorMessages_sigin_errorbox").addClass("d-none");
            	}
				$.ajax({
					type: "POST",
					data: formValues,
					url: targetUrl,
					success: function (response) {
				   // This code added temporary to show the error message. Once we have the user story needs to change the code accordingly
						var splitValue = '';
						if (response.startsWith("Error:")) {
							splitValue = response.split(":");
							if (splitValue.includes('register.email.invalid')) {
							if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
                                  $("#errorMessages_sigin_errorbox").removeClass("d-none");
                                  }
								$("#errorMessages_sigin_email").html("Please enter a valid email");
							}
							if (splitValue.includes('register.pwd.invalid')) {
								if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
									$("#errorMessages_sigin_errorbox").removeClass("d-none");
								}
								$("#errorMessages_sigin_pwd").html("Your password needs to be at least 6 characters long");
							}
							if (splitValue.includes('validation.checkPwd.equals')) {
								if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
									$("#errorMessages_sigin_errorbox").removeClass("d-none");
								}
								$("#errorMessages_sigin_chkPwd").html("Your passwords did not match, please enter them again");
							}
						} else if (response === 'registration.error.account.exists.title') {
							if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
								$("#errorMessages_sigin_errorbox").removeClass("d-none");
							}
							$("#errorMessages_sigin_email").html("Whoops, it looks like you’re already signed up with a BorrowLenses account");
						} else {
							$("#errorMessages_sigin_errorbox").addClass("d-none");
							location.reload();
						}
					},
					error: function (e) {
						// do nothing
					}
				});
			}
			if ($('#register-form-id').val() === '') {
				if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
					$("#errorMessages_sigin_errorbox").removeClass("d-none");
				}
				$("#errorMessages_sigin_email").html("Looks like you forgot to enter your email address");
			} else {
				$("#errorMessages_sigin_email").html("");
			}
			if ($('#password').val() === '') {
				if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
					$("#errorMessages_sigin_errorbox").removeClass("d-none");
				}
				$("#errorMessages_sigin_pwd").html("Your password needs to be at least 6 characters long");
			} else {
				$("#errorMessages_sigin_pwd").html("");
			}
			if ($('#checkPwd-form-id').val() === '') {
				if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
					$("#errorMessages_sigin_errorbox").removeClass("d-none");
				}
				$("#errorMessages_sigin_chkPwd").html("Be sure to provide your password confirmation");
			} else {
				$("#errorMessages_sigin_chkPwd").html("");
			}
		});


		$(document).on("click", ".js-login-popup-validation", function (e) {
			e.preventDefault();
			var formValues = $('#login-popup-validation').serialize();
			var targetUrl = $(this).val();
			if ($('#j_username').val() !== '' && $('#j_password').val() !== '') {
				$.ajax({
					type: "POST",
					data: formValues,
					url: targetUrl,
					success: function (response) {
					 // This code added temporary to show the error message. Once we have the user story needs to change the code accordingly
						if (response === 'login.error.account.not.found.title') {
							$("#errorMessages_login").removeClass("d-none");
							$("#errorMessages_login").html("Your Email or password was incorrect");
						} else {
							location.reload();
						}
					},
					error: function (e) {
						// do nothing
					}
				});
			} else {
				$("#errorMessages_login").removeClass("d-none");
				$("#errorMessages_login").html("Your Email or password was incorrect");
			}
		});

		 var bookmarkicons = document.querySelectorAll(".bookmarkicons");
               console.log(bookmarkicons.length);
                  for(var i=0;i<bookmarkicons.length;i++){
                         bookmarkicons[i].id ="bookmarkicons-"+(i+1);
                         alert("for loop is running");
                          }


              $(document).on('click',".js-add-to-wishlist", function(e) {
              alert("inside wishlist");
                                      e.preventDefault();
                                       var bookmarkId =this.getAttribute("id");
                                        var productCode = $(this).attr('data-product-code');
                                       // var targetAddUrl = $(".add_to_wishList_form").attr("action");
                                        var targetAddUrl    = ACC.config.encodedContextPath + "/wishlist/add"
                                        var targetRemoveUrl = ACC.config.encodedContextPath + "/removewishlist"
                                        var bookmarkValue = $(this).attr('data-bookmark-value');
                                        if(document.getElementById(bookmarkId).classList.contains("bookmark-checked"))
                                        {
                                        $.ajax({
                                                       url: targetRemoveUrl,
                                                       type: 'POST',
                                                       data: {removeproductCode: productCode},
                                                       success: function (response) {
                                                     //  alert(success);
                                                       if(response === 'Success'){
                                                        alert("product removed from wishlist");
                                                        document.getElementById(bookmarkId).classList.remove("bookmark-checked");
                                                       }
                                                       else{
                                                       alert("found error while removing the product");
                                                       }

                                                         },
                                                       error: function (jqXHR, textStatus, errorThrown) {
                                                           console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
                                                        }
                                                 });

                                        }
                                        else{
                                       $.ajax({
                                          url: targetAddUrl,
                                          type: 'POST',
                                          data: {productwishlistCode: productCode},
                                          success: function (response) {
                                        //  alert(success);
                                          if(response === 'Success'){
                                           alert("product added to wishlist");
                                           document.getElementById(bookmarkId).classList.add("bookmark-checked");
                                          }
                                          else{
                                          alert("found error while adding product to wishlist");
                                          }

                                            },
                                          error: function (jqXHR, textStatus, errorThrown) {
                                              console.log("The following error occurred: " +jqXHR, textStatus, errorThrown);
                                           }
                                       });

                                       }
                });

                $('.wishlist_entry-remove').on("click", function (e){
                alert("inside removing entry from whishlist");
                            	e.preventDefault();
                            	var entryNumber = $(this).attr('id');
                            	var form = $('#removewishlistForm' + entryNumber[1]);

                            	var productCode = $(this).attr('data-productcode');
                            	var initialCartQuantity = form.find('input[name=initialQuantity]');
                            //	var cartQuantity = form.find('input[name=quantity]');

                            	ACC.track.trackRemoveFromCart(productCode, initialCartQuantity.val());
                            //	cartQuantity.val(0);
                            //	initialCartQuantity.val(0);
                            	$(".wishlist_entry-remove").attr("disabled", "disabled");
                            	form.submit();
                       });

	}
};
