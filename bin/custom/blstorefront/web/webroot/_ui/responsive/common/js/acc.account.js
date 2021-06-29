ACC.account = {

	_autoload: [
		"accountLink"
	],

	accountLink: function () {
		/* This function is responsible for providing form object for the login popup*/
		$(document).on("click", ".js-login-popup", function (e) {
			e.preventDefault();
			var serialClick = $(this).data('click');
			$('#signIn').html("");
			$.ajax({
				url: $(this).data("link"),
				success: function (result) {
					$('#signIn').html(result);
					$('#serialClick').val(serialClick);
					$('#serialSignUp').attr("data-serial",serialClick);
					setTimeout(function(){$("#signIn").modal('show');},500);
				}
			})
		});

		/*This function is responsible for providing form object for the sign up popup*/
		$(document).on("click", ".js-signUp-popup", function (e) {
			e.preventDefault();
			var serialClick = $(this).data('serial');
			$('#signUp').html("");
			$.ajax({
				url: $(this).data("link"),
				success: function (result) {
					$('#signUp').html(result);
					$('#serialClickSignUP').val(serialClick);
					$('#serialSignInInstead').attr("data-click",serialClick);
					setTimeout(function(){$("#signUp").modal('show');},500)
				}
			})
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
								// BL-689: below line added
								$("#errorMessages_sigin_email").removeClass("d-none");
							}
							if (splitValue.includes('register.pwd.invalid')) {
								if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
									$("#errorMessages_sigin_errorbox").removeClass("d-none");
								}
								$("#errorMessages_sigin_pwd").html("Your password needs to be at least 6 characters long");
								// BL-689: below line added
								$("#errorMessages_sigin_pwd").removeClass("d-none");
							}
							if (splitValue.includes('validation.checkPwd.equals')) {
								if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
									$("#errorMessages_sigin_errorbox").removeClass("d-none");
								}
								$("#errorMessages_sigin_chkPwd").html("Your passwords did not match, please enter them again");
								// BL-689: below line added
								$("#errorMessages_sigin_chkPwd").removeClass("d-none");
							}
						} else if (response === 'registration.error.account.exists.title') {
							if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
								$("#errorMessages_sigin_errorbox").removeClass("d-none");
							}
							// BL-689: below line added
							$("#errorMessages_sigin_email").removeClass("d-none");
							$("#errorMessages_sigin_email").html("Whoops, it looks like you’re already signed up with a BorrowLenses account");
						} else {
							
							$("#errorMessages_sigin_errorbox").addClass("d-none");
							//location.reload();
							
							var serialId = $('#signUppopup-validation').find('input[name="serialClickSignUP"]').val();
							if(serialId == "" || serialId  == undefined)
							{
								location.reload();
							}else{
								$('.' + serialId).click();
							}	
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
				$("#errorMessages_sigin_email").removeClass("d-none");
			} else {
				$("#errorMessages_sigin_email").html("");
				// BL-689: below line added
				$("#errorMessages_sigin_email").addClass("d-none");
			}
			if ($('#password').val() === '') {
				if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
					$("#errorMessages_sigin_errorbox").removeClass("d-none");
				}
				$("#errorMessages_sigin_pwd").html("Your password needs to be at least 6 characters long");
				$("#errorMessages_sigin_pwd").removeClass("d-none");
			} else {
				$("#errorMessages_sigin_pwd").html("");
				// BL-689: below line added
				$("#errorMessages_sigin_pwd").addClass("d-none");
			}
			if ($('#checkPwd-form-id').val() === '') {
				if($("#errorMessages_sigin_errorbox").hasClass("d-none")){
					$("#errorMessages_sigin_errorbox").removeClass("d-none");
				}
				$("#errorMessages_sigin_chkPwd").html("Be sure to provide your password confirmation");
				$("#errorMessages_sigin_chkPwd").removeClass("d-none");
			} else {
				$("#errorMessages_sigin_chkPwd").html("");
				// BL-689: below line added
				$("#errorMessages_sigin_chkPwd").addClass("d-none");
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
							$("#errorMessages_login").html("Your Email or Password was incorrect");
						} else {
							var serialId = $('#login-popup-validation').find('input[name="serialClick"]').val();
							if(serialId == "" || serialId  == undefined)
							{
								location.reload();
							}else{
								$('.' + serialId).click();
							}	
							
							
						}
					},
					error: function (e) {
						// do nothing
					}
				});
			} else {
				$("#errorMessages_login").removeClass("d-none");
				$("#errorMessages_login").html("Your Email or Password was incorrect");
			}
		});

	}
};
