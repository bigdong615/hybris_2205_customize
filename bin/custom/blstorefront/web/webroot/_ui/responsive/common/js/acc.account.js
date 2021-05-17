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
				}
			})
		});

		/** Added for BL-31 to make validation for registration **/

		$(document).on("click", ".js-signUp-popup-validation", function (e) {
			e.preventDefault();
			var formValues = $('#signUppopup-validation').serialize();
			var targetUrl = $(this).val();
			if ($('#register-form-id').val() !== '' && $('#password').val() !== '' && $('#checkPwd-form-id').val() !== '') {
				$.ajax({
					type: "POST",
					data: formValues,
					url: targetUrl,
					success: function (response) {
						var splitValue = '';
						if (response.startsWith("Error:")) {
							splitValue = response.split(":");
							if (splitValue.includes('register.email.invalid')) {
								$("#errorMessages_sigin_email").html("Please enter a valid email");
							}
							if (splitValue.includes('register.pwd.invalid')) {
								$("#errorMessages_sigin_pwd").html("Your password needs to be at least 6 characters long");
							}
							if (splitValue.includes('validation.checkPwd.equals')) {
								$("#errorMessages_sigin_chkPwd").html("Your passwords did not match, please enter them again");
							}
						} else if (response === 'registration.error.account.exists.title') {
							$("#errorMessages_sigin_email").html("Whoops, it looks like you’re already signed up with a BorrowLenses account");
						} else {
							location.reload();
						}
					},
					error: function (e) {
						// do nothing
					}
				});
			}
			if ($('#register-form-id').val() === '') {
				$("#errorMessages_sigin_email").html("Looks like you forgot to enter your email address");
			} else {
				$("#errorMessages_sigin_email").html("");
			}
			if ($('#password').val() === '') {
				$("#errorMessages_sigin_pwd").html("Your password needs to be at least 6 characters long");
			} else {
				$("#errorMessages_sigin_pwd").html("");
			}
			if ($('#checkPwd-form-id').val() === '') {
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
						if (response === 'login.error.account.not.found.title') {
							$("#errorMessages_login").html("Email Address and Password are required");
						} else {
							location.reload();
						}
					},
					error: function (e) {
						// do nothing
					}
				});
			} else {
				$("#errorMessages_login").html("Email Address and Password are required");
			}

		});

	}
};