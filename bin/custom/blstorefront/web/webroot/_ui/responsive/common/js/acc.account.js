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

		      /**
            Added for BL-31 to make validation for registration
          **/
       $(document).on("click", ".js-signUp-popup-validation", function(e) {
           e.preventDefault();
           var formValues = $('#signUppopup-validation').serialize();
           var targetUrl = $(this).val();
           $.ajax({
               type: "POST",
               data: formValues,
               url: targetUrl,
               success: function(response) {
                   $('#signUp').html(response);
                   if ($('#errorMessages_id-signup').data("value") === undefined || $('#errorMessages_id-signup').data("value") === null) {
                       location.reload();
                   }
               },
               error: function(e) {
                   // do nothing
               }
           });

       });


       $(document).on("click", ".js-login-popup-validation", function(e) {
           e.preventDefault();
           var formValues = $('#login-popup-validation').serialize();
           var targetUrl = $(this).val();
           $.ajax({
               type: "POST",
               data: formValues,
               url: targetUrl,
               success: function(response) {
                   $('#signIn').html(response);
                   if ($('#errorMessages_id').data("value") === undefined || $('#errorMessages_id').data("value") === null) {
                       location.reload();
                   }
               },
               error: function(e) {
                   // do nothing
               }
           });


       });

	}
};