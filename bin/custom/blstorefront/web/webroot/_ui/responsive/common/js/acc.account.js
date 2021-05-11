ACC.account = {

	_autoload: [
		"accountLink"
	],

	accountLink: function(){
  /* This function is responsible for providing form object for the login popup*/
  $(document).on("click",".js-login-popup",function(e){
	e.preventDefault();
	$('#signIn').html("");
  $.ajax({
  url : $(this).data("link"),
       success: function(result){
       $('#signIn').html(result);
       }
  })
	});

  /*This function is responsible for providing form object for the sign up popup*/
	$(document).on("click",".js-signUp-popup",function(e){
  	e.preventDefault();
  	$('#signUp').html("");
    $.ajax({
    url : $(this).data("link"),
         success: function(result){
         $('#signUp').html(result);
         }
    })
  	});
	}
	};