ACC.account = {

	_autoload: [
		"accountLink"
	],

	accountLink: function(){

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