ACC.minicart = {

	updateMiniCartDisplay: function(){
		var miniCartRefreshUrl = $(".js-mini-cart-link").data("miniCartRefreshUrl");

		$.ajax({
			url: miniCartRefreshUrl,
			cache: false,
			type: 'GET',
			dataType: 'json',
			success: function(jsonData){
				var $numberItem = $("<span>").addClass("nav-items-total").text(jsonData.miniCartCount);
				$(".js-mini-cart-link .js-mini-cart-count").empty();
				$(".js-mini-cart-link .js-mini-cart-count").append($numberItem);
				$(".js-mini-cart-link .js-mini-cart-price").text(jsonData.miniCartPrice);
			}
		});
	}
};
