ACC.blanalyticsevent = {
	_autoload: [
		"bindAnalyticEvent"
	],
	bindAnalyticEvent: function () {
		$(".js-pdplinkUrl").on("click", function () {
			var productCode = $(this).attr("data-productCode");
			var productName = $(this).attr("data-productName");
			var productType = $(this).attr("data-productType");
			var brand = $(this).attr("data-brand");
			window.mediator.publish('productClick_gtm', {
				productCode: productCode,
				productName: productName,
				brand: brand,
				productType: productType
			});
		});

		$(".js-search-track").on("click", function () {
			var searchText = $('#js-site-search-input').val();
			if (searchText === "" || searchText == undefined) {
				searchText = $('#js-site-search-input-mob').val();
			}
			utag.link({
          "tealium_event"    : "search",
          "search_bar"       : '"'+searchText+'"'
      });
			window.mediator.publish('trackSearch', {
				searchText: searchText
			});
		});
	}
};

function trackDateSelection(date1, date2) {
	var diffTime = Math.abs(date1.dateInstance - date2.dateInstance); //difference time in millisecond
	var diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));
	var lengthOfRental = diffDays + 1;
	var currentDate = new Date();
	diffTime = Math.abs(currentDate - date1.dateInstance);
	diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24))
	var daysInAdvance = diffDays + 1;
	window.mediator.publish('searchRentalDate', {
		daysInAdvance: daysInAdvance,
		lengthOfRental: lengthOfRental
	});
}