ACC.track = {
	trackAddToCart: function (productCode, productName,productBrand,productType,productCategory,quantity)
	{
		window.mediator.publish('trackAddToCart',{
			productCode: productCode,
			productName : productName,
			quantity: quantity,
			productBrand: productBrand,
			productType : productType,
			productCategory : productCategory
		});
	},
	trackRemoveFromCart: function(productCode, productName, initialCartQuantity)
	{
		window.mediator.publish('trackRemoveFromCart',{
			productCode: productCode,
			productName: productName,
			initialCartQuantity: initialCartQuantity
		});
	},

	trackUpdateCart: function(productCode, initialCartQuantity, newCartQuantity)
	{
		window.mediator.publish('trackUpdateCart',{
			productCode: productCode,
			initialCartQuantity: initialCartQuantity,
			newCartQuantity: newCartQuantity
		});
	},

    trackShowReviewClick: function(productCode)
    {
        window.mediator.publish('trackShowReviewClick',{});
    }

};