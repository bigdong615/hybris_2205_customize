ACC.track = {
	trackAddToCart: function (productCode, quantity, cartData,productBrand,productType,category)
	{
		window.mediator.publish('trackAddToCart',{
			productCode: productCode,
			quantity: quantity,
			cartData: cartData,
			productBrand: productBrand,
			productType : productType,
			category : category
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