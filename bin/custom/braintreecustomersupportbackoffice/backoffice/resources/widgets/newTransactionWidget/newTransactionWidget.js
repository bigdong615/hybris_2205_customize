NewTransactionWidgetController = function($scope) {
	$scope.init = function() {
		clientToken = this.model.$_incomingMsg.split("|")[0];
		siteUID = this.model.$_incomingMsg.split("|")[1];
		currentCurrency = this.model.$_incomingMsg.split("|")[2];
		console.log("incoming: " + this.model.$_incomingMsg);
		console.log("clientToken: " + clientToken);
		console.log("siteUID: " + siteUID);
		console.log("currentCurrency: " + currentCurrency);

		if (typeof braintree == 'undefined') {
			console.log("BT libraries not loaded, undefined");
			return;
		}
		braintree.client.create({
			authorization: clientToken
		}, function (clientErr, clientInstance) {
			if (clientErr) {
				console.log("Error, clientErr: " + clientErr);
				$("#validPaymentMethodError").hide();
				$("#validPaymentMethodError").show();
				return;
			}

			braintree.hostedFields.create({
						client: clientInstance,
						styles: {
							// Styling element state
							":focus": {
								"color": "blue"
							},
							".valid": {
								"color": "green"
							},
							".invalid": {
								"color": "red"
							}
						},
						fields: {
							number : {
								selector : "#number",
								placeholder : "Card Number"
							},
							expirationDate : {
								selector : "#expiration-date",
								placeholder : "MM/YY"
							},
							cvv : {
								selector : "#cvv",
								placeholder : "CVV"
							}
						}
					},
				function (hostedFieldsErr, hostedFieldsInstance) {
						if (hostedFieldsErr) {
							$("#validPaymentMethodSuccess").hide();
							$("#validPaymentMethodError").show();
							return;
						}

						$("#submit").unbind("click");
						$("#submit").attr("type", "button");
						$("#submit").click(function () {
							hostedFieldsInstance.tokenize(function (tokenizeErr, response) {
								if (tokenizeErr) {
									$("#validPaymentMethodSuccess").hide();
									$("#validPaymentMethodError").show();
								} else {
									$("#validPaymentMethodSuccess").hide();
									$("#validPaymentMethodError").hide();

									var cardholder = $("#cardholder").val();
									var nonce = response.nonce;

									$scope.adapter.cng().socketEvent("newTransactionOutput", cardholder + "|" + nonce + "|" + siteUID  + "|" + currentCurrency);
								}
							});
						});
					});
		});

	};
};

AngularCNG.init("newTransactionWidget", NewTransactionWidgetController);
