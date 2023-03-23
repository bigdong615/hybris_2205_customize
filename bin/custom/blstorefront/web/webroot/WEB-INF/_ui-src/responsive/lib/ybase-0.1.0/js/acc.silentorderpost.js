ACC.silentorderpost = {

	spinner: $("<img>").attr("src", ACC.config.commonResourcePath + "/images/spinner.gif").addClass("spiner-select"),

	bindUseDeliveryAddress: function ()
	{
		$('#ccUseDeliveryAddress').on('change', function ()
		{
			if ($('#ccUseDeliveryAddress').is(":checked"))
			{
				var options = {'countryIsoCode': $('#useDeliveryAddressData').data('countryisocode'), 'useDeliveryAddress': true};
				ACC.silentorderpost.enableAddressForm();
				ACC.silentorderpost.displayCreditCardAddressForm(options, ACC.silentorderpost.useDeliveryAddressSelected);
				$("#save-address").prop('checked', true);
				$('.hideUseShipping').hide();
				ACC.silentorderpost.enableReadOnlyAddressForm();
			}
			else
			{
				ACC.silentorderpost.clearAddressForm();
				ACC.silentorderpost.enableAddressForm();
				$("#save-address").prop('checked', true);
				$('.hideUseShipping').hide();
				ACC.silentorderpost.disableReadOnlyAddressForm();
			}
		});

		if ($('#ccUseDeliveryAddress').is(":checked"))
		{
			var options = {'countryIsoCode': $('#useDeliveryAddressData').data('countryisocode'), 'useDeliveryAddress': true};
			ACC.silentorderpost.enableAddressForm();
			ACC.silentorderpost.displayCreditCardAddressForm(options, ACC.silentorderpost.useDeliveryAddressSelected);
			$("#save-address").prop('checked', true);
			$('.hideUseShipping').hide();
			ACC.silentorderpost.disableAddressForm();
			
		}
		else
		{
			var options = {'countryIsoCode': $('#useDeliveryAddressData').data('countryisocode'), 'useDeliveryAddress': false};
			ACC.silentorderpost.displayCreditCardAddressForm(options, ACC.silentorderpost.useDeliveryAddressSelected);
			ACC.silentorderpost.enableAddressForm();
			$("#save-address").prop('checked', true);
			$('.hideUseShipping').hide();
			ACC.silentorderpost.disableReadOnlyAddressForm();
		}
	},

	bindSubmitSilentOrderPostForm: function ()
	{
		$('.submit_silentOrderPostForm').click(function ()
		{
			ACC.common.blockFormAndShowProcessingMessage($(this));
			$('.billingAddressForm').filter(":hidden").remove();
			ACC.silentorderpost.enableAddressForm();
			$('#silentOrderPostForm').submit();
		});
	},

	bindCycleFocusEvent: function ()
	{
		$('#lastInTheForm').blur(function ()
		{
			$('#silentOrderPostForm [tabindex$="10"]').focus();
		})
	},

	isEmpty: function (obj)
	{
		if (typeof obj == 'undefined' || obj === null || obj === '') return true;
		return false;
	},

	disableAddressForm: function ()
	{
		$('input[id^="address\\."]').prop('disabled', true);
		$('select[id^="address\\."]').prop('disabled', true);
	},
	
	enableReadOnlyAddressForm: function ()
	{
		$('input[id^="address\\."]').prop('readOnly', true);
		$('select[id^="address\\."]').prop('disabled', true);
	},
	
	disableReadOnlyAddressForm: function ()
	{
		$('input[id^="address\\."]').prop('readOnly', false);
		$('select[id^="address\\."]').prop('disabled', false);
	},

	enableAddressForm: function ()
	{
		$('input[id^="address\\."]').prop('disabled', false);
		$('select[id^="address\\."]').prop('disabled', false);
	},

	clearAddressForm: function ()
	{
		$('input[id^="address\\."]').val("");
		$('select[id^="address\\."]').val("");
	},

	useDeliveryAddressSelected: function ()
	{
		if ($('#ccUseDeliveryAddress').is(":checked"))
		{
			var countryIsoCode = $('#address\\.country').val($('#useDeliveryAddressData').data('countryisocode')).val();
			if(ACC.silentorderpost.isEmpty(countryIsoCode))
			{
				$('#ccUseDeliveryAddress').click();
				$('#ccUuseDeliveryAddress').parent().hide();
			}
			else
			{
				$("#save-address").prop('checked', true);
				$('.hideUseShipping').hide();
				ACC.silentorderpost.enableReadOnlyAddressForm();
			}
		}
		else
		{
			ACC.silentorderpost.clearAddressForm();
			ACC.silentorderpost.enableAddressForm();
			$("#save-address").prop('checked', true);
			$('.hideUseShipping').hide();
			ACC.silentorderpost.disableReadOnlyAddressForm();
		}
	},
	
	

	bindCreditCardAddressForm: function ()
	{
		$('#billingCountrySelector :input').on("change", function ()
		{
			var countrySelection = $(this).val();
			var options = {
				'countryIsoCode': countrySelection,
				'useDeliveryAddress': false
			};
			ACC.silentorderpost.displayCreditCardAddressForm(options);
		})
	},

	displayCreditCardAddressForm: function (options, callback)
	{
		$.ajax({ 
			url: ACC.config.encodedContextPath + '/checkout/multi/sop/billingaddressform',
			async: true,
			data: options,
			dataType: "html",
			beforeSend: function ()
			{
				$('#billingAddressForm').html(ACC.silentorderpost.spinner);
			}
		}).done(function (data)
				{
					$("#billingAddressForm").html(data);
					if (typeof callback == 'function')
					{
						callback.call();
					}
				});
	}
}

$(document).ready(function ()
{
	ACC.silentorderpost.bindUseDeliveryAddress();
	ACC.silentorderpost.bindSubmitSilentOrderPostForm();
	ACC.silentorderpost.bindCreditCardAddressForm();
	$('.hideUseShipping').hide();
	// check the checkbox
	$("#useDeliveryAddress").click();
});
