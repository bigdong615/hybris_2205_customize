ACC.blanalyticsevent = {
    _autoload: [
        "bindAnalyticEvent"
    ],
    bindAnalyticEvent: function ()
    {
    $(".js-pdplinkUrl").on("click", function ()
            {
                var productCode = $(this).attr("data-productCode");
                var productName =  $(this).attr("data-productName");
                var productType =  $(this).attr("data-productType");
                var brand       =  $(this).attr("data-brand");
                 window.mediator.publish('productClick_gtm',{
                            productCode: productCode,
                            productName: productName,
                            brand: brand,
                            productType: productType
                        });
            });
    }
    };