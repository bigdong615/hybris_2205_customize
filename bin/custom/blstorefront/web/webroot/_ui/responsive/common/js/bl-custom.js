

        $(document).on("click", "#applyCode-d", function(e) {
            var me = $(this);
            e.preventDefault();
            if ( me.prop('disabled') ) {
                return;
            }
            me.prop('disabled', true);
            var method = "POST";
            var code1;
            $('input[type="text"].inputCode-d').each(function() {
                if ($(this).val() != "") {
                    code1 = $(this).val();
                }
            });
            var code = code1;

            // Trigger coupon code used GTM event
           /* if (typeof trackCouponCodeUsedGoogleTagManagerEvent === "function") {
                trackCouponCodeUsedGoogleTagManagerEvent(code);
            }*/

            $.ajax({
                url: ACC.config.encodedContextPath + '/checkout/apply',
                data: {
                    code: code
                },
                type: method,
                success: function(data, status, xhr) {
                    var ct = xhr.getResponseHeader("content-type") || "";
                    if (ct.indexOf('html') > -1) {
                       var homepage = "/";
                       parent.window.location = homepage;
                    }else if (data == "") {
                         var currentURL = window.location.href;
                      // Trigger coupon code used GTM event
                      //   trackCouponCodeUsedGoogleTagManagerEvent(code);
                         parent.window.location = currentURL;

                    } else {
                        $('.tc-giftcard-coupon-alert').text(data);
                        $('.tc-giftcard-coupon-alert').removeAttr('hidden');
                        $('.promotion_message').attr('hidden','');
                    }
                },
                complete: function() {
                    me.prop('disabled', false);
                },
                error: function(error) {
                    console.log("voucher error");
                    me.prop('disabled', false);
                }
            });
        });


        $('.js-release-voucher-remove-btn').on("click", function(e) {
            e.preventDefault();
            var method = "POST";
            var voucherForm = {};
            var code = $(this).attr('id');

            voucherForm["voucherCode"] = $(this).attr('id');
            $.ajax({
                url: ACC.config.encodedContextPath + '/checkout/remove',
                data: {
                    code: code
                },
                async: false,
                type: method,
                success: function(data, status, xhr) {
                    var currentURL = window.location.href;
                    parent.window.location = currentURL;
                },
                error: function(error) {
                    console.log("voucher error");
                }
            });
        })