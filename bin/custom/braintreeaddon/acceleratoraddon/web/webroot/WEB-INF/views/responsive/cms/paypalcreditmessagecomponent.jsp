<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<c:if test="${isCreditMessagesEnabled}">

    <script>

        var isCreditMessagesEnabled = "${isCreditMessagesEnabled}";
        var creditEnabled = ${isCreditEnabled};
        var paypalIntent = "${checkoutData.intent}";
        var storeInVault = "${checkoutData.storeInVault}";
        var braintreeLocale = "${braintreeLocale}";
        var currency = "${checkoutData.currency}";
        var clientToken = "${client_token}";
        var userAction = "${userAction}";

    </script>

    <div id="paypal-credit-message"
         data-pp-message data-pp-placement="${component.placement.code}"
         data-pp-amount="${checkoutData.amount}"
         data-pp-style-layout="${component.layout.code}"
         data-pp-style-color="${component.color}"
         data-pp-style-ratio="${component.ratio}"
         data-pp-style-logo-type="${component.logoType.code}"
         data-pp-style-logo-position="${component.logoPosition.code}"
         data-pp-style-text-color="${component.textColor.code}"
    ></div>

    <script>
        var target = document.getElementsByClassName("mini-cart-price")[0];
        var observer = new MutationObserver(function (mutations) {
            var totalPrice = target.innerHTML.trim().substr(1).replace(/,/g, '');
            document.getElementById("paypal-credit-message").setAttribute("data-pp-amount", totalPrice);
        });
        var config = {attributes: true, childList: true, characterData: true};
        observer.observe(target, config);
    </script>
</c:if>
