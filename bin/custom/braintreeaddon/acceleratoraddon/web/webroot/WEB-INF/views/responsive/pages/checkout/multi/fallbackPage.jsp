<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<%@ taglib prefix="sec" uri="http://www.springframework.org/security/tags" %>
<sec:csrfMetaTags />

<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1"/>

<title>Fallback</title>

<script type="text/javascript" src="https://code.jquery.com/jquery-1.7.1.min.js"></script>

<script type="text/javascript" src="https://js.braintreegateway.com/web/3.62.1/js/client.min.js"></script>
<script type="text/javascript" src="https://js.braintreegateway.com/web/3.62.1/js/local-payment.min.js"></script>

</head>
<body>

    <div>Please wait. We will redirect you to the storefront</div>

<script>
var HTML = {
    DIV: "<div>",
    BR: "<br/>",
    IMG: "<img/>",
    INPUT: "<input>",
    FORM: "<form>",
    TYPE: "type"
};

var CONST = {
    ATTR_NAME: "name",
    ATTR_ACTION: "action",
    ATTR_METHOD: "method",
    FORM_METHOD_POST: "POST"
};

 console.log('Check');

 console.log("${params}")


 fallback();

 console.log('Check2');

 function fallback() {
        braintree.client.create({
        authorization: "${client_token}"
        }).then(function (clientInstance) {
    return braintree.localPayment.create({
        client: clientInstance
    });
    }).then(function (localPaymentInstance) {

    if (localPaymentInstance.hasTokenizationParams()) {
        localPaymentInstance.tokenize().then(function (payload) {

        console.log('localPaymentInstance.tokenize');
        console.log(payload);


    // send payload.nonce to your server
    processResponce(payload);

    }).catch(function (tokenizeError) {
    // handle tokenization error
        console.log('tokenization error !!!!!!!!!!!!!');
        console.log(tokenizeError);
    });
    } else {
        console.log('localPaymentInstance else');
    // if this page should only be reached when
    // recovering from a mobile app switch,
    // display an error for not having the
    // correct params in the query string
    }
    });
}

function processResponce(responce) {
    var url = "${request.contextPath}" + '/braintree/checkout/lpm/processFallback';

    var form = createForm("fallback", url);

    var paymentNonce = createHiddenParameter("paymentNonce", responce.nonce);
    var paymentId = createHiddenParameter("paymentId", "${paymentId}");

        form.append($(paymentNonce));
        form.append($(paymentId));
        form.appendTo('body');
        form.submit()
}

function createForm(name, action) {
    var form = $(HTML.FORM).attr(CONST.ATTR_ACTION, action)
        .attr(CONST.ATTR_NAME, name).attr(CONST.ATTR_METHOD, CONST.FORM_METHOD_POST);
    return form;
    }

    function createHiddenParameter(name, value) {
    var input = $(HTML.INPUT).attr(HTML.TYPE, "hidden").attr(CONST.ATTR_NAME, name).val(
        value);
    return input;
}

</script>

</body>
</html>



