<%@ attribute name="sdkVersion" required="false" type="java.lang.String" %>
<%@ attribute name="enableGooglePay" required="false" type="java.lang.Boolean" %>
<%@ attribute name="enableApplePay" required="false" type="java.lang.Boolean" %>
<%@ attribute name="enablePayPal" required="false" type="java.lang.Boolean" %>
<%@ attribute name="enableLocalPayment" required="false" type="java.lang.Boolean" %>
<%@ attribute name="enableVenmo" required="false" type="java.lang.Boolean" %>
<%@ attribute name="enableSecure3d" required="false" type="java.lang.Boolean" %>
<%@ attribute name="enableHostedFields" required="false" type="java.lang.Boolean" %>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/client.min.js"></script>
<script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/data-collector.min.js"></script>

<c:if test="${enablePayPal}">
    <script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/paypal.min.js"></script>
    <script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/paypal-checkout.min.js"></script>
</c:if>
<c:if test="${enableHostedFields}">
    <script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/hosted-fields.min.js"></script>
</c:if>
<c:if test="${enableApplePay}">
    <script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/apple-pay.min.js"></script>
</c:if>
<c:if test="${enableGooglePay}">
    <script type="text/javascript" src="https://pay.google.com/gp/p/js/pay.js"></script>
    <script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/google-payment.min.js"></script>
</c:if>
<c:if test="${enableLocalPayment}">
    <script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/local-payment.min.js"></script>
</c:if>
<c:if test="${enableVenmo}">
    <script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/venmo.min.js"></script>
</c:if>
<c:if test="${enableSecure3d}">
    <script type="text/javascript" src="https://js.braintreegateway.com/web/${sdkVersion}/js/three-d-secure.min.js"></script>
</c:if>