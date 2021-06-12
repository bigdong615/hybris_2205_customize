<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<script>
var errorMessageMap = {
		"hosted_fields_fields_empty":"<spring:theme code="braintree.error.hosted_fields_fields_empty"/>",
		"hosted_fields_fields_invalid":"<spring:theme code="braintree.error.hosted_fields_fields_invalid"/>",
		"hosted_fields_failed_tokenization":"<spring:theme code="braintree.error.hosted_fields_failed_tokenization "/>",
		"hosted_fields_tokenization_network_error":"<spring:theme code="braintree.error.hosted_fields_tokenization_network_error"/>",
		"client_request_timeout": "<spring:theme code="braintree.error.client_request_timeout"/>",
		"client_gateway_network":"<spring:theme code="braintree.error.client_gateway_network"/>",
		"client_request_error":"<spring:theme code="braintree.error.client_request_error"/>",
		"client_missing_gateway_configuration":"<spring:theme code="braintree.error.client_missing_gateway_configuration"/>",
		"paypal_account_tokenization_failed":"<spring:theme code="braintree.error.paypal_account_tokenization_failed"/>",
		"paypal_invalid_payment_option":"<spring:theme code="braintree.error.paypal_invalid_payment_option"/>",
		"paypal_flow_failed":"<spring:theme code="braintree.error.paypal_flow_failed"/>",
		"paypal_browser_not_supported":"<spring:theme code="braintree.error.paypal_browser_not_supported"/>"

}
function getErrorMessage(errorCode) {
    return errorMessageMap[errorCode];
}
</script>

