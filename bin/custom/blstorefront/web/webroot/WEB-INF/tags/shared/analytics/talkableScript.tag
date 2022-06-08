<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<!-- Begin Talkable integration code -->
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('talkable.account.id')"
					var="accountId" scope="page" />
<script>
  window._talkableq = window._talkableq || [];
  window._talkableq.unshift(['init', { site_id: '${accountId}' }]);
  window._talkableq.push(['authenticate_customer', {
    email: '${user.uid}',
    first_name: '${user.firstName}',
    last_name: '${user.lastName}',
    traffic_source: '' // Optional, the source of the traffic driven to the campaign. Example: 'facebook'
  }]);
  window._talkableq.push(['register_affiliate', {}]);
</script>

<c:if test = "${pageType == 'ORDERCONFIRMATION'}">
<c:set var="address" value="${orderData.deliveryAddress}"/>
<div id="talkable-offer"></div>
<script>
  window._talkableq = window._talkableq || [];
  var _talkable_data = {
    purchase: {
      order_number: '${orderData.code}',
      subtotal: '${orderData.subTotal.value}',
     	<c:if test="${not empty orderData.appliedVouchers}">
     		coupon_code : '${ycommerce:encodeJavaScript(orderData.appliedVouchers[0])}',
     	</c:if>
      shipping_zip: '${address.postalCode}',
      shipping_address: '${address.line1}, ${address.line2}, ${address.town}, ${address.region.name}, ${address.postalCode}, ${address.country.name}'
    },
    customer: {
      email: '${user.uid}',
      traffic_source: '' // The source of the traffic driven to the campaign. Example: 'facebook'
    }
  };
  window._talkableq.push(['register_purchase', _talkable_data]);
</script>
</c:if>
<!-- End Talkable integration code -->