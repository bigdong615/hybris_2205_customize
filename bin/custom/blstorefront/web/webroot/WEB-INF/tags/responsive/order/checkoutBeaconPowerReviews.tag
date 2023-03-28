<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ attribute name="email" required="false" type="java.lang.String"%>

<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('powerreviews.merchant.groupid')"
					var="merchantGroupId" scope="page" />
<spring:eval expression="T(de.hybris.platform.util.Config).getParameter('powerreviews.merchant.id')"
					var="merchantID" scope="page" />

<script type="text/javascript" src="//static.powerreviews.com/t/v1/tracker.js"></script>

<c:choose>
 <c:when test = "${fn:contains(orderData.user.name, ' ')}">
	<c:set var = "firstName" value = "${fn:split(orderData.user.name, ' ')[0]}"/>
  <%--  <c:set var = "lastName" value = "${fn:split(orderData.user.lastName, ' ')[1]}"/> --%>
 </c:when>
<c:otherwise>
	<c:set var = "firstName" value = "${orderData.user.name}"/>
</c:otherwise>
</c:choose>


<script type="text/javascript">

   (function(){
    try{
    var tracker = POWERREVIEWS.tracker.createTracker({
        merchantGroupId: "${merchantGroupId}"});
        var orderFeed = {
            merchantGroupId: '${merchantGroupId}',
            merchantId: '${merchantID}',
            locale: 'en_US',
            merchantUserId: '',
            marketingOptIn: true,
            userEmail: '${fn:escapeXml(email)}',
            userFirstName: '${firstName}',
            userLastName: '${firstName}',
            orderId: '${orderData.code}',
            orderItems: [
           <c:forEach items="${orderData.entries}" var="entry" varStatus="loop"> 
		      {
		    	page_id: '${fn:substring(entry.product.code, 0, 41)}',
	    		product_name: '${ycommerce:encodeUrl(entry.product.name)}',
              	unit_price: ${entry.basePrice.value},
	    		quantity: ${entry.quantity} 
     		   },
              
            </c:forEach> 
            ]
        }
        tracker.trackCheckout(orderFeed);
 }catch(e)
 {
	 window.console && window.console.log(e)}
 }());
</script>


	