<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>


<spring:htmlEscape defaultHtmlEscape="true" />


<c:url value="/" var="homePageUrl" />
<c:url value="/my-account/order/${extendOrderData.code}" var="viewOrderAction" />
<div id="accountContent" class="col-lg-5 offset-lg-1">
                    <h1> <spring:theme code="text.extend.order" /></h1>
                    <hr>
                    <h5 class="mb-5"><spring:theme code="text.extend.order.new.returndate" />${extendOrderData.extendOrderConfirmationDate}</h5>
              <p>
             <c:if test="${fn:containsIgnoreCase(extendOrderPaymentMethod, 'creditCard')}">
                     <spring:theme code="text.extend.order.text"/>
             </c:if>
             <c:if test="${fn:containsIgnoreCase(extendOrderPaymentMethod, 'poNumber')}">
                     <spring:theme code="text.extend.order.po"/>
             </c:if>
             <c:if test="${fn:containsIgnoreCase(extendOrderPaymentMethod, 'payPal')}">
                 <spring:theme code="text.extend.order.paypal"/>
             </c:if>
             <format:blPrice priceData="${extendOrderData.orderTotalWithTaxForExtendRental}"/>&nbsp;<spring:theme code="text.extend.order.additional"/>&nbsp;${extendOrderData.addedTimeForExtendRental}
                    <spring:theme code="text.extend.order.confirmation"/><b>${extendOrderData.customerMail}</b></p>
                    <div class="confirmation-actions my-5">
                        <a href="${viewOrderAction}" class="btn btn-primary mx-3 mb-4 mb-sm-0"><spring:theme code="text.extend.order.view.order"/></a>
                        <a href="${homePageUrl}" class="btn btn-outline mx-3 mb-4 mb-sm-0"><spring:theme code="text.extend.order.start"/></a>
                    </div>

</div>