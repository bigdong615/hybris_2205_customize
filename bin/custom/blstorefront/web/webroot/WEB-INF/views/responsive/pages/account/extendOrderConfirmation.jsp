<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>


<spring:htmlEscape defaultHtmlEscape="true" />


<c:url value="/" var="homePageUrl" />
<div id="accountContent" class="col-lg-5 offset-lg-1">
                    <h1> <spring:theme code="text.extend.order" /></h1>
                    <hr>
                    <h5 class="mb-5"><spring:theme code="text.extend.order.new.returndate" />Feb 2, 2021</h5>
             <p><spring:theme code="text.extend.order.text"/> <format:blPrice priceData="${extendOrderData.orderTotalWithTaxForExtendRental}"/>&nbsp;<spring:theme code="text.extend.order.additional"/>${extendOrderData.addedTimeForExtendRental}
                    <spring:theme code="text.extend.order.confirmation"/><b>johndoe@gmail.com</b></p>
                    <div class="confirmation-actions my-5">
                        <a href="#" class="btn btn-primary mx-3 mb-4 mb-sm-0"><spring:theme code="text.extend.order.view.order"/></a>
                        <a href="${homePageUrl}" class="btn btn-outline mx-3 mb-4 mb-sm-0"><spring:theme code="text.extend.order.start"/></a>
                    </div>

</div>