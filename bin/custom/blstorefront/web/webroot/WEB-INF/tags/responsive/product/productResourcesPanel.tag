<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:if test = "${not empty product.data_Sheet}">
<c:forEach var="resourceData" items="${product.data_Sheet}">
<div>
<a href="${resourceData.url}" target="_blank">
<img src="${commonResourcePath}/images/manuapdficon.png" />
</a>
</div>
</c:forEach>
</c:if>
