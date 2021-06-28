<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="entry" required="false" type="de.hybris.platform.commercefacades.order.data.OrderEntryData" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>


<spring:htmlEscape defaultHtmlEscape="true" />
         <div class="col-4 col-md-3"><product:productPrimaryImage product="${entry.product}" format="product"/></div>
