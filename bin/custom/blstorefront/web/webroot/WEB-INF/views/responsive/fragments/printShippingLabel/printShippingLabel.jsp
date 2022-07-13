<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="store" tagdir="/WEB-INF/tags/responsive/store"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<link rel="stylesheet" type="text/css" media="all" href="${fn:escapeXml(themeResourcePath)}/css/blstyle.css"/>

<h2 style="text-align:center;font-size: 32px;margin-top: 50px;">Print Shipment Label</h2>
<c:forEach var="entry" items="${packageData}" varStatus="loop">
 <br />
	
	<%-- <c:url var="printingOutboundURL" value="${entry.outBoundShippingLabel }"/>
	<c:url var="printinginboundURL" value="${entry.inBoundShippingLabel }"/> --%>
	<table class="printShippingLabel">
			<tr>
			<th style="padding:10px;text-align:left">
						Package-${loop.index }
					</th>
			</tr>
				<tr>
					<td style="padding:10px">
						<a href="${entry.outBoundShippingLabel }" title="Download Outboud Label">Download Outboud Label</a>
					</td>
					<td style="padding:10px">
						<a href="${entry.inBoundShippingLabel }" title="Download Inbound Label">Download Inbound Label</a>
					</td>
				</tr>
			</table>
	<%-- <form id="printLabel-${loop.index}"
		action="${printingURL}"
		method="post">
		<input type="hidden" name="${CSRFToken.parameterName }"
			value="${CSRFToken.token }"> <input type="hidden"
			name="label" id="label" value="${entry.outBoundShippingLabel }">
			
			<table class="printShippingLabel">
			<tr>
			<th style="padding:10px;text-align:left">
						Package-${loop.index }
					</th>
			</tr>
				<tr>
					<td style="padding:10px">
					Outbound Label
						<a href="https://localhost:9002/blstorefront/shipment/printLabelValue?label=${entry.outBoundShippingLabel }">Outbound Label</a>
					</td>
					<td style="padding:10px">
						<input type="submit" name="Out Bound Label"	value="Print Outbound Label">
					</td>
				</tr>
			</table>
	</form>
	
	
	<form id="printLabel-${loop.index + 1}"
		action="${printingURL}"
		method="post">
		<input type="hidden" name="${CSRFToken.parameterName }"
			value="${CSRFToken.token }"> <input type="hidden"
			name="label" id="label" value="${entry.inBoundShippingLabel }">
			
			<table class="printShippingLabel">
				<tr>
					<td style="padding:10px">
					Inbound Label
			<a href="https://localhost:9002/blstorefront/printLabel/printLabelValue?label=${entry.inBoundShippingLabel }">Inbound Label</a>	
					</td>
					<td style="padding:10px">
						<input type="submit" name="Inbound Bound Label"	value="Print Inbound Label">
					</td>
				</tr>
			</table>

		
	</form> --%>

	
</c:forEach>