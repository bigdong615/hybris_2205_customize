<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ tag import="com.tealium.dataconnector.hybris.HybrisDataConverter" %>

<script data-cfasync="false" data-ui="off" src="https://cdn.transcend.io/cm/def7ceaa-4629-4804-82dc-2fe3ad26e438/airgap.js"></script>

<%
  String syncTagString = HybrisDataConverter.getSyncTag();
  String output = "";
  if (syncTagString != null){
    output = syncTagString;
  }
%>
  
<%=output%>