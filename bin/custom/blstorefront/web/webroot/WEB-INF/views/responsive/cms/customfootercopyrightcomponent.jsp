<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/fmt" prefix="fmt" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<jsp:useBean id="date" class="java.util.Date" />
<c:if test="${feature.uid  eq 'CopyRightParagraphComponent1'}">
<div class="col-12 col-xl-10">
	<div class="row">
		<div class="col-12 col-md-6">
		<p class="body14 text-start">&#169;<fmt:formatDate value="${date}" pattern="yyyy" />&nbsp${feature.footerCopyRight}</p>
		</div>
		<div class="col-12 col-md-6">
			${feature.footerLinks}
		</div>
	</div>
</div>
</c:if>