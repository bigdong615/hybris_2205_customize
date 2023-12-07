<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="footer" tagdir="/WEB-INF/tags/responsive/common/footer"  %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>



<c:if test="${component.visible}">
	<c:forEach items="${component.navigationNode.children}"
		var="childLevel1">
		<c:forEach items="${childLevel1.children}"
			step="${component.wrapAfter}" varStatus="i">
			<div class="col-6">
				<c:if test="${component.wrapAfter > i.index}">
					<p>
						<b>${fn:escapeXml(childLevel1.title)}</b>
					</p>
				</c:if>
				<ul>
					<c:forEach items="${childLevel1.children}" var="childLevel2"
						begin="${i.index}" end="${i.index + component.wrapAfter - 1}">
						<c:forEach items="${childLevel2.entries}" var="childlink">
						<c:url value="${childlink.item.url}" var="link"/>
							<li><a href="${link }">${fn:escapeXml(childLevel2.title)}</a></li>
						</c:forEach>
					</c:forEach>
				</ul>
			</div>
		</c:forEach>
	</c:forEach>

</c:if>