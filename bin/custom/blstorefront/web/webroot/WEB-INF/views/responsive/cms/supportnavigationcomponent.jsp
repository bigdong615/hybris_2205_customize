<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>

<a class="nav-link dropdown-toggle" href="#" id="supportdropdown" data-bs-toggle="dropdown" aria-expanded="false">${component.name }</a>
<div class="dropdown-menu megamenu" aria-labelledby="supportdropdown">
	<div class="container">
		<div class="row">
			<div class=" col-md-10 offset-md-1 submenu">
				<h5>
					<i class="icon-support"></i> ${component.title }
				</h5>
				<div class="row">
					<c:forEach items="${component.navigationNode.entries}"	var="entry">
						<div class="col-md-3">						
							<h6>${entry.item.title }</h6>
							<p class="body14">${entry.item.content }</p>
						</div>
					</c:forEach>
				</div>
			</div>
		</div>
	</div>
</div>
</a>