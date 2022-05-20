<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<a class="filter-expand" data-bs-toggle="collapse" href="#resources" role="button" aria-expanded="false" aria-controls="notes">
    <h3><div class="sizeAdj-5"><spring:theme code= "pdp.resources.section.text" /></div></h3></a>
     <div class="collapse" id="resources">
     <product:productResourcesPanel />
     </div>
   