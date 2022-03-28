<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<a class="filter-expand" data-bs-toggle="collapse" href="#specs" role="button" aria-expanded="false" aria-controls="specs">
    <h3><div class="sizeAdj-5"><spring:theme code = "pdp.specification.section.text"/></div></h3></a>
           <div class="collapse" id="specs">
            <p>${ycommerce:sanitizeHTML(product.specifications)}</p>
           </div>
           <hr>