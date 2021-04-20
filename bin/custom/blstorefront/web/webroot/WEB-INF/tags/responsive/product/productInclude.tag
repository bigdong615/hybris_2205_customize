<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<a class="filter-expand" data-bs-toggle="collapse" href="#rental-includes" role="button" aria-expanded="false" aria-controls="includes">
   <h5><spring:theme code="pdp.product.includes.section.text"/></h5></a>
    <div class="collapse" id="rental-includes">
     <p>${ycommerce:sanitizeHTML(product.usedIncludes)}</p>
      </div>
    <hr>