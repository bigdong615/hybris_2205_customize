<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

  <a class="filter-expand" data-bs-toggle="collapse" href="#rental-includes" role="button" aria-expanded="false" aria-controls="includes">
    <h3><div class="sizeAdj-5"><spring:theme code="pdp.rental.includes.section.text"/></div></h3>
  </a>
  <div class="collapse" id="rental-includes">
     <p>${ycommerce:sanitizeHTML(product.rentalIncludes)}</p>
  </div>
  <hr>

