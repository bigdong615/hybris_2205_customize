<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

 <a class="filter-expand" data-bs-toggle="collapse" href="#rental-notes" role="button" aria-expanded="false" aria-controls="notes">
 <h5><spring:theme code= "pdp.rental.notes.section.text" /></h5></a>
 <div class="collapse" id="rental-notes">
 <p>${ycommerce:sanitizeHTML(product.rentalNote)}</p>
 </div>
  <hr>
