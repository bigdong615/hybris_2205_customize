<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<template:page pageTitle="${pageTitle}">
   <c:choose>
       <c:when test="${IsRentalPage eq 'true' && product.forRent eq 'true'}">
              <product:blRentalProductDetailsPanel />
      </c:when>
      <c:when test="${IsRentalPage eq 'false' && product.forSale eq 'true'}">
         <product:blUsedProductDetailsPanel/>
      </c:when>
   </c:choose>

    <!-- Modals -->
       <div class="modal fade" id="sku52678" tabindex="-1" aria-hidden="true">
         <div class="modal-dialog modal-dialog-centered modal-lg">
           <div class="modal-content">
             <div class="modal-header">
               <h5 class="modal-title" id="sku52678-title">Used Gear Rating</h5>
               <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
             </div>
             <div class="modal-body">
               <div class="row">
                   <div class="col-12 col-md-7">
                       <p><b>Cosmetic <span class="float-end">8.0</span></b></p>
                       <p class="body14 mb-5">Some wear and tear: Noticeable wear from heavy impacts, scuffs and the paint job is diminishing, but fully functional</p>
                       <p><b>Sensor <span class="float-end">9.5</span></b></p>
                       <p class="body14 mb-5">Very light internal dust, but images are clean</p>
                   </div>
                   <div class="col-12 col-md-4 offset-md-1">
                       <div class="ratingCircle">
                           <div class="ratingContent">
                               <span class="mt-5">Expert Rating</span>
                               <span class="rating-number mt-2">9.2</span>
                           </div>
                       </div>
                       <p class="text-center mt-4"><b>Serial</b> #52678</p>
                   </div>

               </div>
             </div>
             <div class="modal-footer">
                 <p class="body14">Our used gear is always fully functional and each item is individually inspected, cleaned, and rated by our photo & video experts before being listed for sale.</p>
                 <p class="body14">For additional details, head to our used gear FAQ.</p>
             </div>
           </div>
         </div>
       </div>
</template:page>