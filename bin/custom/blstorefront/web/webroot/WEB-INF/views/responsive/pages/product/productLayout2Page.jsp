<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<template:page pageTitle="${pageTitle}">

<!-- BL-927: Added condition for Gift Card as per requirement -->
<c:choose>
      <c:when test="${product.productType eq 'GIFTCARD'}">
       <product:giftCardProductDetailsPanel />
      </c:when>
       <c:when test="${IsRentalPage eq 'true' && product.forRent eq 'true'}">
          <product:blRentalProductDetailsPanel />
      </c:when>
      <c:when test="${IsRentalPage eq 'false' && product.forSale eq 'true'}">
         <product:blUsedProductDetailsPanel/>
      </c:when>
       <c:when test="${product.retailGear eq true}">
          <product:newgearProductDetailsPanel/>
      </c:when>

<c:otherwise>
</c:otherwise>
</c:choose>

 
    <!-- Modals -->
       <div class="modal fade" id="sku52678" tabindex="-1" aria-hidden="true">
         <div class="modal-dialog modal-dialog-centered modal-lg">
           <div class="modal-content">
             <div class="modal-header">
               <h5 class="modal-title" id="sku52678-title"><spring:theme code="pdp.rating.popup.header.text"/></h5>
               <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
             </div>
             <div class="modal-body">
               <div class="row">
                   <div class="col-12 col-md-7">
                       <p class="body14 mb-5"></p>
                        </br>
                       <p><b><spring:theme code="pdp.rating.popup.cosmetic.rating.text"/><span class="float-end" id="cosmetic"></span></b></p>
                       <p class="body14 mb-5"></p>
                       <p class="body14 mb-5"></p>
                       <p><b><spring:theme code="pdp.rating.popup.functional.rating.text"/> <span class="float-end" id="functional"></span></b></p>
                   </div>
                   <div class="col-12 col-md-4 offset-md-1">
                       <div class="ratingCircle">
                           <div class="ratingContent">
                               <span class="mt-5"><spring:theme code="pdp.rating.popup.expert.rating.text"/></span>
                               <span class="rating-number mt-2" id="conditionRating"></span>
                           </div>
                       </div>
                       <p class="text-center mt-4" id="serialId"></p>
                   </div>
               </div>
             </div>
             <div class="modal-footer">
                 <p class="body14"><spring:theme code="pdp.rating.popup.footer.text1" /></p>
                 <p class="body14"><spring:theme code="pdp.rating.popup.footer.text2"/></p>
             </div>
           </div>
         </div>
       </div>
       
       <div class="modal fade" id="pickup-delivery-options" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-lg">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title" id="sku52678-title"><spring:theme code="text.pickup.delivery.modal.title"/></h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
          <div class="modal-body">
            <div class="row">
                <div class="col-12">
                    <p class="overline lightteal"><spring:theme code="text.pickup.delivery.modal.fast"/></p>
                    <h6><b><spring:theme code="text.pickup.delivery.modal.ship.it"/> </b></h6>
                    <p class="gray80"><spring:theme code="text.pickup.delivery.modal.line1"/></p>
                    <hr>
                    <p class="overline lightteal"><spring:theme code="text.pickup.delivery.modal.faster"/></p>
                    <h6><b><spring:theme code="text.pickup.delivery.modal.pickup"/></b></h6>
                    <p class="gray80"><spring:theme code="text.pickup.delivery.modal.line2"/></p>
                    <hr>
                    <p class="overline lightteal"><spring:theme code="text.pickup.delivery.modal.fastest"/></p>
                    <h6><b><spring:theme code="text.pickup.delivery.modal.same.day"/></b></h6>
                    <p class="gray80"><spring:theme code="text.pickup.delivery.modal.line3"/></p>
                </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    
</template:page>
