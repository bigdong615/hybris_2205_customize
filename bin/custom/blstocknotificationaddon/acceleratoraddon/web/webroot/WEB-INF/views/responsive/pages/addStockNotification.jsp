<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<spring:htmlEscape defaultHtmlEscape="true" />

<div class="modal-dialog modal-dialog-centered modal-sm">
   <div class="modal-content">
      <div class="modal-header">
         <h5 class="modal-title text-center"><img class="logo" src="${themeResourcePath}/assets/bl-logo@2x.png"></h5>
         <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
      </div>
      <div class="modal-body">
         <h5>Get Notified</h5>
         <c:choose>
            <c:when test="${isAddingNotification}">
               <p class="body14">
                  <spring:theme code="pdp.rental.upcomming.product.add.subscription.text1"/>&nbsp;<strong>${productData.name}</strong>&nbsp;
                   <spring:theme code="pdp.rental.upcomming.product.add.subscription.text2" />
               </p>
            </c:when>
            <c:otherwise>
               <p class="body14">
                  <spring:theme code="pdp.rental.upcomming.product.remove.subscription.text"/>
               </p>
            </c:otherwise>
         </c:choose>
      </div>
   </div>
</div>
