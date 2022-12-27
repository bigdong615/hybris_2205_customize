<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>

<footer>
    <div class="container">
            <div class="row justify-content-center">
                <div class="col-12 col-xl-10">
                    <div class="row social-icon">
                        <cms:pageSlot position="FooterSlot" var="feature">
        					       	<cms:component component="${feature}"/>
    			         	   	</cms:pageSlot>
                  					      <cms:pageSlot position="SocialMediaSectionSlot" var="feature">
                                          <cms:component component="${feature}"/>
                                 </cms:pageSlot>
                        <div class="col-md-5 offset-md-1 col-lg-4 offset-lg-4">
                               <cms:pageSlot position="FooterSubscriptionSlot" var="feature">
                                           <cms:component component="${feature}"/>
                                </cms:pageSlot>
                        </div>
                    </div>

                </div>    
            </div> 
            <div id="copyright" class="row justify-content-center pt-5">
            <div class="col-12 col-xl-10">
            	<div class="row">
                <cms:pageSlot position="FooterCopyRightslot" var="feature">
                    <cms:component component="${feature}"/>
                </cms:pageSlot>
                </div>
                </div>
            </div>
        </div>

</footer>

<div class="modal fade" id="privacyconsent" tabindex="-1" aria-hidden="true">
   <div class="modal-dialog modal-dialog-centered modal-mm">
      <div class="modal-content">
         <div class="modal-header">
            <h5 class="modal-title" id="privacyconsent-title">Your Privacy</h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
         </div>
         <div class="modal-body">
            <div class="row">
               <p class="body14 mt-3">When you visit our site, we use cookies and similar technology to provide our advertising to you on other websites </p>
               <p class="body14 mt-3"><b>Enable personalized advertising</b></p>
               <p class="d-flex align-items-center body14 mb-3 ">Off &nbsp
                  <label class="switch">
                  <input type="checkbox" id="js-consent-box" >
                  <span class="slider round"></span>
                  </label>&nbspOn
               </p>
            </div>
            <div class="row">
               <p class="body14 mb-3"></p>
               <p class="body14 mb-3"></p>
               <p class="body14 mb-3"><b>CA CO VA CT Residents additional options</b><br> If youare a resident of one of these states and want more data option, <a href="<c:url value='/stateConfirm'/>">Click Here</a></p>
            </div>
         </div>
      </div>
   </div>
</div>
