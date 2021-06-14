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
                  					      ${feature}gggggggg
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
                <cms:pageSlot position="FooterCopyRightslot" var="feature">
                    <cms:component component="${feature}"/>
                </cms:pageSlot> 
            </div>
        </div>  
</footer>

