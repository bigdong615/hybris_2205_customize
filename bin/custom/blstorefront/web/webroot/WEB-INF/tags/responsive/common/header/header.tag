<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="hideHeaderLinks" required="false"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="sec" uri="http://www.springframework.org/security/tags"%>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav"%>

<spring:htmlEscape defaultHtmlEscape="true" />

      <cms:pageSlot position="PromoBannerSlot" var="component" element="div" > 
         <cms:component component="${component}" />
    </cms:pageSlot>

<nav class="navbar navbar-expand-lg">
<div class="container">
<a class="mobileNavToggle d-inline-block d-lg-none" href="#my-menu"><span></span></a>
<cms:pageSlot position="SiteLogoHeaderSlot" var="logo" limit="1">
						<cms:component component="${logo}"/>
					</cms:pageSlot>
 <div class="mobile-right d-inline-block d-lg-none">  
            <div class="nav-account"><a href="#">My Account</a></div>
            <!-- BL-380 : Mini Cart Section for Mobile Device -->
            <div class="nav-cart">
	            <cms:pageSlot position="MobileMiniCartSlot" var="component">
					<cms:component component="${component}"/>
			    </cms:pageSlot>
	   		</div>
  </div>	
  <!-- Mobile Menu -->	
  <nav id="my-menu">	
  	<ul>
  		<!-- BL-377 Mobile Navigation bar -->
  		 <cms:pageSlot position="NavigationBarMobileSlot" var="component">
			<cms:component component="${component}" />	
         </cms:pageSlot>
         <!-- BL-388 Mobile device - Header - Ship or PickUp section -->
         <li>
			<cms:pageSlot position="MobileHeaderLinkForShipOrPickupSlot" var="component" class="">
				<cms:component component="${component}" />
			</cms:pageSlot>
		</li>
		<!-- BL-385 Mobile device - Header - Support section -->
		<li>
			<cms:pageSlot position="MobileHeaderLinkForSupportSlot" var="component" class="">
				<cms:component component="${component}" />
		   </cms:pageSlot>
		</li>
		
		<li>
			<cms:pageSlot position="MobileHeaderLinkForAccountSlot" var="component" class="">
				<cms:component component="${component}" />
		   </cms:pageSlot>
		</li>
		<li>
			<span>
				<cms:pageSlot position="MobileHeaderBottomInfo" var="component" class="">
				<cms:component component="${component}" element="div" class="mnav-third"/>
		   </cms:pageSlot>
			</span>
		</li>
  	</ul>
  </nav>
  <div class="collapse navbar-collapse" id="blnav">
  	<ul class="navbar-nav me-auto mb-2 mb-md-0" role="menu">
	  	<cms:pageSlot position="NavigationBarSlot" var="component" class="">
			<cms:component component="${component}" />
		</cms:pageSlot>
	</ul>
	<ul class="navbar-nav navbar-right ms-auto mb-2 mb-md-0">
		<li class="nav-item dropdown menu-large">
       <cms:pageSlot position="HeaderLinkForShipOrPickupSlot" var="component" class="">
			<cms:component component="${component}" />
	   </cms:pageSlot>
	   </li>
	   <li class="nav-item dropdown menu-large">
       <cms:pageSlot position="HeaderLinkForSupportSlot" var="component" class="">
			<cms:component component="${component}" />
	   </cms:pageSlot>
	   </li>
	   <li class="nav-item dropdown nav-account">
	   	<cms:pageSlot position="MyAccountSlot" var="component" class="">
			<cms:component component="${component}" />
	   </cms:pageSlot>
	   </li>
	   <li class="nav-item nav-cart">
	   <cms:pageSlot position="MiniCartSlot" var="component" class="">
			<cms:component component="${component}" />
	   </cms:pageSlot>      
	   </li>
    </ul>
  </div>			
					
</div>
</nav>

<!-- modal for sign in -->
   <div class="modal fade signinbox" id="signIn" aria-hidden="true" aria-labelledby="..." tabindex="-1">
   <div class="modal-dialog modal-dialog-centered modal-sm">
   </div>
  </div>
    <!-- modal for sign up -->
    <div class="modal fade signinbox" id="signUp" aria-hidden="true" aria-labelledby="..." tabindex="-1">
     <div class="modal-dialog modal-dialog-centered modal-sm">
     </div>
    </div>

    <!-- modal for forgot password -->
        <div class="modal fade signinbox" id="forgotPass" tabindex="-1" aria-hidden="true">
          <div class="modal-dialog modal-dialog-centered modal-sm">
            <div class="modal-content">
              <div class="modal-header">
                <h5 class="modal-title text-center"><img class="logo" src="${themeResourcePath}/assets/bl-logo@2x.png"></h5>
                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
              </div>
              <div class="modal-body">
                  <h5><spring:theme code="forgottenPwd.header"/></h5>
                  <p class="body14"><spring:theme code="forgottenPwd.description"/></p>
                  <form>
                    <input type="text" class="form-control mb-3" placeholder="register.email">
                    <button type="submit" class="btn btn-block btn-primary mt-4"><spring:theme code="forgottenPwd.title"/></button>
                    <p class="body14 text-center mb-0 mt-4"><a href="#"><spring:theme code="login.login"/></a></p>
                  </form>
              </div>
            </div>
          </div>
        </div>