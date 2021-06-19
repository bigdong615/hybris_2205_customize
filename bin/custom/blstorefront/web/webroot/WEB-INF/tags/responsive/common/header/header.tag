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
            <li class="nav-item dropdown  nav-account">
                 <a class="nav-link dropdown-toggle" href="#" id="accountdropdown" data-bs-toggle="dropdown" aria-expanded="false"><spring:theme code="text.account.yourAccount"/></a>
                  <div class="dropdown-menu dropdown-menu-right" aria-labelledby="accountdropdown">
                   <h5><spring:theme code="text.account.yourAccount"/></h5>
                    <ul>
                      <sec:authorize access="hasAnyRole('ROLE_ANONYMOUS')">
                        <li><a class="dropdown-item js-signUp-popup"  data-link="<c:url value='/login/register'/>" href="#" data-bs-toggle="modal"
                         data-bs-target="#signUp"><spring:theme code="text.header.account.create.account" /></a></li>
           	            <li><a class="dropdown-item js-login-popup"  data-link="<c:url value='/login/loginpopup'/>" href="#" data-bs-toggle="modal"
           	             data-bs-target="#signIn"><spring:theme code="text.header.account.sign.in" /></a></li>
                      </sec:authorize>
                      <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
                      <c:url value="/logout" var="signoutUrl" />
                        <li><a class="dropdown-item" href="${signoutUrl}"><spring:theme code="text.header.account.sign.out" /></a></li>
                      </sec:authorize>
                      </ul>
                  </div>

            </li>
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

    <!-- Get notify modal start-->
    <div class="modal fade signinbox" id="getNotified" tabindex="-1" aria-hidden="true">
       <div class="modal-dialog modal-dialog-centered modal-sm">
       </div>
    </div>
   <!-- Get notify modal end -->

    <!-- modal for forgot password -->
        <div class="modal fade signinbox" id="forgotPass" tabindex="-1" aria-hidden="true">
           <div class="modal-dialog modal-dialog-centered modal-sm">

           </div>
        </div>