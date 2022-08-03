<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ attribute name="hideHeaderLinks" required="false"%>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="sec" uri="http://www.springframework.org/security/tags"%>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav"%>
<%@ taglib prefix="livechat" tagdir="/WEB-INF/tags/shared/livechat" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<spring:eval expression="@configurationService.configuration.getProperty('livechat.button.id.value')" var="buttonId"/>
<spring:eval expression="@configurationService.configuration.getProperty('livechat.endpointURL.link')" var="liveChatURL"/>
 <livechat:livechat/>

<cms:pageSlot position="TopHeaderSlot" var="component" element="div" class="container">
  <cms:component component="${component}" />
</cms:pageSlot>

<span id="timer-count" value="${usedGearTimer}"></span>

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

                   <!--  To display myaccount dropdown in mobile view  -->                   
                      <sec:authorize access="!hasAnyRole('ROLE_ANONYMOUS')">
                      <c:url var = "baseUrl" value="/my-account"/>
                      <c:url value="/logout" var="signoutUrl" />
                         <li><a class="dropdown-item" href="${baseUrl}/orders"><spring:theme code="text.orders" /></a></li>
                         <li><a class="dropdown-item" href="${baseUrl}/address-book"><spring:theme code="text.address" /></a></li>
                         <li><a class="dropdown-item" href="${baseUrl}/update-email"><spring:theme code="text.address.email" /></a></li>
                         <li><a class="dropdown-item" href="${baseUrl}/update-password"><spring:theme code="text.update.password" /></a></li>
                         <li><a class="dropdown-item" href="${baseUrl}/saved-carts"><spring:theme code="text.saved.cart" /></a></li>
                         <li><a class="dropdown-item" href="${baseUrl}/bookmarks"><spring:theme code="text.bookmarks" /></a></li>
                         <li><a class="dropdown-item" href="${baseUrl}/verificationImages"><spring:theme code="text.verification.documents" /></a></li>
                         <li><a class="dropdown-item" href="${baseUrl}/payment-details"><spring:theme code="text.credit.cards" /></a></li>
                         <li class="divider"></li>
                        <li><a class="dropdown-item" href="${signoutUrl}"><spring:theme code="text.header.account.sign.out" /></a></li>
                      </sec:authorize>
                      </ul>
                  </div>

            </li>
            <!-- BL-380 : Mini Cart Section for Mobile Device -->
            <!-- <div class="nav-cart"> -->
	            <cms:pageSlot position="MobileMiniCartSlot" var="component">
					<cms:component component="${component}"/>
			    </cms:pageSlot>
	   	    <!-- </div> -->
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
		<c:if test="${empty agent.uid}">
		<!-- [BL-1043] Live Chat Mobile view-->
     <%--<li id="liveAgentChat_online" class="live-chat clickGA livechat">
       <a id="liveagent_button_online_${buttonId}" href="javascript://Chat" onclick="liveagent.startChat('${buttonId}')" name="&amp;lid=GlobalHeader_live-chat clickGA"><!-- Online Chat Content -->
       <!----><span class="expertHelpIcon"></span><spring:theme code="text.live.chat.label" /></a>
     </li>--%>

     <li id="liveAgentChat_offline" class="live-chat clickGA" style="display: none !important;">
          <div id="liveagent_button_offline_${buttonId}" style=""><!-- Offline Chat Content --></div>
     </li>
     </c:if>
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
	   <c:if test="${empty agent.uid}">
     	   <%--<li id="liveAgentChat_online" class="live-chat clickGA nav-item" style="">
          <a style=" text-decoration:none;" id="liveagent_button_online_${buttonId}" href="javascript://Chat" onclick="liveagent.startChat('${buttonId}')" name="&amp;lid=GlobalHeader_live-chat clickGA"><!-- Online Chat Content -->
          <!----><span class="expertHelpIcon"></span></a>
          </li>--%>
          <li id="liveAgentChat_offline" class="live-chat clickGA" style="display: none !important;">
           <div id="liveagent_button_offline_${buttonId}" style=""><!-- Offline Chat Content --></div>
          </li>
          </c:if>
	   <li class="nav-item">
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