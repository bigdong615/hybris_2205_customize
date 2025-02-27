<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<spring:eval expression="@configurationService.configuration.getProperty('livechat.endpointURL.link')" var="liveChatURL"/>
<spring:eval expression="@configurationService.configuration.getProperty('livechat.orgID.key.value')" var="organizationID"/>
<spring:eval expression="@configurationService.configuration.getProperty('livechat.org.deploymentID.value')" var="deploymentID"/>
<spring:eval expression="@configurationService.configuration.getProperty('livechat.button.id.value')" var="buttonId"/>


<!--/*Deployment:*/-->
<script type='text/javascript' src='https://c.la2-c1-iad.salesforceliveagent.com/content/g/js/48.0/deployment.js'></script>
<script type='text/javascript'>
liveagent.init('${liveChatURL}', '${organizationID}', '${deploymentID}');
</script>

<!--/*Chat buttons:*/-->
<script type="text/javascript">
	if (!window._laq) { window._laq = []; }
	window._laq.push(function()
			{
				liveagent.showWhenOnline('${buttonId}', document.getElementById('liveAgentChat_online'));
				liveagent.showWhenOffline('${buttonId}', document.getElementById('liveagent_button_offline_${buttonId}'));
			}
	);
</script>

