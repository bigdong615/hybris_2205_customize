ACC.privacyconsent = {

    _autoload: [
        "privacyConsentSlide",
    ],

    privacyConsentSlide: async function() {
        await airgap.sync();

        // Is user fully opted-in to core unessential tracking purposes:
        // Functional, Analytics, and Advertising
        const isOptedIn = airgap.isOptedIn();
        const consentCheckbox = document.getElementById('js-consent-box');
        consentCheckbox.checked = isOptedIn;
        consentCheckbox.addEventListener('change', (interaction) => {
            if (interaction.target.checked) {
                // Consent to all user-configurable tracking purposes
                airgap.optIn({
                    interaction
                });
            } else {
                // Opt-out of all user-configurable tracking purposes
                airgap.optOut({
                    interaction
                });
            }
        });

    },
}

 function displayConsent() {
            var ele = document.getElementsByName('privacy-consent');
            var baseUrl= window.location.origin;
            var checkboxSelectedUrl ="";

            for(i = 0; i < ele.length; i++) {
                if(ele[i].checked)
                checkboxSelectedUrl= ele[i].value
            }
           var redirectUrl= encodeURI(ACC.config.encodedContextPath+checkboxSelectedUrl);
            if(checkboxSelectedUrl != ""){
            window.location.href = redirectUrl;
            }
        }