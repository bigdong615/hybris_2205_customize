package com.bl.storefront.form;

import de.hybris.platform.acceleratorstorefrontcommons.forms.ConsentForm;

public class BlRegisterForm {
    private String email;
    private String pwd;
    private String checkPwd;

    public ConsentForm getConsentForm() {
        return consentForm;
    }

    public void setConsentForm(ConsentForm consentForm) {
        this.consentForm = consentForm;
    }

    private ConsentForm consentForm;

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPwd() {
        return pwd;
    }

    public void setPwd(String pwd) {
        this.pwd = pwd;
    }

    public String getCheckPwd() {
        return checkPwd;
    }

    public void setCheckPwd(String checkPwd) {
        this.checkPwd = checkPwd;
    }

}
