package com.bl.storefront.controllers.pages;

public class BlControllerConstants
{

	public static final String FORM_GLOBAL_ERROR = "form.global.error";
	public static final String VALIDATE_CHECKPWD_EQUALS = "validation.checkPwd.equals";
	public static final String REGISTER_CHECKPWD_INVALID = "register.checkPwd.invalid";
	public static final String REGISTER_PWD_INVALID = "register.pwd.invalid";
	public static final String REGISTER_EMAIL_INVALID = "register.email.invalid";
	public static final int PASSWORD_MIN_LENGTH = 6;
	public static final int PASSWORD_MAX_LENGTH = 255;
	public static final int EMAIL_MAX_LENGTH = 255;
	public static final String DUBLICATE_UID_ERROR = "registration.error.account.exists.title";
	public static final String REGISTRATION_CONFIRMATION_MESSAGE = "registration.confirmation.message.title";
	public static final String EMAIL = "email";
	public static final String RENTAL_PAGE_IDENTIFIER = "rent";
	public static final String USED_PAGE_IDENTIFIER = "buy";
	public static final String FORWARD_SLASH = "/";
	public static final String PRODUCT_CODE_PATH_VARIABLE_PATTERN = FORWARD_SLASH +"{productCode:.*}";
	public static final String IS_RENTAL_PAGE = "IsRentalPage";
	public static final String HOME_CSS = "home ";
	public static final String HOMEPAGE = "homepage";
	public static final String COMMA = ",";

	private BlControllerConstants()
	{
		//empty
	}

}
