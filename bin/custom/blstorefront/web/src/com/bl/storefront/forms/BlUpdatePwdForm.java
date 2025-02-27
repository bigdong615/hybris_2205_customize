/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.forms;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;


/** 
 * @author Aditi
 * Create form for BL-2094 : Password Policy - InfoSec updates 
 * To Update validation on reset password.
 * Form object for updating the password.
 */
public class BlUpdatePwdForm
{
	private String pwd;
	private String checkPwd;
	private String token;


	/**
	 * @return the pwd
	 */
	@NotNull(message = "{register.pwd.invalid}")
	@Size(min = 8, message = "{register.pwd.invalid}")
	public String getPwd()
	{
		return pwd;
	}

	/**
	 * @param pwd
	 *           the pwd to set
	 */
	public void setPwd(final String pwd)
	{
		this.pwd = pwd;
	}

	/**
	 * @return the checkPwd
	 */
	@Size(min = 8, message = "{register.pwd.invalid}")
	public String getCheckPwd()
	{
		return checkPwd;
	}

	/**
	 * @param checkPwd
	 *           the checkPwd to set
	 */
	public void setCheckPwd(final String checkPwd)
	{
		this.checkPwd = checkPwd;
	}

	/**
	 * @return the token
	 */
	public String getToken()
	{
		return token;
	}

	/**
	 * @param token
	 *           the token to set
	 */
	public void setToken(final String token)
	{
		this.token = token;
	}
}
