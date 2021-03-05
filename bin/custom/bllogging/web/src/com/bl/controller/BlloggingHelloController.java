/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.controller;

import static com.bl.constants.BlloggingConstants.PLATFORM_LOGO_CODE;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.bl.service.BlloggingService;


@Controller
public class BlloggingHelloController
{
	@Autowired
	private BlloggingService blloggingService;

	@RequestMapping(value = "/", method = RequestMethod.GET)
	public String printWelcome(final ModelMap model)
	{
		model.addAttribute("logoUrl", blloggingService.getHybrisLogoUrl(PLATFORM_LOGO_CODE));
		return "welcome";
	}
}
