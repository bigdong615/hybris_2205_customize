/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.controller;

import static com.bl.constants.BlhotfolderConstants.PLATFORM_LOGO_CODE;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.bl.service.BlhotfolderService;


@Controller
public class BlhotfolderHelloController
{
	@Autowired
	private BlhotfolderService blhotfolderService;

	@RequestMapping(value = "/", method = RequestMethod.GET)
	public String printWelcome(final ModelMap model)
	{
		model.addAttribute("logoUrl", blhotfolderService.getHybrisLogoUrl(PLATFORM_LOGO_CODE));
		return "welcome";
	}
}
