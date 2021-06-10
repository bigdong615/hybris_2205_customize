/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved
 */
package com.braintree.customersupportbackoffice.widgets;

import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.Label;

import com.hybris.cockpitng.util.DefaultWidgetController;

import com.braintree.customersupportbackoffice.services.BraintreecustomersupportbackofficeService;


public class BraintreecustomersupportbackofficeController extends DefaultWidgetController
{
	private static final long serialVersionUID = 1L;
	private Label label;

	@WireVariable
	private transient BraintreecustomersupportbackofficeService braintreecustomersupportbackofficeService;

	@Override
	public void initialize(final Component comp)
	{
		super.initialize(comp);
		label.setValue(braintreecustomersupportbackofficeService.getHello() + " BraintreecustomersupportbackofficeController");
	}
}
