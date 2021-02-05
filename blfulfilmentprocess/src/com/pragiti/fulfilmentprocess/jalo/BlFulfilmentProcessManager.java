/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.pragiti.fulfilmentprocess.jalo;

import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jalo.extension.ExtensionManager;
import com.pragiti.fulfilmentprocess.constants.BlFulfilmentProcessConstants;

public class BlFulfilmentProcessManager extends GeneratedBlFulfilmentProcessManager
{
	public static final BlFulfilmentProcessManager getInstance()
	{
		ExtensionManager em = JaloSession.getCurrentSession().getExtensionManager();
		return (BlFulfilmentProcessManager) em.getExtension(BlFulfilmentProcessConstants.EXTENSIONNAME);
	}
	
}
