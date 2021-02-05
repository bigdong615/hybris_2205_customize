/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.pragiti.core.jalo;

import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jalo.extension.ExtensionManager;
import com.pragiti.core.constants.BlCoreConstants;
import com.pragiti.core.setup.CoreSystemSetup;


/**
 * Do not use, please use {@link CoreSystemSetup} instead.
 * 
 */
public class BlCoreManager extends GeneratedBlCoreManager
{
	public static final BlCoreManager getInstance()
	{
		final ExtensionManager em = JaloSession.getCurrentSession().getExtensionManager();
		return (BlCoreManager) em.getExtension(BlCoreConstants.EXTENSIONNAME);
	}
}
