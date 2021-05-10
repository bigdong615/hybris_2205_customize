/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.integration.facades;

import de.hybris.platform.core.servicelayer.data.SearchPageData;

import java.util.List;

import com.bl.integration.data.UserData;
import com.bl.integration.dto.SampleWsDTO;
import com.bl.integration.dto.TestMapWsDTO;


public interface SampleFacades
{
	SampleWsDTO getSampleWsDTO(final String value);

	UserData getUser(String id);

	List<UserData> getUsers();

	SearchPageData<UserData> getUsers(SearchPageData<?> params);

	TestMapWsDTO getMap();
}
