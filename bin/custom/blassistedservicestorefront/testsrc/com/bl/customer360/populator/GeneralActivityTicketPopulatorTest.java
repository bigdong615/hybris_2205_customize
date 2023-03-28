/*
 * [y] hybris Platform
 *
 * Copyright (c) 2018 SAP SE or an SAP affiliate company. All rights reserved.
 *
 * This software is the confidential and proprietary information of SAP
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with SAP.
 */
package com.bl.customer360.populator;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.ticket.enums.CsTicketCategory;
import de.hybris.platform.ticket.enums.CsTicketState;
import de.hybris.platform.ticket.model.CsTicketModel;

import java.util.Date;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.bl.customer360.GeneralActivityData;
import com.bl.customer360.populators.GeneralActivityTicketPopulator;

@UnitTest

public class GeneralActivityTicketPopulatorTest
{
    @InjectMocks
    private final GeneralActivityTicketPopulator populator = new GeneralActivityTicketPopulator();

    @Mock
    private BaseSiteService baseSiteService;

    @Before
    public void setup()
    {
		 // MockitoAnnotations.initMocks(this);
    }

    @Test
    public void populateTest()
    {
        final String headline = "HEADLINE";
        final String ticketId = "123";
        final CsTicketState aNew = CsTicketState.NEW;
        final Date created = new Date();
        final Date update = new Date();

        final CsTicketModel csTicketModel = new CsTicketModel();
        csTicketModel.setTicketID(ticketId);
        csTicketModel.setState(aNew);
        csTicketModel.setHeadline(headline);
        csTicketModel.setCategory(CsTicketCategory.COMPLAINT);
        csTicketModel.setCreationtime(created);
        csTicketModel.setModifiedtime(update);

        final GeneralActivityData generalActivityData = new GeneralActivityData();
        Mockito.when(baseSiteService.getCurrentBaseSite()).thenReturn(null);
        populator.populate(csTicketModel, generalActivityData);

        Assert.assertEquals(ticketId, generalActivityData.getId());
        Assert.assertEquals(aNew.getCode(), generalActivityData.getStatus());
        Assert.assertEquals(created, generalActivityData.getCreated());
        Assert.assertEquals(update, generalActivityData.getUpdated());
        Assert.assertEquals(headline, generalActivityData.getDescription());
    }
}

