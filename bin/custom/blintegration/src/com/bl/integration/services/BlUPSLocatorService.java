package com.bl.integration.services;

import com.bl.facades.locator.data.UPSLocatorRequestData;
import com.bl.facades.locator.data.UpsLocatorResposeData;
import com.bl.facades.locator.data.UpsStoreData;
import java.util.List;

public interface BlUPSLocatorService {
  public UpsLocatorResposeData provideUPSLocation(UPSLocatorRequestData zipcode);
}
