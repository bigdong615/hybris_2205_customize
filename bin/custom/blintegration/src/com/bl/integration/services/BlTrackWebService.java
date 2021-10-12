
package com.bl.integration.services;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Map;

public interface BlTrackWebService {

  Map trackService(final AbstractOrderModel abstractOrderModel , final PackagingInfoModel packagingInfoModel);

  }

