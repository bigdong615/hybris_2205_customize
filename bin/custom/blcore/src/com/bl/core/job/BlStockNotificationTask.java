package com.bl.core.job;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.customerinterestsservices.model.ProductInterestModel;
import de.hybris.platform.notificationservices.enums.NotificationChannel;
import de.hybris.platform.notificationservices.enums.NotificationType;
import de.hybris.platform.notificationservices.service.NotificationService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.stocknotificationservices.constants.StocknotificationservicesConstants;
import de.hybris.platform.stocknotificationservices.cronjob.StockNotificationTask;
import java.util.Map;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;

/**
 * BL1815: This tread class was created to provide custom implementation of run method.
 * @author vijay vishwakarma.
 */
public class BlStockNotificationTask extends StockNotificationTask {

  private final NotificationService notificationService;
  private final Map<String, ItemModel> data;
  private final ProductInterestModel productInterest;
  private final ModelService modelService;

  public BlStockNotificationTask(final NotificationService notificationService, final ModelService modelService,
      final Map<String, ItemModel> data)
  {
    super(notificationService,modelService,data);
    this.notificationService = notificationService;
    this.data = data;
    this.productInterest = (ProductInterestModel) data.get(StocknotificationservicesConstants.PRODUCT_INTEREST);
    this.modelService = modelService;
  }

  @Override
  public void run()
  {
    final Set<NotificationChannel> notificationChannels = productInterest.getNotificationChannels();
    if (CollectionUtils.isEmpty(notificationChannels))
    {
      notificationService.notifyCustomer(NotificationType.BACK_IN_STOCK, productInterest.getCustomer(), data);
    }
    else
    {
      notificationService.notifyCustomer(NotificationType.BACK_IN_STOCK, productInterest.getCustomer(), notificationChannels,
          data);
    }
    modelService.remove(productInterest);
  }

}
