package com.bl.tax.stratergy;

import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.externaltax.DecideExternalTaxesStrategy;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.util.Config;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is created for abstract order
 * @author Manikandan
 */
public class BlDetermineExternalTaxStrategy implements DecideExternalTaxesStrategy {

  private static final Logger LOG = Logger.getLogger(BlDetermineExternalTaxStrategy.class);

  /**
   * This method added to check whether to calculate tax or not
   */
  @Override
  public boolean shouldCalculateExternalTaxes(final AbstractOrderModel abstractOrder) {
    if(null != abstractOrder) {
      return null != abstractOrder.getDeliveryMode() && null != abstractOrder.getDeliveryAddress()
          && Config.getBoolean("bl.calculate.externaltax", true);
    }
    BlLogger.logMessage(LOG , Level.INFO , "abstractOrder should not be null" ,new IllegalStateException());
    return false;
  }
}
