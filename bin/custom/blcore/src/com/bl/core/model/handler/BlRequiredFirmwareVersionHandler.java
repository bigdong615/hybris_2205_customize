package com.bl.core.model.handler;

import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is responsible to get dynamic value of serials required firmware version from SKU.
 *
 * @author Ravikumar
 */
public class BlRequiredFirmwareVersionHandler implements
    DynamicAttributeHandler<String, BlSerialProductModel> {

  private static final Logger LOG = Logger.getLogger(BlRequiredFirmwareVersionHandler.class);

  @Override
  public String get(BlSerialProductModel serialProduct) {
    if (Objects.nonNull(serialProduct) && Objects.nonNull(serialProduct.getBlProduct())) {
      return serialProduct.getBlProduct().getFirmwareVersion();
    }
    BlLogger.logMessage(LOG, Level.ERROR, "Enable to get firmware version from SKU");
    return StringUtils.EMPTY;
  }

  @Override
  public void set(BlSerialProductModel serialProduct, String stringValue) {
    BlLogger.logMessage(LOG, Level.ERROR,
        "Setter for attribute BlSerialProduct.skuFirmwareVersion is not supported");
    throw new UnsupportedOperationException();
  }
}
