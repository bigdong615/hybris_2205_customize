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
  public String get(final BlSerialProductModel serialProduct) {
    if (Objects.isNull(serialProduct) || Objects.isNull(serialProduct.getBlProduct()) 
   		 || StringUtils.isBlank(serialProduct.getBlProduct().getFirmwareVersion())) {
   	 BlLogger.logMessage(LOG, Level.ERROR, 
   			 "Cannot evaluate the value for BlSerialProduct.skuFirmwareVersion because Serial or SKU or Firmware Version on SKU is null");
   	 return StringUtils.EMPTY;
    }
    final String skuFirmwareVersion = serialProduct.getBlProduct().getFirmwareVersion();
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Firmware version on SKU {} is {}", serialProduct.getBlProduct().getCode(), 
   		 skuFirmwareVersion);
    return skuFirmwareVersion;
  }

  @Override
  public void set(final BlSerialProductModel serialProduct, final String stringValue) {
    BlLogger.logMessage(LOG, Level.ERROR,
        "Setter for attribute BlSerialProduct.skuFirmwareVersion is not supported");
    throw new UnsupportedOperationException();
  }
}
