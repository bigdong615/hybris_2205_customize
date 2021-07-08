package com.bl.core.model.handler;

import com.bl.core.model.BlProductModel;
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
    if (Objects.nonNull(serialProduct)) {
   	 if(Objects.nonNull(serialProduct.getBlProduct())) {
   		 return getFirmwareVersionOfSku(serialProduct.getBlProduct());
   	 }   	 
   	 BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "No SKU is assigned to Serial Product with ProductId : {}", 
   			 serialProduct.getProductId());
   	 return StringUtils.EMPTY;
    }
    BlLogger.logMessage(LOG, Level.ERROR, "Cannot evaluate the value for BlSerialProduct.skuFirmwareVersion because Serial is null");
    return StringUtils.EMPTY;
  }
  
  /**
   * Gets the firmware version of sku.
   *
   * @param sku the sku
   * @return the firmware version of sku
   */
  private String getFirmwareVersionOfSku(final BlProductModel sku) {
	  final String skuFirmwareVersion = sku.getFirmwareVersion();
	  if(StringUtils.isNotBlank(skuFirmwareVersion)) {
		  return skuFirmwareVersion;
	  }
	  BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "No Firmware version found on SKU with code : {}", sku.getCode());
	  return StringUtils.EMPTY;
  }

  @Override
  public void set(final BlSerialProductModel serialProduct, final String stringValue) {
    BlLogger.logMessage(LOG, Level.ERROR,
        "Setter for attribute BlSerialProduct.skuFirmwareVersion is not supported");
    throw new UnsupportedOperationException();
  }
}
