package com.bl.logging.impl;

import com.bl.logging.LogError;

/**
 * An enum for error codes. This can be internationalised if necessary, but the default English value is provided as
 * the description. This enum implements the interface {@link LogError}.
 *
 * @author Kushal Kumar
 */
public enum LogErrorCodeEnum implements LogError
{
    ORDER_MISSING("0001", "Order missing"),
    ORDER_INVALID("0002", "Order invalid"), 
	 INTEGRATION_ERROR("0003", "Integration Error"), 
	 CRONJOB_ERROR("0004", "CRONJOB ERROR"),
	 HOT_FOLDER_ERROR("0005", "HotFolder Error"),
	 DYNAMIC_PRICING_ERROR("0006", "Dynamic Price Calculating Error"),
	 SOLR_INDEXING_ERROR("0007", "Solr Property Indexing Error"),
	 CART_INTERNAL_ERROR("0008", "Error while updating the cart"),
   UPS_INTEGRATION_ERROR("0009","Error occure whiling calling UPS intergration service."),
    ORDER_SOURCING_ERROR("0010","Error occur while sourcing the order"),
  ORDER_ALLOCATION_ERROR("0011","Error occur while allocation of the order"),
  ORDER_OPTIMIZATION_ERROR("0012","Error occur while shipping optimization process for order");

    private String code;
    private String description;

    /**
     * @param code        the error code
     * @param description the default English description
     */
    LogErrorCodeEnum(final String code, final String description)
    {
        this.code = code;
        this.description = description;
    }

    @Override
    public String getCode()
    {
        return code;
    }

    @Override
    public String getDescription()
    {
        return description;
    }
}

