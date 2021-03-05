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
	 HOT_FOLDER_ERROR("0005", "HotFolder Error");
    //ADDRESS_FALL_BACK_ERROR("0073", "Address Fallback Not Found."),
    //SALES_ORDER_EXPORT_ERROR("0075","Error has occurred in sales order export"),
    //PRODUCT_IMPORT_ERROR("0003","Error has occurred in product import process");
    //TODO LOGGING -  further enums to be defined here...


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

