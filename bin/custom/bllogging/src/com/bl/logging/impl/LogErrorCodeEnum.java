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

