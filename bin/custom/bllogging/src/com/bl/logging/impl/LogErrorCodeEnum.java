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
   ORDER_OPTIMIZATION_ERROR("0012","Error occur while shipping optimization process for order"),
   EMAIL_SUBSCRIPTION_INTEGRATION_ERROR("0013", "Email subscription integration Error"),
  CONSIGNMENT_CREATION_ERROR("0014", "Error occur while creating consignment"),
    ESP_EVENT_INTEGRATION_ERROR("0015", "Error occur while getting access token for ESP Event API"),
    ESP_EVENT_ACCESS_TOKEN_OBJECT_IS_MISSING("0016", "ClientId , AccountId , ClientSecret , GrantType , Scope is null "),
    ESP_EVENT_BASE_URL_OR_ACCESS_TOKEN_URL_IS_MISSING("0017", "Event Base URL Or Access Token URL is null"),
    ESP_EVENT_ACCESS_TOKEN_EMPTY("0018", "Event Access token is empty"),
    ESP_EVENT_ACCESS_TOKEN_API_FAILED_ERROR("0019", "Event access token call for ESP API failed."),
    ESP_EVENT_NO_ACCESS_TOKEN_RECEIVED_FROM_API_ERROR("0020", "No access token received from Access Token API."),
    ESP_EVENT_SERVICE_RESPONSE_NULL("0021", "Event Service Response is null"),
    ESP_EVENT_API_FAILED_ERROR("0022", "Event API call failed."),
  ESP_EVENT_POPULATOR_EXCEPTION("0023", "Error while populating data for ESP Event"),
  ESP_EVENT_REST_API_URL_AND_ENDPOINT_IS_MISSING("0024", "ESP Event Rest API URL And Endpoint is missing");

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

