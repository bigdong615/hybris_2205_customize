package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SecuritySchemes{
    @JsonProperty("apiKeyHeader") 
    public ApiKeyHeader getApiKeyHeader() { 
		 return this.apiKeyHeader; } 
    public void setApiKeyHeader(ApiKeyHeader apiKeyHeader) { 
		 this.apiKeyHeader = apiKeyHeader; } 
    ApiKeyHeader apiKeyHeader;
    @JsonProperty("apiKeyQuery") 
    public ApiKeyQuery getApiKeyQuery() { 
		 return this.apiKeyQuery; } 
    public void setApiKeyQuery(ApiKeyQuery apiKeyQuery) { 
		 this.apiKeyQuery = apiKeyQuery; } 
    ApiKeyQuery apiKeyQuery;
}
