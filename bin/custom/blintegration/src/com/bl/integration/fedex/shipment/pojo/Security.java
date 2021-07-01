package com.bl.integration.fedex.shipment.pojo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Security{
    @JsonProperty("apiKeyHeader") 
    public List<Object> getApiKeyHeader() { 
		 return this.apiKeyHeader; } 
    public void setApiKeyHeader(List<Object> apiKeyHeader) { 
		 this.apiKeyHeader = apiKeyHeader; } 
    List<Object> apiKeyHeader;
    @JsonProperty("apiKeyQuery") 
    public List<Object> getApiKeyQuery() { 
		 return this.apiKeyQuery; } 
    public void setApiKeyQuery(List<Object> apiKeyQuery) { 
		 this.apiKeyQuery = apiKeyQuery; } 
    List<Object> apiKeyQuery;
}