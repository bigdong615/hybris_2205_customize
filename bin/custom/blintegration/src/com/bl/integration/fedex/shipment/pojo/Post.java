package com.bl.integration.fedex.shipment.pojo;

import java.lang.reflect.Parameter;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Post{
    @JsonProperty("summary") 
    public String getSummary() { 
		 return this.summary; } 
    public void setSummary(String summary) { 
		 this.summary = summary; } 
    String summary;
    @JsonProperty("description") 
    public String getDescription() { 
		 return this.description; } 
    public void setDescription(String description) { 
		 this.description = description; } 
    String description;
    @JsonProperty("operationId") 
    public String getOperationId() { 
		 return this.operationId; } 
    public void setOperationId(String operationId) { 
		 this.operationId = operationId; } 
    String operationId;
    @JsonProperty("requestBody") 
    public RequestBody getRequestBody() { 
		 return this.requestBody; } 
    public void setRequestBody(RequestBody requestBody) { 
		 this.requestBody = requestBody; } 
    RequestBody requestBody;
    @JsonProperty("responses") 
    public Responses getResponses() { 
		 return this.responses; } 
    public void setResponses(Responses responses) { 
		 this.responses = responses; } 
    Responses responses;
    @JsonProperty("parameters") 
    public List<Parameter> getParameters() { 
		 return this.parameters; } 
    public void setParameters(List<Parameter> parameters) { 
		 this.parameters = parameters; } 
    List<Parameter> parameters;
}
