package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Parameter{
    @JsonProperty("name") 
    public String getName() { 
		 return this.name; } 
    public void setName(String name) { 
		 this.name = name; } 
    String name;
    @JsonProperty("in") 
    public String getIn() { 
		 return this.in; } 
    public void setIn(String in) { 
		 this.in = in; } 
    String in;
    @JsonProperty("description") 
    public String getDescription() { 
		 return this.description; } 
    public void setDescription(String description) { 
		 this.description = description; } 
    String description;
    @JsonProperty("required") 
    public boolean getRequired() { 
		 return this.required; } 
    public void setRequired(boolean required) { 
		 this.required = required; } 
    boolean required;
    @JsonProperty("schema") 
    public Schema getSchema() { 
		 return this.schema; } 
    public void setSchema(Schema schema) { 
		 this.schema = schema; } 
    Schema schema;
}