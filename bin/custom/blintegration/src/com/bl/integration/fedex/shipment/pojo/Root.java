package com.bl.integration.fedex.shipment.pojo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Root{
    @JsonProperty("openapi") 
    public String getOpenapi() { 
		 return this.openapi; } 
    public void setOpenapi(String openapi) { 
		 this.openapi = openapi; } 
    String openapi;
    @JsonProperty("info") 
    public Info getInfo() { 
		 return this.info; } 
    public void setInfo(Info info) { 
		 this.info = info; } 
    Info info;
    @JsonProperty("servers") 
    public List<Server> getServers() { 
		 return this.servers; } 
    public void setServers(List<Server> servers) { 
		 this.servers = servers; } 
    List<Server> servers;
    @JsonProperty("paths") 
    public Paths getPaths() { 
		 return this.paths; } 
    public void setPaths(Paths paths) { 
		 this.paths = paths; } 
    Paths paths;
    @JsonProperty("components") 
    public Components getComponents() { 
		 return this.components; } 
    public void setComponents(Components components) { 
		 this.components = components; } 
    Components components;
    @JsonProperty("security") 
    public List<Security> getSecurity() { 
		 return this.security; } 
    public void setSecurity(List<Security> security) { 
		 this.security = security; } 
    List<Security> security;
}
