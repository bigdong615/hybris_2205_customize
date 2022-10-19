package com.bl.integration.marketplace.jaxb;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "rss")
public class Rss
{
   protected Channel channel;
	protected String version;

	@XmlAttribute
	public String getVersion()
	{
		return version;
	}

	public void setVersion(final String version)
	{
		this.version = version;
	}

	public Channel getChannel()
	{
		return channel;
	}

	public void setChannel(final Channel channel)
	{
		this.channel = channel;
	}
}
