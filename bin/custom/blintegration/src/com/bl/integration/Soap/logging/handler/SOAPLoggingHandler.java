/*
 *
 */
package com.bl.integration.Soap.logging.handler;


import java.io.PrintStream;
import java.util.Collections;
import java.util.Set;

import javax.xml.namespace.QName;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.handler.MessageContext;
import javax.xml.ws.handler.soap.SOAPHandler;
import javax.xml.ws.handler.soap.SOAPMessageContext;


public class SOAPLoggingHandler implements SOAPHandler<SOAPMessageContext>
{


	/*
	 * This simple SOAPHandler will output the contents of incoming and outgoing messages.
	 */

	// change this to redirect output if desired
	private static PrintStream out = System.out; //NOSONAR

	public Set<QName> getHeaders()
	{
		return Collections.emptySet();
	}

	public boolean handleMessage(final SOAPMessageContext smc)
	{
		logToSystemOut(smc);
		return true;
	}

	public boolean handleFault(final SOAPMessageContext smc)
	{
		logToSystemOut(smc);
		return true;
	}

	// nothing to clean up
	public void close(final MessageContext messageContext)
	{
		//
	}

	/*
	 * Check the MESSAGE_OUTBOUND_PROPERTY in the context to see if this is an outgoing or incoming message. Write a
	 * brief message to the print stream and output the message. The writeTo() method can throw SOAPException or
	 * IOException
	 */
	private void logToSystemOut(final SOAPMessageContext smc)
	{
		final Boolean outboundProperty = (Boolean) smc.get(MessageContext.MESSAGE_OUTBOUND_PROPERTY);

		if (outboundProperty.booleanValue())
		{
			out.println("\nOutbound message:");
		}
		else
		{
			out.println("\nInbound message:");
		}

		final SOAPMessage message = smc.getMessage();
		try
		{
			message.writeTo(out);
			out.println(""); // just to add a newline
		}
		catch (final Exception e)
		{
			out.println("Exception in handler: " + e);
		}
	}

}
