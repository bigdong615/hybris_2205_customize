package com.bl.core.subscription.models;

import java.util.List;


/**
 * ContactResponse for subscribing the emails.
 *
 * @author Sunil Sahu
 */
public class ContactResponse{
	private String operationStatus;
	private int rowsAffetcted;
	private String contactKey;
	private int contactId;
	private int contactTypeID;
	private boolean isNewContactKey;
	private String requestServiceMessageID;
	private boolean hasErrors;
	private List<Object> resultMessages;
	private String serviceMessageID;

	 /**
	  * @return the operationStatus
	  */
	public String getOperationStatus()
	{
		return operationStatus;
	}
	/**
	 * @return the rowsAffetcted
	 */
	public int getRowsAffetcted()
	{
		return rowsAffetcted;
	}
	/**
	 * @return the contactKey
	 */
	public String getContactKey()
	{
		return contactKey;
	}
	/**
	 * @return the contactId
	 */
	public int getContactId()
	{
		return contactId;
	}
	/**
	 * @return the contactTypeID
	 */
	public int getContactTypeID()
	{
		return contactTypeID;
	}
	/**
	 * @return the isNewContactKey
	 */
	public boolean isNewContactKey()
	{
		return isNewContactKey;
	}
	/**
	 * @return the requestServiceMessageID
	 */
	public String getRequestServiceMessageID()
	{
		return requestServiceMessageID;
	}
	/**
	 * @return the hasErrors
	 */
	public boolean isHasErrors()
	{
		return hasErrors;
	}
	/**
	 * @return the resultMessages
	 */
	public List<Object> getResultMessages()
	{
		return resultMessages;
	}
	/**
	 * @return the serviceMessageID
	 */
	public String getServiceMessageID()
	{
		return serviceMessageID;
	}



	/**
	 * @param operationStatus
	 *           the operationStatus to set
	 */
	public void setOperationStatus(final String operationStatus)
	{
		this.operationStatus = operationStatus;
	}

	/**
	 * @param rowsAffetcted
	 *           the rowsAffetcted to set
	 */
	public void setRowsAffetcted(final int rowsAffetcted)
	{
		this.rowsAffetcted = rowsAffetcted;
	}

	/**
	 * @param contactKey
	 *           the contactKey to set
	 */
	public void setContactKey(final String contactKey)
	{
		this.contactKey = contactKey;
	}

	/**
	 * @param contactId
	 *           the contactId to set
	 */
	public void setContactId(final int contactId)
	{
		this.contactId = contactId;
	}

	/**
	 * @param contactTypeID
	 *           the contactTypeID to set
	 */
	public void setContactTypeID(final int contactTypeID)
	{
		this.contactTypeID = contactTypeID;
	}

	/**
	 * @param isNewContactKey
	 *           the isNewContactKey to set
	 */
	public void setNewContactKey(final boolean isNewContactKey)
	{
		this.isNewContactKey = isNewContactKey;
	}

	/**
	 * @param requestServiceMessageID
	 *           the requestServiceMessageID to set
	 */
	public void setRequestServiceMessageID(final String requestServiceMessageID)
	{
		this.requestServiceMessageID = requestServiceMessageID;
	}

	/**
	 * @param hasErrors
	 *           the hasErrors to set
	 */
	public void setHasErrors(final boolean hasErrors)
	{
		this.hasErrors = hasErrors;
	}

	/**
	 * @param resultMessages
	 *           the resultMessages to set
	 */
	public void setResultMessages(final List<Object> resultMessages)
	{
		this.resultMessages = resultMessages;
	}

	/**
	 * @param serviceMessageID
	 *           the serviceMessageID to set
	 */
	public void setServiceMessageID(final String serviceMessageID)
	{
		this.serviceMessageID = serviceMessageID;
	}
}