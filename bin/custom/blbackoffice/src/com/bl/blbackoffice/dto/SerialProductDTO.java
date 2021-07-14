/**
 *
 */
package com.bl.blbackoffice.dto;

import java.io.Serializable;

import com.bl.core.model.BlSerialProductModel;


/**
 * @author Keyur
 *
 */
public class SerialProductDTO implements Serializable
{
	private static final long serialVersionUID = 1L;

	private BlSerialProductModel serialProduct;

	/**
	 * @return the serialProduct
	 */
	public BlSerialProductModel getSerialProduct()
	{
		return serialProduct;
	}

	/**
	 * @param serialProduct
	 *           the serialProduct to set
	 */
	public void setSerialProduct(final BlSerialProductModel serialProduct)
	{
		this.serialProduct = serialProduct;
	}

}
