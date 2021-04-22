package com.bl.core.product.dao;

import java.util.Collection;

import com.bl.core.model.BlProductModel;


/**
 * It is used to fetch the products
 * 
 * @author Moumita
 *
 */
public interface BlProductDao
{
	/**
	 * It fetches all the active sku products present in system
	 * 
	 * @return Collection<BlProductModel> the list of sku products
	 */
	public Collection<BlProductModel> getAllActiveSkuProducts();
}
