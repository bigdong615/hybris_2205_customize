package com.bl.core.product.dao;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.product.daos.ProductDao;

import java.util.Collection;

import com.bl.core.model.BlProductModel;


/**
 * It is used to fetch the products
 *
 * @author Moumita
 *
 */
public interface BlProductDao extends ProductDao
{
	/**
	 * It fetches all the active sku products present in system
	 *
	 * @return Collection<BlProductModel> the list of sku products
	 */
	public Collection<BlProductModel> getAllActiveSkuProducts();

	/**
	 * It fetches all the active serial products associated with the sku products in system
	 *
	 * @return Collection<BlSerialProductModel> the list of serial products
	 */
	public Collection<BlSerialProductModel> getAssociatedActiveSerials(final Collection<BlProductModel> skuProducts);
}
