package com.bl.core.wishlist;

import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.wishlist2.impl.daos.Wishlist2Dao;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;

/**
 * Interface to fetch wishlistEntries
 * @author Sahana SB
 */
public interface BlWishlistDao extends Wishlist2Dao {

  /*
   * To fetch wishlistEntries for the pageable data
   */
  SearchPageData<Wishlist2EntryModel> getWishlistEntries(final PageableData pageableData);
}
