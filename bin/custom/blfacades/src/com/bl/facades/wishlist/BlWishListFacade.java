package com.bl.facades.wishlist;

import com.bl.facades.wishlist.data.Wishlist2EntryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;

/*
 * This Interface includes all the methods for adding product to Wishlist
 * removing product from Wishlist
 * fetch WishlistEntries for the Pageable data
 * @author Sahana SB
 */
public interface BlWishListFacade {

  /**
   * This method is used to add product to Wishlist
   * @param code
   */
  void addToWishlist(final String code);

  /**
   * This method is used to remove product to Wishlist
   * @param code
   */
  void removeWishlist(final String code);

  /**
   * This method is used to fetch WishlistEntries for the pageable data
   * @param pageableData
   * @return
   */
  SearchPageData<Wishlist2EntryData> getWishlistEntries(final PageableData pageableData);

}
