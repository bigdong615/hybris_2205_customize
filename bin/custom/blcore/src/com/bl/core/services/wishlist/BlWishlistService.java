package com.bl.core.services.wishlist;


import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;

/**
 * Created to get WishlistEntries for the pageable data
 *
 * @author Sahana SB
 */
public interface BlWishlistService {

  /*
   * This method is to get the Wishlistentries for the pageable data
   */
  SearchPageData<Wishlist2EntryModel> getWishlistEntries(final PageableData pageableData);


}
