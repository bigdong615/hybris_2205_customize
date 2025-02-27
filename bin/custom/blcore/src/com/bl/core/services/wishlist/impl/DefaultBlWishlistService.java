package com.bl.core.services.wishlist.impl;

import com.bl.core.services.wishlist.BlWishlistService;
import com.bl.core.wishlist.impl.DefaultBlWishlistDao;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.wishlist2.impl.DefaultWishlist2Service;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;

/*
 *Created to get WishlistEntries for the pageable data
 * @author Sahana SB
 */
public class DefaultBlWishlistService extends DefaultWishlist2Service implements BlWishlistService {

  private DefaultBlWishlistDao blwishlistDao;

  /**
   * {@inheritDoc}
   */
  @Override
  public SearchPageData<Wishlist2EntryModel> getWishlistEntries(final PageableData pageableData) {
    return getBlwishlistDao().getWishlistEntries(pageableData);
  }

  public DefaultBlWishlistDao getBlwishlistDao() {
    return blwishlistDao;
  }

  public void setBlwishlistDao(DefaultBlWishlistDao blwishlistDao) {
    this.blwishlistDao = blwishlistDao;
  }

}
