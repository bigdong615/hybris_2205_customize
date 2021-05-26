package com.bl.facades.wishlist;

import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import java.util.List;

public interface BlWishListFacade {

  void addToWishlist(String code);

  Wishlist2Model getWishlist();

  void removeWishlist(String code);

}
