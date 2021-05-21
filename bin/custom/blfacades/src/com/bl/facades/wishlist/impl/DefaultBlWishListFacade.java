package com.bl.facades.wishlist.impl;

import com.bl.facades.wishlist.BlWishListFacade;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.Wishlist2Service;
import de.hybris.platform.wishlist2.enums.Wishlist2EntryPriority;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import java.util.List;

public class DefaultBlWishListFacade implements BlWishListFacade {

  private UserService userService;
  private ProductService productService;
  private Wishlist2Service wishlistService;

  @Override
  public void addToWishlist(String code) {
    final UserModel user = getUserService().getCurrentUser();
    final ProductModel product = getProductService().getProductForCode(code);
    if (getWishlistService().getDefaultWishlist(user) == null) {
      getWishlistService().createDefaultWishlist(user, "DefaultWishList", "wishlist");
      getWishlistService()
          .addWishlistEntry(user, product, 1, Wishlist2EntryPriority.MEDIUM, "Product ");
    } else {
      getWishlistService()
          .addWishlistEntry(user, product, 1, Wishlist2EntryPriority.MEDIUM, "Product");
    }

  }

  @Override
  public List<Wishlist2Model> getWishlist() {
    return getWishlistService().getWishlists();
  }

  @Override
  public void removeWishlist(String code) {
    final UserModel user = getUserService().getCurrentUser();
    final ProductModel product = getProductService().getProductForCode(code);
    Wishlist2Model model = getWishlistService().getDefaultWishlist(user);
    getWishlistService().removeWishlistEntryForProduct(product, model);
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

  public ProductService getProductService() {
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }

  public Wishlist2Service getWishlistService() {
    return wishlistService;
  }

  public void setWishlistService(final Wishlist2Service wishlistService) {
    this.wishlistService = wishlistService;
  }
}
