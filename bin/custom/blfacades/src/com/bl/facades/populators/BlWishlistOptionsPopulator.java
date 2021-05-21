package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.Wishlist2Service;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import org.springframework.util.ObjectUtils;

public class BlWishlistOptionsPopulator implements Populator<BlProductModel, ProductData> {

  private UserService userService;
  private Wishlist2Service wishlistService;
  private ProductService productService;

  @Override
  public void populate(BlProductModel source, ProductData target)
      throws ConversionException {

    final UserModel user = getUserService().getCurrentUser();
    Wishlist2Model wishlist2Model = getWishlistService().getDefaultWishlist(user);
    final ProductModel product = getProductService().getProductForCode(source.getCode());
    Wishlist2EntryModel entry = getWishlistService()
        .getWishlistEntryForProduct(product, wishlist2Model);

    try {
      if (!ObjectUtils.isEmpty(entry)) {
        target.setIsBookMarked(true);
      }
    } catch (Exception e) {
      target.setIsBookMarked(false);
    }

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

  public void setWishlistService(Wishlist2Service wishlistService) {
    this.wishlistService = wishlistService;
  }
}
