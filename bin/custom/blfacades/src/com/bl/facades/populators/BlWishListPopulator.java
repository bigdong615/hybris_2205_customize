package com.bl.facades.populators;

import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class BlWishListPopulator implements Populator<Wishlist2EntryModel, ProductData> {


  private UserService userService;

  @Override
  public void populate(Wishlist2EntryModel source, ProductData target) {
    if (!userService.isAnonymousUser(userService.getCurrentUser())) {
      target.setDisplayName(source.getProduct().getName());
      target.setCode(source.getProduct().getCode());
   //   target.setImages((Collection<ImageData>) source.getProduct().getPicture());
//      for (ProductModel wishlistEntry : source.getProduct().) {
//        target.setDisplayName(wishlistEntry.getName());
//        // wishlistEntry.getProduct().
//      }
    }
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

}
