package com.bl.facades.populators;

import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import java.util.List;

public class BlWishListPopulator implements Populator<Wishlist2Model, ProductData> {


  private UserService userService;

  @Override
  public void populate(Wishlist2Model source, ProductData target)
      throws ConversionException {
    if (!userService.isAnonymousUser(userService.getCurrentUser())) {
      target.setName(source.getName());
    }
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

}
