package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.Wishlist2Service;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import java.util.Objects;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/*
 *This class is to populate Product is Bookmarked or Not.
 * @author Sahana SB
 */
public class BlWishlistOptionsPopulator implements Populator<BlProductModel, ProductData> {

  private static final Logger LOG = Logger.getLogger(BlWishlistOptionsPopulator.class);
  private UserService userService;
  private Wishlist2Service wishlistService;
  private ProductService productService;

  @Override
  public void populate(final BlProductModel source, final ProductData target)
      throws ConversionException {
    if (!userService.isAnonymousUser(userService.getCurrentUser())) {
      final CustomerModel user = (CustomerModel) getUserService().getCurrentUser();
      Wishlist2Model wishlist = getWishlistService().getDefaultWishlist(user);
      if (Objects.nonNull(wishlist)) {
        ProductModel product = null;
        try {
          product = getProductService().getProductForCode(source.getCode());
          Wishlist2EntryModel wishlist2Entry = getWishlistService()
              .getWishlistEntryForProduct(product, wishlist);
          if (Objects.nonNull(wishlist2Entry)) {
            target.setIsBookMarked(true);
          }
        } catch (ModelNotFoundException e) {
          target.setIsBookMarked(false);
          BlLogger
              .logMessage(LOG, Level.ERROR,
                  "Wishlist entry with product " + product.getCode() + "not found.",
                  e);
        } catch (UnknownIdentifierException e) {
          target.setIsBookMarked(false);
          BlLogger
              .logMessage(LOG, Level.ERROR,
                  "Wishlist entry with product " + product.getCode() + " in wishlist " + wishlist.getName() + "  not found.",
                  e);
        }
      }
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
