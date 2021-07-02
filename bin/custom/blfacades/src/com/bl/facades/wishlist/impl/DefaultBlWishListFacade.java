package com.bl.facades.wishlist.impl;

import com.bl.core.services.wishlist.BlWishlistService;
import com.bl.facades.wishlist.BlWishListFacade;
import com.bl.facades.wishlist.data.Wishlist2EntryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.enums.Wishlist2EntryPriority;
import de.hybris.platform.wishlist2.impl.DefaultWishlist2Service;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import de.hybris.platform.converters.Converters;

/*
 *This class has implementation of methods related to Wishlist
 * @author Sahana SB
 */
public class DefaultBlWishListFacade implements BlWishListFacade {

  private UserService userService;
  private ProductService productService;
  private BlWishlistService blwishlistService;
  private Converter<Wishlist2EntryModel, Wishlist2EntryData> blWishList2EntryConverter;
  private DefaultWishlist2Service defaultWishlistService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void addToWishlist(String code) {
    final ProductModel product = getProductService().getProductForCode(code);
    if (getDefaultWishlistService().getDefaultWishlist(fetchUser()) == null) {
      getDefaultWishlistService().createDefaultWishlist(fetchUser(), "DefaultWishList", "wishlist");
      getDefaultWishlistService()
          .addWishlistEntry(fetchUser(), product, 1, Wishlist2EntryPriority.MEDIUM, "Product ");
    } else {
      getDefaultWishlistService()
          .addWishlistEntry(fetchUser(), product, 1, Wishlist2EntryPriority.MEDIUM, "Product");
    }

  }

  /**
   * Method to fetch the Current User
   *
   * @return CustomerModel
   */
  private CustomerModel fetchUser() {
    return (CustomerModel) getUserService().getCurrentUser();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeWishlist(String code) {
    final ProductModel product = getProductService().getProductForCode(code);
    Wishlist2Model model = getDefaultWishlistService().getDefaultWishlist(fetchUser());
    getDefaultWishlistService().removeWishlistEntryForProduct(product, model);
  }

  /**
   * {@inheritDoc}
   *
   * @return SearchPageData of Wishlist2EntryData
   */
  @Override
  public SearchPageData<Wishlist2EntryData> getWishlistEntries(final PageableData pageableData) {
    final SearchPageData<Wishlist2EntryModel> wishlistEntries = getBlwishlistService()
        .getWishlistEntries(pageableData);
    return convertPageData(wishlistEntries, getBlWishList2EntryConverter());
  }

  /*
   * Method to convert Page data
   */
  protected <S, T> SearchPageData<T> convertPageData(final SearchPageData<S> source,
      final Converter<S, T> converter) {
    final SearchPageData<T> result = new SearchPageData<T>();
    result.setPagination(source.getPagination());
    result.setSorts(source.getSorts());
    result.setResults(Converters.convertAll(source.getResults(), converter));
    return result;
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

  public DefaultWishlist2Service getDefaultWishlistService() {
    return defaultWishlistService;
  }

  public void setDefaultWishlistService(
      DefaultWishlist2Service defaultWishlistService) {
    this.defaultWishlistService = defaultWishlistService;
  }

  public BlWishlistService getBlwishlistService() {
    return blwishlistService;
  }

  public void setBlwishlistService(BlWishlistService blwishlistService) {
    this.blwishlistService = blwishlistService;
  }

  public Converter<Wishlist2EntryModel, Wishlist2EntryData> getBlWishList2EntryConverter() {
    return blWishList2EntryConverter;
  }

  public void setBlWishList2EntryConverter(
      Converter<Wishlist2EntryModel, Wishlist2EntryData> blWishList2EntryConverter) {
    this.blWishList2EntryConverter = blWishList2EntryConverter;
  }
}
