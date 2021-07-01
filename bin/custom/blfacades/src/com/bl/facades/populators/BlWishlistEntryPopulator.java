package com.bl.facades.populators;

import com.bl.facades.wishlist.data.Wishlist2EntryData;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import java.util.Arrays;

/*
 * To Populate the WishlistEntryData
 * @author Sahana SB
 */
public class BlWishlistEntryPopulator implements
    Populator<Wishlist2EntryModel, Wishlist2EntryData> {

  private ProductFacade productFacade;

  @Override
  public void populate(final Wishlist2EntryModel source, final Wishlist2EntryData target)
      throws ConversionException {
    target.setProduct(populateProductData(source));
    target.setEntryPk(source.getPk().getLong());
  }

  private ProductData populateProductData(final Wishlist2EntryModel source) {

    final ProductData productData = getProductFacade()
        .getProductForCodeAndOptions(source.getProduct().getCode(),
            Arrays.asList(ProductOption.PRICE, ProductOption.REQUIRED_DATA, ProductOption.GALLERY,
                ProductOption.STOCK));
    return productData;
  }

  public ProductFacade getProductFacade() {
    return productFacade;
  }

  public void setProductFacade(ProductFacade productFacade) {
    this.productFacade = productFacade;
  }

}
