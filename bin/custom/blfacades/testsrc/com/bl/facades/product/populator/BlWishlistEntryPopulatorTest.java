package com.bl.facades.product.populator;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;

import com.bl.facades.populators.BlWishlistEntryPopulator;
import com.bl.facades.wishlist.data.Wishlist2EntryData;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import java.util.Arrays;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.runners.MockitoJUnitRunner;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlWishlistEntryPopulatorTest {

  @InjectMocks
  private final BlWishlistEntryPopulator blWishlistEntryPopulator = Mockito
      .spy(new BlWishlistEntryPopulator());

  public static String PRODUCT_CODE = "testcode";
  @Mock
  private ProductFacade productFacade;
  @Mock
  private ProductModel productModel;
  @Mock
  private ProductData productData;
  @Mock
  private Wishlist2EntryModel wishlist2EntryModel;
  private Wishlist2EntryData wishlist2EntryData;

  @Test
  public void populate() {
    wishlist2EntryData = new Wishlist2EntryData();
    Mockito.when(wishlist2EntryModel.getPk()).thenReturn(PK.parseHex("8796322464446"));
    when(productModel.getCode()).thenReturn(PRODUCT_CODE);
   // when(wishlist2EntryModel.getProduct().getCode()).thenReturn(PRODUCT_CODE);
//    Mockito.when(wishlist2EntryModel.getProduct().getCode()).thenReturn("Canon_1Ds_Mark_II");
    given(
        productFacade.getProductForOptions(Matchers.any(),
            Arrays.asList(ProductOption.PRICE, ProductOption.REQUIRED_DATA, ProductOption.GALLERY,
                ProductOption.STOCK))).willReturn(productData);
    blWishlistEntryPopulator.populate(wishlist2EntryModel, wishlist2EntryData);
  }

}
