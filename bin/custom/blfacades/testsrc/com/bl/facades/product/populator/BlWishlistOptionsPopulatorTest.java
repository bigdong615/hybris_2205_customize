package com.bl.facades.product.populator;

import static de.hybris.platform.testframework.Assert.assertEquals;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.Wishlist2Service;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.util.ObjectUtils;

import com.bl.core.model.BlProductModel;
import com.bl.facades.populators.BlWishlistOptionsPopulator;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlWishlistOptionsPopulatorTest {

  @Mock
  private UserService userService;
  @Mock
  private Wishlist2Service wishlistService;
  @Mock
  private ProductService productService;
  @Mock
  BlProductModel productModel;

  private static final String WLDEFAULT = "default";
  private ProductModel productOne;
  private ProductModel productTwo;
  ProductData productData;
  private UserModel cuurentUser;

  @InjectMocks
  private final BlWishlistOptionsPopulator populator = Mockito.spy(new BlWishlistOptionsPopulator());

  @Test
  public void populateBookmarkTrue(){
    cuurentUser = userService.getUserForUID("user");
    productOne = productService.getProductForCode("HW2300-2356");
    productTwo = productService.getProductForCode("HW2300-4121");
    userService.setCurrentUser(cuurentUser);
    final Wishlist2Model wishlist = wishlistService.createDefaultWishlist(WLDEFAULT, "Default wishlist");
    Mockito.when(wishlistService.getDefaultWishlist(cuurentUser)).thenReturn(wishlist);
    final Wishlist2EntryModel wishlistEntry = wishlistService.getWishlistEntryForProduct(productOne, wishlist);
    populator.populate(productModel,productData);
    Assert.assertNotNull(!ObjectUtils.isEmpty(wishlistEntry));
    assertEquals(productData.getIsBookMarked(),Boolean.TRUE);
  }
}
