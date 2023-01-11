package com.bl.facade;


import static org.junit.Assert.assertEquals;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.enums.Wishlist2EntryPriority;
import de.hybris.platform.wishlist2.impl.DefaultWishlist2Service;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;

import org.apache.poi.ss.formula.functions.T;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.services.wishlist.BlWishlistService;
import com.bl.facades.wishlist.data.Wishlist2EntryData;
import com.bl.facades.wishlist.impl.DefaultBlWishListFacade;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlWishListFacadeTest {

  @InjectMocks
  private final DefaultBlWishListFacade blWishListFacade = Mockito
      .spy(new DefaultBlWishListFacade());

  private static final String WLDEFAULT = "default";
  @Mock
  private UserService userService;
  @Mock
  private ProductService productService;
  @Mock
  private BlWishlistService blwishlistService;
  @Mock
  private Converter<Wishlist2EntryModel, Wishlist2EntryData> blWishList2EntryConverter;
  @Mock
  private DefaultWishlist2Service defaultWishlistService;
  @Mock
  ProductModel productModel;
  private UserModel cuurentUser;
  private ProductModel product1;
  private ProductModel product2;

  @Before
  public void setUp() throws InvalidCartException {
	  //MockitoAnnotations.initMocks(this);
    cuurentUser = userService.getUserForUID("user");
    userService.setCurrentUser(cuurentUser);
    product1 = productService.getProductForCode("Canon_50mm_f1.2_L");
    product2 = productService.getProductForCode("Canon_180mm_f3.5_Macro");
  }

  @Test
  public void addToWishlist() {
    defaultWishlistService.createDefaultWishlist(WLDEFAULT, "My default wishlist");
    defaultWishlistService
        .addWishlistEntry(product1, Integer.valueOf(1), Wishlist2EntryPriority.MEDIUM, "I like it");
    defaultWishlistService
        .addWishlistEntry(product2, Integer.valueOf(2), Wishlist2EntryPriority.HIGH, "Must have");
    assertEquals("Products on wishlist", 2,
        defaultWishlistService.getDefaultWishlist().getEntries().size());
    blWishListFacade.addToWishlist(product1.getCode());
  }

  @Test
  public void removeWishlist() {
    final Wishlist2Model wishlist = defaultWishlistService
        .createDefaultWishlist(WLDEFAULT, "My default wishlist");
    defaultWishlistService
        .addWishlistEntry(product1, Integer.valueOf(1), Wishlist2EntryPriority.MEDIUM, "good");
    assertEquals("one product in wishlist", 1, wishlist.getEntries().size());
    defaultWishlistService.removeWishlistEntryForProduct(product1, wishlist);
    assertEquals("no product in wishlist", 0, wishlist.getEntries().size());
  }

  @Test
  public void getWishlistEntries() {
    final PageableData pageableData = new PageableData();
    pageableData.setCurrentPage(0);
    pageableData.setPageSize(5);
    final SearchPageData<Wishlist2EntryModel> wishlistEntries = blwishlistService
        .getWishlistEntries(pageableData);
    final SearchPageData<T> result = new SearchPageData<T>();
    Mockito.when(result.getPagination()).thenReturn(wishlistEntries.getPagination());
    Mockito.when(result.getSorts()).thenReturn(wishlistEntries.getSorts());
    blWishListFacade.getWishlistEntries(pageableData);
    Assert.assertEquals(2, wishlistEntries.getResults().size());
  }
}


