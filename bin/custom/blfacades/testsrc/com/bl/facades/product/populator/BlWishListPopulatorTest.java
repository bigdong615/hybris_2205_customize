package com.bl.facades.product.populator;

import static de.hybris.platform.testframework.Assert.assertEquals;
import static org.mockito.Mockito.when;

import com.bl.facades.populators.BlWishListPopulator;
import com.bl.facades.wishlist.data.Wishlist2Data;
import com.bl.facades.wishlist.data.Wishlist2EntryData;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.runners.MockitoJUnitRunner;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlWishListPopulatorTest {

  public static String PRODUCT_NAME = "test product";
  public static String DESCRIPTION = "This is sort description";

  @InjectMocks
  private final BlWishListPopulator populator = Mockito.spy(new BlWishListPopulator());

  @Mock
  private UserService userService;
  @Mock
  private Converter<Wishlist2EntryModel, Wishlist2EntryData> blWishList2EntryConverter;
  @Mock
  Wishlist2Model wishlist2Model;
  Wishlist2Data wishlist2Data;
  private UserModel curentUser;

  @Test
  public void populateTest() {
    curentUser = userService.getUserForUID("user");
    userService.setCurrentUser(curentUser);
    Wishlist2EntryModel wishlist2EntryModel = new Wishlist2EntryModel();
    wishlist2EntryModel.getProduct().getName(Locale.forLanguageTag("Canon"));
    List<Wishlist2EntryModel> entries = new ArrayList<>();
    entries.add(wishlist2EntryModel);
    when(wishlist2Model.getName()).thenReturn(PRODUCT_NAME);
    when(wishlist2Model.getDescription()).thenReturn(DESCRIPTION);
    when(wishlist2Model.getEntries()).thenReturn(entries);
    populator.populate(wishlist2Model, wishlist2Data);
    assertEquals(wishlist2Data.getName(), PRODUCT_NAME);
    assertEquals(wishlist2Data.getDecription(), DESCRIPTION);
  }
}
