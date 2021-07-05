package com.bl.core.services.wishlist;

import com.bl.core.services.wishlist.impl.DefaultBlWishlistService;
import com.bl.core.wishlist.impl.DefaultBlWishlistDao;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.runners.MockitoJUnitRunner;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlWishlistServiceTest {

  @InjectMocks
  private final DefaultBlWishlistService blWishlistService = Mockito
      .spy(new DefaultBlWishlistService());
  @Mock
  private DefaultBlWishlistDao wishlistDao;

  @Test
  public void getWishlistEntries() {
    PageableData pageableData = new PageableData();
    pageableData.setPageSize(5);
    pageableData.setCurrentPage(0);
    pageableData.setSort("creationtime");
    Wishlist2EntryModel wishlist2Entry = new Wishlist2EntryModel();
    Mockito.when(wishlist2Entry.getProduct().getCode()).thenReturn("Canon_EF_135mm_f2.0L_USM");
    Mockito.when(wishlist2Entry.getProduct().getPriceQuantity()).thenReturn(130.78);
    SearchPageData<Wishlist2EntryModel> wishlistEntries = wishlistDao
        .getWishlistEntries(pageableData);
    blWishlistService.getWishlistEntries(pageableData);
    Assert.assertEquals(wishlist2Entry, wishlistEntries);
  }
}

