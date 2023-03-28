package com.bl.core.services.wishlist;

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
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.services.wishlist.impl.DefaultBlWishlistService;
import com.bl.core.wishlist.impl.DefaultBlWishlistDao;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlWishlistServiceTest {

  @InjectMocks
  private final DefaultBlWishlistService blWishlistService = Mockito
      .spy(new DefaultBlWishlistService());
  @Mock
  private DefaultBlWishlistDao blwishlistDao;
  Wishlist2EntryModel wishlist2Entry;

  @Test
  public void getWishlistEntries() {
    final PageableData pageableData = new PageableData();
    pageableData.setPageSize(5);
    pageableData.setCurrentPage(0);
    pageableData.setSort("creationtime");
    final SearchPageData<Wishlist2EntryModel> wishlistEntries = blwishlistDao
        .getWishlistEntries(pageableData);
    blWishlistService.getWishlistEntries(pageableData);
    Assert.assertEquals(wishlist2Entry, wishlistEntries);
  }
}

