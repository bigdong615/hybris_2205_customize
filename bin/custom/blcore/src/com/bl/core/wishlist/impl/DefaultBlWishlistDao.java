package com.bl.core.wishlist.impl;

import com.bl.core.wishlist.BlWishlistDao;
import de.hybris.platform.commerceservices.search.flexiblesearch.PagedFlexibleSearchService;
import de.hybris.platform.commerceservices.search.flexiblesearch.data.SortQueryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.impl.daos.impl.DefaultWishlist2Dao;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class is used to fetch Wishlistentries for the Pageable data
 *
 * @author Sahana SB
 */
public class DefaultBlWishlistDao extends DefaultWishlist2Dao implements BlWishlistDao {

  private UserService userService;
  private PagedFlexibleSearchService pagedFlexibleSearchService;

//  private static final String SORT_WISHLIST_BY_DATE =
//      " ORDER BY {" + Wishlist2EntryModel.CREATIONTIME + "} DESC ;

  private static final String SORT_WISHLIST_BY_DATE = " ORDER BY {" + Wishlist2EntryModel.CREATIONTIME + "} DESC, {" + Wishlist2EntryModel.PK + "}";
  private static final String FIND_WISHLIST_ENTRIES = "select {wle:pk} from {Wishlist2 as wl}, {Wishlist2Entry as wle} where {wle.wishlist} = {wl.pk} and {wl.user}= ?user";

  /**
   * {@inheritDoc}
   */
  @Override
  public SearchPageData<Wishlist2EntryModel> getWishlistEntries(final PageableData pageableData) {
    final List<SortQueryData> sortQueries;
    final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();
    final Map<String, Object> queryParams = new HashMap<String, Object>();
    queryParams.put("user", currentCustomer);
    sortQueries = Arrays.asList(createSortQueryData("byDate",
        createQuery(FIND_WISHLIST_ENTRIES, SORT_WISHLIST_BY_DATE)));
    return getPagedFlexibleSearchService().search(sortQueries, "byDate", queryParams, pageableData);
  }

  /**
   * To Sort the Wishlist Entries By Creation time.
   * @param sortCode
   * @param query
   * @return Sort Query Data
   */
  protected SortQueryData createSortQueryData(final String sortCode, final String query) {
    final SortQueryData result = new SortQueryData();
    result.setSortCode(sortCode);
    result.setQuery(query);
    return result;
  }

  /**
   *  This method joins the query to fetch wishlist entries from wishlistmodel for a particular user
   *  order by descending.
   * @param queryClauses
   * @return The Query
   */
  protected String createQuery(final String... queryClauses) {
    final StringBuilder queryBuilder = new StringBuilder();

    for (final String queryClause : queryClauses) {
      queryBuilder.append(queryClause);
    }

    return queryBuilder.toString();
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

  public PagedFlexibleSearchService getPagedFlexibleSearchService() {
    return pagedFlexibleSearchService;
  }

  public void setPagedFlexibleSearchService(
      PagedFlexibleSearchService pagedFlexibleSearchService) {
    this.pagedFlexibleSearchService = pagedFlexibleSearchService;
  }


}
