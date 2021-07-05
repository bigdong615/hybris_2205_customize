package com.bl.facades.cart;

import de.hybris.platform.commercefacades.order.SaveCartFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.enums.OrderStatus;
import java.util.List;
import org.springframework.ui.Model;

/**
 * This interface was created for providing bl specific saved cart functionality.
 * @auther Vijay Vishwakarma
 */
 public interface BlSaveCartFacade extends SaveCartFacade {

 /**
  * This method overload to customize saved cart.
  */
  SearchPageData<CartData> getSavedCartsForCurrentUser(final PageableData pageableData, final List<OrderStatus> orderStatus,final Model model); // NOSONAR
}
