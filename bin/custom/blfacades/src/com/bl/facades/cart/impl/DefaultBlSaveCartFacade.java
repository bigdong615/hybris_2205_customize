package com.bl.facades.cart.impl;

import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.cart.BlSaveCartFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.impl.DefaultSaveCartFacade;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.converters.Converters;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.CartModel;

import java.util.List;
import javax.annotation.Resource;

public class DefaultBlSaveCartFacade extends DefaultSaveCartFacade implements BlSaveCartFacade {

  @Resource(name ="cartFacade")
  private BlCartFacade blCartFacade;

  @Override
  public SearchPageData<CartData> getSavedCartsForCurrentUser(final PageableData pageableData,
      final List<OrderStatus> orderStatus)
  {
    final SearchPageData<CartData> result = new SearchPageData<>();
    final SearchPageData<CartModel> savedCartModels = getCommerceSaveCartService().getSavedCartsForSiteAndUser(pageableData,
        getBaseSiteService().getCurrentBaseSite(), getUserService().getCurrentUser(), orderStatus);

    savedCartModels.getResults().forEach(cartModel -> {
      blCartFacade.removeDiscontinueProductFromCart(cartModel);
    });


    result.setPagination(savedCartModels.getPagination());
    result.setSorts(savedCartModels.getSorts());

    final List<CartData> savedCartDatas = Converters
        .convertAll(savedCartModels.getResults(), getCartConverter());

    result.setResults(savedCartDatas);
    return result;
  }
}
