package com.bl.facades.cart.impl;

import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.cart.BlSaveCartFacade;
import com.bl.facades.constants.BlFacadesConstants;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.impl.DefaultSaveCartFacade;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.converters.Converters;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.CartModel;

import java.util.List;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.ui.Model;


/**
 * This class was created for providing bl specific saved cart functionality.
 * @auther Vijay Vishwakarma
 */
public class DefaultBlSaveCartFacade extends DefaultSaveCartFacade implements BlSaveCartFacade {

  @Resource(name ="cartFacade")
  private BlCartFacade blCartFacade;

  /**
   *  This method overload to customize saved cart.
   * @param pageableData
   * @param orderStatus
   * @return
   */
  @Override
  public SearchPageData<CartData> getSavedCartsForCurrentUser(final PageableData pageableData, final List<OrderStatus> orderStatus,final Model model) // NOSONAR
  {
    final SearchPageData<CartData> result = new SearchPageData<>(); // NOSONAR
    final SearchPageData<CartModel> savedCartModels = getCommerceSaveCartService().getSavedCartsForSiteAndUser(pageableData,
        getBaseSiteService().getCurrentBaseSite(), getUserService().getCurrentUser(), orderStatus);
    // remove discontinue entry from cart.
    StringBuilder removedBufferEntries = new StringBuilder();
    if(CollectionUtils.isNotEmpty(savedCartModels.getResults())) {
      savedCartModels.getResults().forEach(cartModel -> {
        String removedEntry =  blCartFacade.removeDiscontinueProductFromCart(cartModel, Boolean.FALSE);
            if(StringUtils.isNotEmpty(removedEntry)) {
              removedBufferEntries.append(BlFacadesConstants.COMMA_SEPERATER).append(removedEntry);
            }
      });
    }
    model.addAttribute(BlFacadesConstants.REMOVE_ENTRIES,removedBufferEntries.toString());
    result.setPagination(savedCartModels.getPagination());
    result.setSorts(savedCartModels.getSorts());

    final List<CartData> savedCartDatas = Converters
        .convertAll(savedCartModels.getResults(), getCartConverter());

    result.setResults(savedCartDatas);
    return result;
  }
}
