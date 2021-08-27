package com.bl.facades.cart.impl;

import com.bl.core.services.cart.BlCartService;
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
import java.util.concurrent.atomic.AtomicBoolean;
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
  @Resource(name = "cartService")
  private BlCartService blCartService;

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
   final StringBuilder removedBufferEntries = new StringBuilder();
   final AtomicBoolean isRemovedCart = new AtomicBoolean(false);
    if(CollectionUtils.isNotEmpty(savedCartModels.getResults())) {
      savedCartModels.getResults().forEach(cartModel -> {
      final String removedEntry =  blCartFacade.removeDiscontinueProductFromCart(cartModel, Boolean.FALSE);
            if(StringUtils.isNotEmpty(removedEntry)) {
              removedBufferEntries.append(BlFacadesConstants.COMMA_SEPERATER).append(removedEntry);
            }
            /** Collecting all cart code which don't have entry*/
        if (CollectionUtils.isEmpty(cartModel.getEntries())) {
          blCartService.removeEmptyCart(cartModel);
          isRemovedCart.set(true);
        }
      });
    }
    SearchPageData<CartModel> updatedSavedCartModels = savedCartModels;
    if(isRemovedCart.get()){
      /** pick updated saved cart data which show on saved cart page.*/
      updatedSavedCartModels = getCommerceSaveCartService().getSavedCartsForSiteAndUser(pageableData,
          getBaseSiteService().getCurrentBaseSite(), getUserService().getCurrentUser(), orderStatus);
    }
    model.addAttribute(BlFacadesConstants.REMOVE_ENTRIES,removedBufferEntries.toString());
    result.setPagination(updatedSavedCartModels.getPagination());
    result.setSorts(updatedSavedCartModels.getSorts());

    final List<CartData> savedCartDatas = Converters
        .convertAll(updatedSavedCartModels.getResults(), getCartConverter());

    result.setResults(savedCartDatas);
    return result;
  }
}
