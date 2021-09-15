package com.bl.core.order.strategy;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.logging.BlLogger;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.commerceservices.order.impl.DefaultCommercePlaceOrderStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.AbstractOrderEntryService;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.promotions.model.PromotionResultModel;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * @author Manikandan
 * This class override for setting order is to submit
 */
public class DefaultBlCommercePlaceOrderStrategy  extends DefaultCommercePlaceOrderStrategy {

  private static final Logger LOG = Logger.getLogger(DefaultBlCommercePlaceOrderStrategy.class);

  @Resource(name="abstractOrderEntryService")
  private AbstractOrderEntryService abstractOrderEntryService;

  /**
   * This method override to set the order to set as submit for tax
   */
  @Override
  public CommerceOrderResult placeOrder(final CommerceCheckoutParameter parameter) throws InvalidCartException
  {
    final CartModel cartModel = parameter.getCart();
    validateParameterNotNull(cartModel, "Cart model cannot be null");
    final CommerceOrderResult result = new CommerceOrderResult();
    try
    {
      beforePlaceOrder(parameter);
      if (getCalculationService().requiresCalculation(cartModel))
      {
        BlLogger.logFormattedMessage(LOG , Level.ERROR , "CartModel's [%s] calculated flag was false with cart code{}" , cartModel.getCode());
      }

      final CustomerModel customer = (CustomerModel) cartModel.getUser();
      validateParameterNotNull(customer, "Customer model cannot be null");
      final OrderModel orderModel = getOrderService().createOrderFromCart(cartModel);
      if (orderModel != null)
      {
        // Reset the Date attribute for use in determining when the order was placed
        orderModel.setDate(getTimeService().getCurrentTime());
        // Store the current site and store on the order
        orderModel.setSite(getBaseSiteService().getCurrentBaseSite());
        orderModel.setStore(getBaseStoreService().getCurrentBaseStore());
        orderModel.setLanguage(getCommonI18NService().getCurrentLanguage());

        if (parameter.getSalesApplication() != null)
        {
          orderModel.setSalesApplication(parameter.getSalesApplication());
        }

        // clear the promotionResults that where cloned from cart PromotionService.transferPromotionsToOrder will copy them over bellow.
        orderModel.setAllPromotionResults(Collections.<PromotionResultModel> emptySet());
        // Creating entry for bundle product.
        createEntryForBundleProduct(orderModel);
        getModelService().saveAll(customer, orderModel);

        if (cartModel.getPaymentInfo() != null && cartModel.getPaymentInfo().getBillingAddress() != null)
        {
          final AddressModel billingAddress = cartModel.getPaymentInfo().getBillingAddress();
          orderModel.setPaymentAddress(billingAddress);
          orderModel.getPaymentInfo().setBillingAddress(getModelService().clone(billingAddress));
          getModelService().save(orderModel.getPaymentInfo());
        }
        getModelService().save(orderModel);
        // Transfer promotions to the order
        getPromotionsService().transferPromotionsToOrder(cartModel, orderModel, false);

        // Calculate the order now that it has been copied
        try
        {
          orderModel.setIsOrderSubmit(Boolean.TRUE);
          getCalculationService().calculateTotals(orderModel, false);
          getExternalTaxesService().calculateExternalTaxes(orderModel);
        }
        catch (final CalculationException ex)
        {
          BlLogger.logFormattedMessage(LOG , Level.ERROR , "Failed to calculate order with code {}" , orderModel.getCode());
        }

        getModelService().refresh(orderModel);
        getModelService().refresh(customer);

        result.setOrder(orderModel);

        this.beforeSubmitOrder(parameter, result);

        getOrderService().submitOrder(orderModel);
      }
      else
      {
        throw new IllegalArgumentException(String.format("Order was not properly created from cart %s", cartModel.getCode()));
      }
    }
    finally
    {
      getExternalTaxesService().clearSessionTaxDocument();
    }

    this.afterPlaceOrder(parameter, result);
    return result;
  }

  /**
   * This method is used to create a separate entry for every product present in the bundle.
   * @param orderModel
   */
  private void createEntryForBundleProduct(final OrderModel orderModel) {
    final List<AbstractOrderEntryModel> orderEntryModelList = new ArrayList<>();
    orderEntryModelList.addAll(
        orderModel.getEntries().stream().filter(orderEntryModel -> !orderEntryModel.isBundleEntry())
            .collect(Collectors.toList()));
    final AtomicInteger entryNumber = new AtomicInteger(orderModel.getEntries().size());
    orderModel.getEntries().forEach(existingEntry -> {
      if (existingEntry.isBundleMainEntry() && !existingEntry.isEntryCreated()) {
        final Collection<ProductReferenceModel> productReferenceModels = existingEntry.getProduct()
            .getProductReferences().stream()
            .filter(productReferenceModel -> ProductReferenceTypeEnum.CONSISTS_OF
                .equals(productReferenceModel.getReferenceType())).collect(Collectors.toList());
        productReferenceModels.forEach(productReferenceModel -> {
          final AbstractOrderEntryModel newEntryModel = abstractOrderEntryService.createEntry(orderModel);
          populateOrderEntry(productReferenceModel, newEntryModel, existingEntry);
          newEntryModel.setEntryNumber(entryNumber.get());
          getModelService().save(newEntryModel);
          orderEntryModelList.add(newEntryModel);
          entryNumber.getAndIncrement();
        });
        existingEntry.setEntryCreated(Boolean.TRUE);
        getModelService().save(existingEntry);
      }
    });
    orderModel.setEntries(orderEntryModelList);
    getModelService().save(orderModel);
  }

  /**
   * This method is used to populate created entry data.
   */
  private void populateOrderEntry(final ProductReferenceModel productReferenceModel,
      final AbstractOrderEntryModel newEntryModel,
      final AbstractOrderEntryModel existingEntry) {
    final Long quantity = productReferenceModel.getQuantity()!= null ? productReferenceModel.getQuantity():1L;
    newEntryModel.setQuantity(existingEntry.getQuantity()*quantity);
    newEntryModel.setProduct(productReferenceModel.getTarget());
    newEntryModel.setBundleEntry(Boolean.TRUE);
    newEntryModel.setBundleProductCode(existingEntry.getProduct().getCode());
    newEntryModel.setBasePrice(0.0d);
    newEntryModel.setTotalPrice(0.0d);
    newEntryModel.setUnit(existingEntry.getUnit());
  }
}
