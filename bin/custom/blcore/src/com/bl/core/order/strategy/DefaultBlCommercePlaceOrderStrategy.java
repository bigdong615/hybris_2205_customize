package com.bl.core.order.strategy;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.catalog.model.ProductReferenceModel;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.impl.DefaultBlProductService;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.order.impl.DefaultCommercePlaceOrderStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.promotions.model.PromotionResultModel;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * @author Manikandan
 * This class override for setting order is to submit
 */
public class DefaultBlCommercePlaceOrderStrategy  extends DefaultCommercePlaceOrderStrategy {

	private DefaultBlProductService defaultBlProductService;

  private static final Logger LOG = Logger.getLogger(DefaultBlCommercePlaceOrderStrategy.class);

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
      checkifVideoOrder(orderModel);
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
        getModelService().saveAll(customer, orderModel);

        if (cartModel.getPaymentInfo() != null && cartModel.getPaymentInfo().getBillingAddress() != null)
        {
          final AddressModel billingAddress = cartModel.getPaymentInfo().getBillingAddress();
          orderModel.setPaymentAddress(billingAddress);
          orderModel.getPaymentInfo().setBillingAddress(getModelService().clone(billingAddress));
          getModelService().save(orderModel.getPaymentInfo());
        }
        setPickFormDetailsInAddress(orderModel);
        getModelService().save(orderModel);
        // Transfer promotions to the order
        getPromotionsService().transferPromotionsToOrder(cartModel, orderModel, false);

        // Calculate the order now that it has been copied
        try
        {
          orderModel.setIsOrderSubmit(Boolean.TRUE);
          orderModel.setOrderModifiedDate(new Date());
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
   * It sets the pickup person details into delivery address
   * @param orderModel the order model
   */
  private void setPickFormDetailsInAddress(final OrderModel orderModel) {
    if(null != orderModel.getDeliveryAddress() && null != orderModel.getPickUpPersonFirstName()) {
      final AddressModel deliveryAddress = orderModel.getDeliveryAddress();
      deliveryAddress.setCompany(deliveryAddress.getFirstname());
      deliveryAddress.setFirstname(orderModel.getPickUpPersonFirstName());
      deliveryAddress.setLastname(orderModel.getPickUpPersonLastName());
      deliveryAddress.setEmail(orderModel.getPickUpPersonEmail());
      deliveryAddress.setPhone1(orderModel.getPickUpPersonPhone());
      getModelService().save(deliveryAddress);
      getModelService().refresh(deliveryAddress);
    }
  }
  
	/**
	 * This method is used to check if order is video order or not
	 *
	 * @param orderModel
	 */
	private void checkifVideoOrder(final OrderModel orderModel)
	{
		if (BooleanUtils.isFalse(orderModel.getIsRetailGearOrder()) && BooleanUtils.isFalse(orderModel.isGiftCardOrder()))
		{
			final List<AbstractOrderEntryModel> newCreatedMainBundleEntry = orderModel.getEntries().stream()
					.filter(entry -> entry.isBundleMainEntry() && !entry.isEntryCreated()).collect(Collectors.toList());

			if (CollectionUtils.isNotEmpty(newCreatedMainBundleEntry))
			{
				checkifBundleOrder(orderModel, newCreatedMainBundleEntry);
			}
			if (Objects.isNull(orderModel.getIsVideoOrder()) || BooleanUtils.isFalse(orderModel.getIsVideoOrder()))
			{
				checkifRentalOrUsedGearOrder(orderModel);
			}
		}
	}

	/**
	 * This method will update video flag for rental or used gear order
	 * @param orderModel
	 */
	private void checkifRentalOrUsedGearOrder(final OrderModel orderModel)
	{
		for (final AbstractOrderEntryModel orderEntries : orderModel.getEntries())
		{
			final ProductModel blProduct = orderEntries.getProduct();
			if (isVideoProduct(blProduct))
			{
				orderModel.setIsVideoOrder(Boolean.TRUE);
				break;
			}
		}
	}
	
	/**
	 * This method will mark bundle order as video order 
	 * @param orderModel
	 * @param newCreatedMainBundleEntry
	 */
	private void checkifBundleOrder(final OrderModel orderModel, final List<AbstractOrderEntryModel> newCreatedMainBundleEntry)
	{
		newCreatedMainBundleEntry.forEach(bundleEntry -> {
			final ProductModel bundleProduct = bundleEntry.getProduct();
			final List<ProductReferenceModel> bundleProductReferenceModel = getDefaultBlProductService()
					.getBundleProductReferenceModel(bundleProduct);
			for (final ProductReferenceModel referenceProduct : bundleProductReferenceModel)
			{
				if (referenceProduct.getTarget() instanceof BlProductModel
						&& Boolean.TRUE.equals(((BlProductModel) referenceProduct.getTarget()).getIsVideo()))
				{
					orderModel.setIsVideoOrder(Boolean.TRUE);
					break;
				}
			}
		});
	}

	/**
	 * This method is used to check if product is video product or not
	 *
	 * @param blProduct
	 * @return
	 */
	private boolean isVideoProduct(final ProductModel blProduct)
	{
		if (blProduct instanceof BlSerialProductModel)
		{
			return ((BlSerialProductModel) blProduct).getBlProduct().getIsVideo();
		}
		else if (blProduct instanceof BlProductModel)
		{
			return ((BlProductModel) blProduct).getIsVideo();
		}
		return false;
	}

	/**
	 * @return the defaultBlProductService
	 */
	public DefaultBlProductService getDefaultBlProductService()
	{
		return defaultBlProductService;
	}

	/**
	 * @param defaultBlProductService the defaultBlProductService to set
	 */
	public void setDefaultBlProductService(DefaultBlProductService defaultBlProductService)
	{
		this.defaultBlProductService = defaultBlProductService;
	}

}
