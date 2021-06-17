package com.bl.facades.productreference.impl;

import com.bl.core.model.GiftCardModel;
import com.bl.core.services.gitfcard.BlGiftCardService;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.giftcard.BlGiftCardFacade;
import com.bl.facades.productreference.BlCommerceProductReferenceService;
import com.bl.facades.productreference.BlProductFacade;
import com.bl.facades.productreference.service.DefaultBlCommerceProductReferenceService;
import com.bl.logging.BlLogger;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.commercefacades.order.CheckoutFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.commerceservices.product.CommerceProductReferenceService;
import de.hybris.platform.commerceservices.product.data.ReferenceData;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.converters.ConfigurablePopulator;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is a default implementation of {@link BlGiftCardFacade}.
 *
 * @author Neeraj Singh
 */
public class DefaultBlProductFacade<REF_TARGET> implements BlProductFacade {

  private BlCommerceProductReferenceService<ProductReferenceTypeEnum, REF_TARGET> blCommerceProductReferenceService;
  private Converter<ReferenceData<ProductReferenceTypeEnum, REF_TARGET>, ProductReferenceData> referenceDataProductReferenceConverter;
  private ConfigurablePopulator<REF_TARGET, ProductData, ProductOption> referenceProductConfiguredPopulator;

  public List<ProductReferenceData> getProductReferencesForCode(final ProductModel currentProduct,
      final List<ProductOption> options, final Integer limit) {
    final List<ReferenceData<ProductReferenceTypeEnum, REF_TARGET>> references = getBlCommerceProductReferenceService()
        .getProductReferencesForCode(currentProduct, limit);

    final List<ProductReferenceData> result = new ArrayList<ProductReferenceData>();

    for (final ReferenceData<ProductReferenceTypeEnum, REF_TARGET> reference : references) {
      final ProductReferenceData productReferenceData = getReferenceDataProductReferenceConverter()
          .convert(reference);
      getReferenceProductConfiguredPopulator()
          .populate(reference.getTarget(), productReferenceData.getTarget(), options);
      result.add(productReferenceData);
    }

    return result;
  }

  public BlCommerceProductReferenceService<ProductReferenceTypeEnum, REF_TARGET> getBlCommerceProductReferenceService() {
    return blCommerceProductReferenceService;
  }

  public void setBlCommerceProductReferenceService(
      BlCommerceProductReferenceService<ProductReferenceTypeEnum, REF_TARGET> blCommerceProductReferenceService) {
    this.blCommerceProductReferenceService = blCommerceProductReferenceService;
  }

  public Converter<ReferenceData<ProductReferenceTypeEnum, REF_TARGET>, ProductReferenceData> getReferenceDataProductReferenceConverter() {
    return referenceDataProductReferenceConverter;
  }

  public void setReferenceDataProductReferenceConverter(
      Converter<ReferenceData<ProductReferenceTypeEnum, REF_TARGET>, ProductReferenceData> referenceDataProductReferenceConverter) {
    this.referenceDataProductReferenceConverter = referenceDataProductReferenceConverter;
  }

  public ConfigurablePopulator<REF_TARGET, ProductData, ProductOption> getReferenceProductConfiguredPopulator() {
    return referenceProductConfiguredPopulator;
  }

  public void setReferenceProductConfiguredPopulator(
      ConfigurablePopulator<REF_TARGET, ProductData, ProductOption> referenceProductConfiguredPopulator) {
    this.referenceProductConfiguredPopulator = referenceProductConfiguredPopulator;
  }
}

