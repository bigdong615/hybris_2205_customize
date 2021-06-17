package com.bl.facades.productreference.service;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.facades.productreference.BlCommerceProductReferenceService;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.commerceservices.product.data.ReferenceData;
import de.hybris.platform.commerceservices.product.impl.DefaultCommerceProductReferenceService;
import de.hybris.platform.commerceservices.strategies.ProductReferenceTargetStrategy;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.variants.model.VariantProductModel;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;

public class DefaultBlCommerceProductReferenceService implements
    BlCommerceProductReferenceService<ProductReferenceTypeEnum, ProductModel> {

  private static final Logger LOG = Logger.getLogger(DefaultCommerceProductReferenceService.class);

  private ModelService modelService;
  private Map<ProductReferenceTypeEnum, ProductReferenceTargetStrategy> productReferenceTargetStrategies;
  private ProductReferenceTargetStrategy defaultProductReferenceTargetStrategy;

  @Override
  public List<ReferenceData<ProductReferenceTypeEnum, ProductModel>> getProductReferencesForCode(
      ProductModel currentProduct, Integer limit) {
    validateParameterNotNull(currentProduct, "Parameter code must not be null");

    final List<ReferenceData<ProductReferenceTypeEnum, ProductModel>> result = new ArrayList<ReferenceData<ProductReferenceTypeEnum, ProductModel>>();

    final List<ProductReferenceModel> references = getAllActiveProductReferencesFromSourceOfType(
        currentProduct);
    if (references != null && !references.isEmpty()) {
      for (final ProductReferenceModel reference : references) {
        final ProductModel targetProduct = resolveTarget(currentProduct, reference);

        final ReferenceData<ProductReferenceTypeEnum, ProductModel> referenceData = createReferenceData();
        referenceData.setTarget(targetProduct);
        referenceData.setDescription(reference.getDescription());
        referenceData.setQuantity(reference.getQuantity());
        referenceData.setReferenceType(reference.getReferenceType());
        result.add(referenceData);

        // Check the limit
        if (limit != null && result.size() >= limit.intValue()) {
          break;
        }
      }
    }

    return result;
  }


  protected List<ProductReferenceModel> getAllActiveProductReferencesFromSourceOfType(
      final ProductModel product) {
    final List<ProductReferenceModel> allReferences = (List<ProductReferenceModel>) getProductReferencesForProduct(
        product);
    return allReferences;
  }


  protected Collection<ProductReferenceModel> getProductReferencesForProduct(
      final ProductModel product) {
    return (Collection<ProductReferenceModel>) getProductAttribute(product,
        ProductModel.PRODUCTREFERENCES);
  }

  protected ProductModel resolveTarget(final ProductModel sourceProduct,
      final ProductReferenceModel reference) {
    // Look for a strategy for the specific type of reference
    final Map<ProductReferenceTypeEnum, ProductReferenceTargetStrategy> strategiesMap = getProductReferenceTargetStrategies();
    if (strategiesMap != null) {
      final ProductReferenceTargetStrategy strategy = strategiesMap
          .get(reference.getReferenceType());
      if (strategy != null) {
        return resolveTarget(sourceProduct, reference, strategy);
      }
    }

    // Fallback to the default strategy
    return resolveTarget(sourceProduct, reference, getDefaultProductReferenceTargetStrategy());
  }

  protected ProductModel resolveTarget(final ProductModel sourceProduct,
      final ProductReferenceModel reference,
      final ProductReferenceTargetStrategy strategy) {
    final ProductModel target = strategy.getTarget(sourceProduct, reference);
    if (target != null) {
      return target;
    }
    return reference.getTarget();
  }

  /**
   * Get an attribute value from a product. If the attribute value is null and the product is a
   * variant then the same attribute will be requested from the base product.
   *
   * @param product the product
   * @param attribute the name of the attribute to lookup
   * @return the value of the attribute
   */
  protected Object getProductAttribute(final ProductModel product, final String attribute) {
    final Object value = getModelService().getAttributeValue(product, attribute);
    if (product instanceof VariantProductModel
        && (value == null || (value instanceof Collection && ((Collection) value).isEmpty()))) {
      final ProductModel baseProduct = ((VariantProductModel) product).getBaseProduct();
      if (baseProduct != null) {
        return getProductAttribute(baseProduct, attribute);
      }
    }
    return value;
  }

  protected ReferenceData<ProductReferenceTypeEnum, ProductModel> createReferenceData() {
    return new ReferenceData<ProductReferenceTypeEnum, ProductModel>();
  }

  protected ModelService getModelService() {
    return modelService;
  }

  @Required
  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }

  protected Map<ProductReferenceTypeEnum, ProductReferenceTargetStrategy> getProductReferenceTargetStrategies() {
    return productReferenceTargetStrategies;
  }

  @Required
  public void setProductReferenceTargetStrategies(
      final Map<ProductReferenceTypeEnum, ProductReferenceTargetStrategy> productReferenceTargetStrategies) {
    this.productReferenceTargetStrategies = productReferenceTargetStrategies;
  }

  protected ProductReferenceTargetStrategy getDefaultProductReferenceTargetStrategy() {
    return defaultProductReferenceTargetStrategy;
  }

  @Required
  public void setDefaultProductReferenceTargetStrategy(
      final ProductReferenceTargetStrategy defaultProductReferenceTargetStrategy) {
    this.defaultProductReferenceTargetStrategy = defaultProductReferenceTargetStrategy;
  }

}
