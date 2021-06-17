package com.bl.facades.productreference.service;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.facades.productreference.BlCommerceProductReferenceService;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.commerceservices.product.data.ReferenceData;
import de.hybris.platform.commerceservices.product.impl.DefaultCommerceProductReferenceService;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.ArrayList;
import java.util.List;

public class DefaultBlCommerceProductReferenceService extends
    DefaultCommerceProductReferenceService implements
    BlCommerceProductReferenceService<ProductReferenceTypeEnum, ProductModel> {

  @Override
  public List<ReferenceData<ProductReferenceTypeEnum, ProductModel>> getProductReferencesForCode(
      final ProductModel currentProduct, final Integer limit) {
    validateParameterNotNull(currentProduct, "Parameter code must not be null");

    final List<ReferenceData<ProductReferenceTypeEnum, ProductModel>> result = new ArrayList<>();

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
    return (List<ProductReferenceModel>) getProductReferencesForProduct(product);
  }

}
