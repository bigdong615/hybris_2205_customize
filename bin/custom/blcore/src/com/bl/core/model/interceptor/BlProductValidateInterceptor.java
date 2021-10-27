package com.bl.core.model.interceptor;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;

/**
 * This validator used to validate blproduct data before saving it.
 * @author Vijay Vishwakarma
 */
public class BlProductValidateInterceptor implements ValidateInterceptor<BlProductModel> {

  @Override
  public void onValidate(final BlProductModel blProductModel,
      final InterceptorContext interceptorContext)
      throws InterceptorException {
    if (BooleanUtils.isTrue(blProductModel.getDiscontinued())) {
      final Collection<BlSerialProductModel> blSerialProducts = blProductModel.getSerialProducts();
      boolean hasActiveSerialProduct =  CollectionUtils.isEmpty(blSerialProducts);
      if (CollectionUtils.isNotEmpty(blSerialProducts)) {
        hasActiveSerialProduct = blSerialProducts.stream().anyMatch(blSerialProductModel ->
            blSerialProductModel.getSerialStatus() != null && blSerialProductModel.getSerialStatus()
                .equals(
                    SerialStatusEnum.ACTIVE)
        );}
        if (hasActiveSerialProduct) {
          throw new InterceptorException(
              "Can't mark this product as discontinue");
        }

    }
    checkReferenceProduct(blProductModel, interceptorContext);
  }
  /**
   * check bundle Reference Product
   *
   * @param blProductModel
   * @param interceptorContext
   */
  private void checkReferenceProduct(final BlProductModel blProductModel,final InterceptorContext interceptorContext) throws InterceptorException {
    if(blProductModel.isBundleProduct() && CollectionUtils.isNotEmpty(blProductModel.getProductReferences())){
      final List<ProductReferenceModel> productReferences = new ArrayList<>(blProductModel.getProductReferences());
      final List<ProductReferenceModel> bundleProductReferences = productReferences.stream().filter(
          productReferenceModel -> ProductReferenceTypeEnum.CONSISTS_OF
              .equals(productReferenceModel.getReferenceType())).collect(
          Collectors.toList());

      if(bundleProductReferences.size() < 2){
        throw new InterceptorException("Can't mark this product as bundle");
      }
    }
  }
  /**
   * Check product References size
   *
   * @param productReferences
   * @return true if productReferences is less than 2
   */
  private boolean checkSizeOfReferences(final Collection<ProductReferenceModel> productReferences){
    return productReferences.size()< 2 ? Boolean.TRUE : Boolean.FALSE;
    }
}

