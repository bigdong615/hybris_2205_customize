package com.bl.core.model.interceptor;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.google.common.collect.Lists;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;
import java.util.Collection;
import java.util.List;
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
  private void checkReferenceProduct(final BlProductModel blProductModel,
      final InterceptorContext interceptorContext) throws InterceptorException {
    if(blProductModel.isBundleProduct()){
      final List<ProductReferenceModel> productReferences = Lists.newArrayList(CollectionUtils.emptyIfNull(blProductModel
          .getProductReferences()));
      productReferences.removeIf(refer -> !ProductReferenceTypeEnum.CONSISTS_OF.equals(refer.getReferenceType()));
      if(CollectionUtils.isEmpty(productReferences) || checkSizeOfReferences(productReferences)){
        throw new InterceptorException("Can't mark this product as discontinue");
      }
    }
  }
  /**
   * Check product References size
   *
   * @param productReferences
   * @return true if productReferences is less than 2
   */
  private boolean checkSizeOfReferences(final List<ProductReferenceModel> productReferences){
    return productReferences.size()< 2 ? Boolean.TRUE : Boolean.FALSE;
    }
}

