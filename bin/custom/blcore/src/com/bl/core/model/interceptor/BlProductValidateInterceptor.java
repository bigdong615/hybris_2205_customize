package com.bl.core.model.interceptor;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;
import java.util.Collection;
import org.apache.commons.collections4.CollectionUtils;

/**
 * This validator used to validate blproduct data before saving it.
 * @author Vijay Vishwakarma
 */
public class BlProductValidateInterceptor implements ValidateInterceptor<BlProductModel> {

  @Override
  public void onValidate(final BlProductModel blProductModel,
      final InterceptorContext interceptorContext)
      throws InterceptorException {
    if (blProductModel.getDiscontinued() != null && blProductModel.getDiscontinued()) {
      final Collection<BlSerialProductModel> blSerialProducts = blProductModel.getSerialProducts();
      boolean hasActiveSerialProduct = false;
      if (CollectionUtils.isNotEmpty(blSerialProducts)) {
        hasActiveSerialProduct = blSerialProducts.stream().anyMatch(blSerialProductModel ->
            blSerialProductModel.getSerialStatus() != null && blSerialProductModel.getSerialStatus()
                .equals(
                    SerialStatusEnum.ACTIVE)
        );
        if (hasActiveSerialProduct) {
          throw new InterceptorException(
              "Can't mark this product as discontinue as it contains some active serial product.");
        }
      }
    }
  }
}
