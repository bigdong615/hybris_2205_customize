package com.bl.core.interceptor;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.RemoveInterceptor;
import org.apache.commons.lang.BooleanUtils;

public class BlProductRemoveInterceptor implements RemoveInterceptor<ProductModel> {
    @Override
    public void onRemove(ProductModel model, InterceptorContext interceptorContext) throws InterceptorException {
        if(BooleanUtils.isNotTrue(Boolean.valueOf(model.getCode())))
        {
            throw new InterceptorException(BlCoreConstants.PRODUCT_INTERCEPTOR_REMOVE_MSG);
        }
    }
}
