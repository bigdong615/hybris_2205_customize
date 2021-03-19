package com.bl.core.model.interceptor;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;
import org.apache.commons.lang.StringUtils;

/**
 * This class is for setting the auto generated product Id on BlProduct
 * when it is created and has no productID associated to it
 *
 * @author Ritika
 */
public class BlProductPrepareInterceptor implements PrepareInterceptor<BlProductModel> {

  private KeyGenerator keyGenerator;

  @Override
  public void onPrepare(BlProductModel blProductModel, InterceptorContext interceptorContext)
      throws InterceptorException {
    setBlProductId(blProductModel,interceptorContext);
  }

  /**
   * Set auto generated productId on BlProduct
   * @param blProductModel
   * @param interceptorContext
   */
  private void setBlProductId(BlProductModel blProductModel, InterceptorContext interceptorContext) {
    if( interceptorContext.isNew(blProductModel) && StringUtils.isBlank(blProductModel.getProductId())){
      blProductModel.setProductId(getKeyGenerator().generate().toString());
    }
  }

  public KeyGenerator getKeyGenerator() {
    return keyGenerator;
  }

  public void setKeyGenerator(KeyGenerator keyGenerator) {
    this.keyGenerator = keyGenerator;
  }
}
