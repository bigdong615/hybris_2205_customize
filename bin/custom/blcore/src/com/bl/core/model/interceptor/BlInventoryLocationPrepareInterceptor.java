package com.bl.core.model.interceptor;

import com.bl.core.model.BlInventoryLocationModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;

public class BlInventoryLocationPrepareInterceptor implements
    PrepareInterceptor<BlInventoryLocationModel> {

  private KeyGenerator keyGenerator;

  @Override
  public void onPrepare(BlInventoryLocationModel blInventoryLocationModel,
      InterceptorContext interceptorContext) throws InterceptorException {

    if (blInventoryLocationModel.getInventoryLocationID() == null)
    {
      blInventoryLocationModel.setInventoryLocationID(getKeyGenerator().generate().toString());
    }

  }

  public KeyGenerator getKeyGenerator() {
    return keyGenerator;
  }

  public void setKeyGenerator(final KeyGenerator keyGenerator) {
    this.keyGenerator = keyGenerator;
  }
}
