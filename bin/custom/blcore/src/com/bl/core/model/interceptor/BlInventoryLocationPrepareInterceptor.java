package com.bl.core.model.interceptor;

import com.bl.core.model.BLInventoryLocationModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;

public class BlInventoryLocationPrepareInterceptor implements
    PrepareInterceptor<BLInventoryLocationModel> {

  private KeyGenerator keyGenerator;

  @Override
  public void onPrepare(BLInventoryLocationModel blInventoryLocationModel,
      InterceptorContext interceptorContext) throws InterceptorException {

    if (blInventoryLocationModel.getInventoryLocationID() == null)
    {
      blInventoryLocationModel.setInventoryLocationID(Integer.parseInt(getKeyGenerator().generate().toString()));
    }

  }

  public KeyGenerator getKeyGenerator() {
    return keyGenerator;
  }

  public void setKeyGenerator(KeyGenerator keyGenerator) {
    this.keyGenerator = keyGenerator;
  }
}
