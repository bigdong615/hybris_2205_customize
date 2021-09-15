package com.bl.core.model.interceptor;

import com.bl.core.model.BlInventoryCycleCountModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;

/**
 * Generating key for Inventory Cycle Count
 *
 * @author Namrata Lohar
 */
public class BlInventoryCycleCountPrepareInterceptor implements PrepareInterceptor<BlInventoryCycleCountModel> {

    private KeyGenerator keyGenerator;

  /**
   * Generate key for Inventory Cycle Count
   *
   * @param blInventoryCycleCountModel model
   * @param interceptorContext context
   * @throws InterceptorException
   */
    @Override
    public void onPrepare(final BlInventoryCycleCountModel blInventoryCycleCountModel, final InterceptorContext interceptorContext)
            throws InterceptorException {
        if (blInventoryCycleCountModel.getInventoryCycleCountCode() == null) {
            blInventoryCycleCountModel.setInventoryCycleCountCode(getKeyGenerator().generate().toString());
        }
    }

    public KeyGenerator getKeyGenerator() {
        return keyGenerator;
    }

    public void setKeyGenerator(final KeyGenerator keyGenerator) {
        this.keyGenerator = keyGenerator;
    }
}
