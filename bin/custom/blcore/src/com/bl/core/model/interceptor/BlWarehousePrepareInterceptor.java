package com.bl.core.model.interceptor;

import com.bl.core.jalo.BlSerialProduct;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.stock.BlStockService;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ItemModelContextImpl;
import java.util.Objects;

/**
 * It is used to intercept the model and modify the attributes before saving the data
 * @author Moumita
 */
public class BlWarehousePrepareInterceptor implements PrepareInterceptor<WarehouseModel>
{
  private BlStockService blStockService;

  /**
   * On prepare.
   *
   * @param warehouseModel
   *           the warehouse model
   * @param interceptorContext
   *           the interceptor context
   * @throws InterceptorException
   *            the interceptor exception
   */
  @Override
  public void onPrepare(final WarehouseModel warehouseModel, final InterceptorContext interceptorContext)
      throws InterceptorException {
    final Object initialValue = getInitialValue(warehouseModel, WarehouseModel.BLOCKINVENTORY);
    if(Objects.nonNull(initialValue) && interceptorContext.isModified(warehouseModel, WarehouseModel.BLOCKINVENTORY)) {
      getBlStockService().reserveProductsBelongToWHForSpecifiedDate(warehouseModel);
    }
  }

  /**
   * It gets the initial value
   * @param warehouseModel the warehouse model
   * @param status the status
   * @return object
   */
  private Object getInitialValue(final WarehouseModel warehouseModel, final String status) {
    final ItemModelContextImpl itemModelCtx = (ItemModelContextImpl) warehouseModel
        .getItemModelContext();
    return itemModelCtx.exists() ? itemModelCtx.getOriginalValue(status) : null;
  }

  public BlStockService getBlStockService() {
    return blStockService;
  }

  public void setBlStockService(BlStockService blStockService) {
    this.blStockService = blStockService;
  }
}
