package com.bl.core.dao.promotion;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ruleengineservices.order.dao.impl.DefaultExtendedOrderDao;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.List;
import org.apache.commons.lang.BooleanUtils;

public class DefaultBlExtendedOrderDao extends DefaultExtendedOrderDao {

  @Override
  public AbstractOrderModel findOrderByCode(String code) {
    ServicesUtil.validateParameterNotNull(code, "Code must not be null");
    AbstractOrderModel example = new AbstractOrderModel();
    example.setCode(code);
    List<AbstractOrderModel> orders = this.getFlexibleSearchService().getModelsByExample(example);
    AbstractOrderModel result = null;
    if (orders.isEmpty()) {
      throw new ModelNotFoundException("Cannot find order/cart with code: " + code);
    } else {
      if (orders.size() == 1) {
        result = orders.get(0);
      } else {
        for(AbstractOrderModel abstractOrderModel :orders) {
          if(BooleanUtils.isTrue(abstractOrderModel.getIsExtendedOrder()) && null == abstractOrderModel.getExtendedOrderCopy() ) { // Add check for status
            result = abstractOrderModel;
          }
        }
      }
    }
    return result;
  }

  protected boolean isOrderModelOriginal(AbstractOrderModel abstractOrderModel) {
    boolean result;
    if (abstractOrderModel instanceof OrderModel) {
      OrderModel orderModel = (OrderModel)abstractOrderModel;
      result = orderModel.getVersionID() == null;
    } else {
      result = false;
    }

    return result;
  }
}
