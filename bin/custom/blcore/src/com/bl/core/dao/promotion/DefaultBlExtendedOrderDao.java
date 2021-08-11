package com.bl.core.dao.promotion;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ruleengineservices.order.dao.impl.DefaultExtendedOrderDao;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is created to override the OOB method to get order model while applying promotion
 * @author Manikandan
 */
public class DefaultBlExtendedOrderDao extends DefaultExtendedOrderDao {

  private static final Logger LOG = Logger.getLogger(DefaultBlExtendedOrderDao.class);

  /**
   * This method is overriden to get the extend order , when promotion is applied from extend order
   */
  @Override
  public AbstractOrderModel findOrderByCode(final String code) {
    ServicesUtil.validateParameterNotNull(code, "Code must not be null");
    final AbstractOrderModel example = new AbstractOrderModel();
    example.setCode(code);
    final List<AbstractOrderModel> orders = this.getFlexibleSearchService().getModelsByExample(example);
    AbstractOrderModel result = null;
    if (orders.isEmpty()) {
      BlLogger.logMessage(LOG , Level.ERROR , "Cannot find order/cart with code: " + code);
    } else {
      if (orders.size() == 1) {
        result = orders.get(0);
      } else {
        result = getExtendOrderFromOrderModel(orders);
      }
    }
    return result;
  }


  private AbstractOrderModel getExtendOrderFromOrderModel(final List<AbstractOrderModel> orders) {
    AbstractOrderModel result = null;
    for(final AbstractOrderModel abstractOrderModel :orders) {
      if(BooleanUtils.isTrue(abstractOrderModel.getIsExtendedOrder()) && null == abstractOrderModel.getExtendedOrderCopy() &&
          abstractOrderModel.getExtendOrderStatus().getCode().equalsIgnoreCase(ExtendOrderStatusEnum.PROCESSING.getCode())) {
        result = abstractOrderModel;
      }
    }
    if(null == result) {
      result = returnOrderModelIfResultIsNull(orders);
    }
    return result;
  }


  /**
   * This method created to get order model from list of order model
   * @param orders list of orders
   * @return AbstractOrderModel
   */
  private AbstractOrderModel returnOrderModelIfResultIsNull(final List<AbstractOrderModel> orders) {
    AbstractOrderModel result = null ;
    AbstractOrderModel abstractOrderModel = null;

    for (final AbstractOrderModel orderModel :orders) {
      if (BooleanUtils.isFalse(orderModel.getIsExtendedOrder()) && CollectionUtils.isNotEmpty(orderModel.getExtendedOrderCopyList())) {
        abstractOrderModel = orderModel;
      }
    }

    if(null != abstractOrderModel && CollectionUtils.isNotEmpty(abstractOrderModel.getExtendedOrderCopyList())) {
      final List<AbstractOrderModel> orderModelList = abstractOrderModel.getExtendedOrderCopyList();

      final int extendOrderSize = orderModelList.size();
      for (AbstractOrderModel extendOrder : abstractOrderModel.getExtendedOrderCopyList()) {
        if (BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) && extendOrder
            .getExtendOrderStatus().getCode()
            .equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())
            && orderModelList.get(extendOrderSize - 1).getPk()
            .equals(extendOrder.getPk())) {
          result = extendOrder;
        }
      }
    }

    return result;
  }

}
