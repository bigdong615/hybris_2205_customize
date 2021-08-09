package com.bl.core.dao.promotion;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ruleengineservices.order.dao.impl.DefaultExtendedOrderDao;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.List;
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
        for(final AbstractOrderModel abstractOrderModel :orders) {
          if(BooleanUtils.isTrue(abstractOrderModel.getIsExtendedOrder()) && null == abstractOrderModel.getExtendedOrderCopy() &&
              abstractOrderModel.getExtendOrderStatus().getCode().equalsIgnoreCase(
                  ExtendOrderStatusEnum.PROCESSING.getCode())) {
            result = abstractOrderModel;
          }
        }
      }
    }
    return result;
  }

}
