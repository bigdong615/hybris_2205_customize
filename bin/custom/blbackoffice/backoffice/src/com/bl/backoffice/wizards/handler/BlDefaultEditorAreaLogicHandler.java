package com.bl.backoffice.wizards.handler;

import com.bl.core.services.tax.DefaultBlExternalTaxesService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.dataaccess.facades.object.exceptions.ObjectSavingException;
import com.hybris.cockpitng.engine.WidgetInstanceManager;
import com.hybris.cockpitng.widgets.baseeditorarea.DefaultEditorAreaLogicHandler;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.jalo.order.OrderEntry;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.refund.OrderRefundEntry;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * @author Manikandan
 * This class created to call avalara while saving the order from cscockpit
 */
public class BlDefaultEditorAreaLogicHandler extends DefaultEditorAreaLogicHandler {

  private static final Logger LOG = Logger.getLogger(BlDefaultEditorAreaLogicHandler.class);

  private DefaultBlExternalTaxesService defaultBlExternalTaxesService;
  private CalculationService calculationService;

  /**
   * This method call when order is saving
   */
  @Override
  public Object performSave(WidgetInstanceManager widgetInstanceManager, Object currentObject) throws ObjectSavingException {
    if (currentObject instanceof OrderModel) {
      OrderModel orderModel = (OrderModel) currentObject;
        orderModel.setCalculated(false);
        orderModel.getEntries().forEach(abstractOrderEntryModel -> abstractOrderEntryModel.setCalculated(Boolean.FALSE));
      try {
        getCalculationService().calculate(orderModel);
      } catch (CalculationException e) {
        BlLogger.logMessage(LOG , Level.ERROR , "Error while BlDefaultEditorAreaLogicHandler" , e);
      }
      return super.performSave(widgetInstanceManager, orderModel);
     }
    return super.performSave(widgetInstanceManager , currentObject);
  }


  public DefaultBlExternalTaxesService getDefaultBlExternalTaxesService() {
    return defaultBlExternalTaxesService;
  }

  public void setDefaultBlExternalTaxesService(
      DefaultBlExternalTaxesService defaultBlExternalTaxesService) {
    this.defaultBlExternalTaxesService = defaultBlExternalTaxesService;
  }

  public CalculationService getCalculationService() {
    return calculationService;
  }

  public void setCalculationService(CalculationService calculationService) {
    this.calculationService = calculationService;
  }
}
