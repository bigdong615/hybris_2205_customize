package com.bl.backoffice.actions.order;

import com.bl.core.order.impl.DefaultBlCalculationService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.Objects;
import javax.annotation.Resource;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * @author Manikandan
 * This action is created to calculate the avalara tax for cscockpit
 */
public class BlCalculateAvalaraTaxAction extends AbstractComponentWidgetAdapterAware implements CockpitAction<OrderModel, OrderModel> {

  private static final Logger LOG = Logger.getLogger(BlCalculateAvalaraTaxAction.class);

  private static final String SOCKET_OUT_CONTEXT = "blCustomAvalaraCalculationContext";
  private static final String SUCCESS = "success";

  @Resource(name = "calculationService")
  private CalculationService calculationService;


  @Resource(name ="defaultBlCalculationService")
  private DefaultBlCalculationService defaultBlCalculationService;


  @Resource(name = "modelService")
  private ModelService modelService;


  /**
   * This method created to calculate the avalara tax
   */
  public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext)
  {
    if(Objects.nonNull(actionContext) && Objects.nonNull(actionContext.getData())
        && actionContext.getData() instanceof OrderModel) {
      final OrderModel order = actionContext.getData();
      order.setCalculated(false);
      order.getEntries().forEach(abstractOrderEntryModel -> abstractOrderEntryModel.setCalculated(Boolean.FALSE));
      try {
        //getCalculationService().calculate(order);

        getDefaultBlCalculationService().recalculateOrderForTax(order);
        getModelService().save(order);
        getModelService().refresh(order);
        this.sendOutput(SOCKET_OUT_CONTEXT, order);
      } catch (final CalculationException e) {
        BlLogger.logFormattedMessage(LOG, Level.ERROR,
            "Error while Calculating Tax for order with nummber {}", order.getCode());
      }
    return new ActionResult(SUCCESS);
      }
    return new ActionResult(SUCCESS);
  }


  /**
   * This method created to perform the action
   */
  @Override
  public boolean canPerform(final ActionContext<OrderModel> actionContext)
  {
    return Objects.nonNull(actionContext.getData()) ;
  }


  public CalculationService getCalculationService() {
    return calculationService;
  }

  public void setCalculationService(CalculationService calculationService) {
    this.calculationService = calculationService;
  }


  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }


  public DefaultBlCalculationService getDefaultBlCalculationService() {
    return defaultBlCalculationService;
  }

  public void setDefaultBlCalculationService(
      DefaultBlCalculationService defaultBlCalculationService) {
    this.defaultBlCalculationService = defaultBlCalculationService;
  }

}
