package com.bl.backoffice.actions;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.order.exceptions.CalculationException;

import javax.annotation.Resource;

import de.hybris.platform.promotions.PromotionsService;
import de.hybris.platform.promotions.jalo.PromotionsManager;
import de.hybris.platform.servicelayer.time.TimeService;
import de.hybris.platform.site.BaseSiteService;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.logging.BlLogger;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;

import java.util.ArrayList;
import java.util.Collection;
import static java.util.Collections.singletonList;


/**
 * ################# BL-982 ################# 
 * This class is created to recalculate the modify order
 *
 * @author Aditi Sharma
 *
 */
public class BlRecalculateOrderTotalsAction implements CockpitAction<OrderModel, OrderModel>
{
	private static final Logger LOG = Logger.getLogger(BlRecalculateOrderTotalsAction.class);

	@Resource
	private CalculationService calculationService;

	@Resource
	private PromotionsService promotionsService;

	@Resource
	private TimeService timeService;


	/**
	 * This method will recalculate the order
	 */
	@Override
	public ActionResult<OrderModel> perform(final ActionContext<OrderModel> abstractOrderModel)
	{
		BlLogger.logMessage(LOG, Level.DEBUG, "Recalculating order totals from Backoffice!");

		final OrderModel orderModel = abstractOrderModel.getData();
		try
		{
			getCalculationService().recalculate(orderModel);
			getPromotionsService().updatePromotions(singletonList(orderModel.getSite().getDefaultPromotionGroup()), orderModel, true, PromotionsManager.AutoApplyMode.APPLY_ALL,
							PromotionsManager.AutoApplyMode.APPLY_ALL, getTimeService().getCurrentTime());
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Recalculation done for order {}", orderModel.getCode());

		}
		catch (final CalculationException e)
		{
			BlLogger.logMessage(LOG, Level.ERROR, e.getMessage());
			return new ActionResult("success");
		}
		return new ActionResult("success");

	}

	/**
	 * This method will start order recalculation
	 *
	 * @param abstractOrderModel
	 * @return boolean
	 */
	public final boolean canPerform(final ActionContext<OrderModel> abstractOrderModel)
	{
		return true;
	}

	/**
	 * This method will be used to take confirmation for recalculation
	 *
	 * @param abstractOrderModel
	 * @return boolean
	 */
	public final boolean needsConfirmation(final ActionContext<OrderModel> abstractOrderModel)
	{
		return true;
	}

	/**
	 * This method will be used to get confirmation
	 *
	 * @param abstractOrderModel
	 * @return boolean
	 */
	public final String getConfirmationMessage(final ActionContext<OrderModel> abstractOrderModel)
	{
		return abstractOrderModel.getLabel("perform.recalculate");
	}

	public CalculationService getCalculationService() {
		return calculationService;
	}

	public void setCalculationService(CalculationService calculationService) {
		this.calculationService = calculationService;
	}

	public PromotionsService getPromotionsService() {
		return promotionsService;
	}

	public void setPromotionsService(PromotionsService promotionsService) {
		this.promotionsService = promotionsService;
	}

	public TimeService getTimeService() {
		return timeService;
	}

	public void setTimeService(TimeService timeService) {
		this.timeService = timeService;
	}

}
