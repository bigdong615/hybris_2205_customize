package com.bl.backoffice.actions;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.text.ParseException;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.commons.collections.map.HashedMap;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.facades.BlCreateShipmentFacade;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


/**
 * This action class is responsible to find the consignment and to create shipment for the same
 *
 * @author Keyur Patel
 *
 */

public class CreateShipmentAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
	private static final Logger LOG = Logger.getLogger(CreateShipmentAction.class);

	@Resource(name = "modelService")
	private ModelService modelService;

	@Resource(name = "blCreateShipmentFacade")
	private BlCreateShipmentFacade blCreateShipmentFacade;

	@Resource(name = "blShipmentCreationService")
	private DefaultBLShipmentCreationService blShipmentCreationService;

	protected static final String SOCKET_OUT_CONTEXT = "blCreatePackageShipmentContext";

	/**
	 * This method is responsible for fetch the consignment which are not in CANCELLED, CHECKED_INVALID,
	 * PAYMENT_NOT_AUTHORIZED and PAYMENT_DECLINED status
	 *
	 * @param actionContext
	 *           the action context
	 * @return the boolean
	 */
	@Override
	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel consignment = actionContext.getData();

		return (consignment != null && getBlShipmentCreationService().checkOrderStatus(consignment));
	}

	/**
	 * This method will fetch the action context data blCreatePackageShipmentContext and start shipment creation call
	 *
	 * @param actionContext
	 *           the action context
	 * @return the action result
	 */
	public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext)
	{
		final Map<String, Integer> sequenceMap = new HashedMap();
		final ConsignmentModel consignment = actionContext.getData();
		modelService.refresh(consignment);
		final List<PackagingInfoModel> packages = consignment.getPackaginginfos();
		final int packageCount = packages.size();
		for (int i = 0; i < packageCount; i++)
		{
			sequenceMap.put(packages.get(i).getPackageId(), i + 1);
		}

		for (final PackagingInfoModel packagingInfoModel : packages)
		{

			try
			{
				getBlCreateShipmentFacade().createBlShipmentPackages(packagingInfoModel, packageCount, sequenceMap);
			}
			catch (final ParseException exception)
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Exception occure at {}", exception.getMessage());
			}
		}
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
		return new ActionResult(BlintegrationConstants.SUCCESS);
	}

	/**
	 * @return the blShipmentCreationService
	 */
	public DefaultBLShipmentCreationService getBlShipmentCreationService()
	{
		return blShipmentCreationService;
	}

	/**
	 * @param blShipmentCreationService
	 *           the blShipmentCreationService to set
	 */
	public void setBlShipmentCreationService(final DefaultBLShipmentCreationService blShipmentCreationService)
	{
		this.blShipmentCreationService = blShipmentCreationService;
	}

	/**
	 * @return the blCreateShipmentFacade
	 */
	public BlCreateShipmentFacade getBlCreateShipmentFacade()
	{
		return blCreateShipmentFacade;
	}

	/**
	 * @param blCreateShipmentFacade
	 *           the blCreateShipmentFacade to set
	 */
	public void setBlCreateShipmentFacade(final BlCreateShipmentFacade blCreateShipmentFacade)
	{
		this.blCreateShipmentFacade = blCreateShipmentFacade;
	}

}
