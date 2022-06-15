package com.bl.backoffice.actions;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;

import javax.annotation.Resource;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zul.Messagebox;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


/**
 * ############# BL-2226 ############# This action class is responsible to change the consignment status to BL_SHIPPED
 * And if all the consignment is in BL_SHIPPED status, mark the order status as SHIPPED
 *
 * @author Aditi Sharma
 *
 */

public class ChangeShipmentStatusAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
	private static final Logger LOG = Logger.getLogger(ChangeShipmentStatusAction.class);
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	@Resource(name = "modelService")
	private ModelService modelService;

	@Resource(name = "blShipmentCreationService")
	private DefaultBLShipmentCreationService blShipmentCreationService;

	protected static final String SOCKET_OUT_CONTEXT = "blChangeShipmentStatusContext";

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
	 * This method will fetch the action context data blChangeShipmentStatusContext and start shipment status change call
	 *
	 * @param actionContext
	 *           the action context
	 * @return the action result
	 */
	public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel consignment = actionContext.getData();
		if (OrderStatus.PAYMENT_CAPTURED.equals(consignment.getOrder().getStatus()) && !ConsignmentStatus.BL_SHIPPED.equals(consignment.getStatus()))
		{
			consignment.getConsignmentEntries()
					.forEach(consignmentEntry -> consignmentEntry.getSerialProducts().forEach(serialProduct -> {
						if (serialProduct instanceof BlSerialProductModel)
						{
							final BlSerialProductModel serialProductModel = ((BlSerialProductModel) serialProduct);
							serialProductModel.setSerialStatus(SerialStatusEnum.SHIPPED);
							getModelService().save(serialProductModel);
							getModelService().refresh(serialProductModel);
							BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Status updated to {} for serial {}",
									serialProductModel.getSerialStatus(), serialProductModel.getCode());
						}
					}));

			consignment.setStatus(ConsignmentStatus.BL_SHIPPED);
			getModelService().save(consignment);
			getModelService().refresh(consignment);
			AbstractOrderModel order = consignment.getOrder();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Status updated to {} for consignment {}", consignment.getStatus(),
					consignment.getCode());

			if (order.getConsignments().stream()
					.allMatch(con -> ConsignmentStatus.BL_SHIPPED.equals(con.getStatus())))
			{
				order.setStatus(OrderStatus.SHIPPED);
				getModelService().save(order);
				getModelService().refresh(order);
			}

			this.sendOutput(OUT_CONFIRM, COMPLETE);
			Messagebox.show("Shipment changed to BL_SHIPPED status", BlintegrationConstants.POPUP_TEXT, Messagebox.OK, "icon");			
		}
		else if(ConsignmentStatus.BL_SHIPPED.equals(consignment.getStatus()))
		{
			this.sendOutput(OUT_CONFIRM, COMPLETE);
			Messagebox.show("Shipment is already shipped", BlintegrationConstants.POPUP_TEXT,
					Messagebox.OK, "icon");
		}
		else
		{
			this.sendOutput(OUT_CONFIRM, COMPLETE);
			Messagebox.show("Shipment is not changed to BL_SHIPPED status as Payment is not Captured for the Order.", BlintegrationConstants.POPUP_TEXT,
					Messagebox.OK, "icon");
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
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

}
