package com.bl.backoffice.actions;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.List;

import javax.annotation.Resource;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Messagebox.Button;

import com.bl.backoffice.consignment.service.BlConsignmentService;
import com.bl.backoffice.widget.controller.order.BlCustomCancelRefundConstants;
import com.bl.core.constants.BlCoreConstants;
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
	
	@Resource(name = "defaultBlConsignmentService")
	private BlConsignmentService defaultBlConsignmentService;

	private ConsignmentModel consignment;

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
		this.setConsignment(actionContext.getData());
		if (getDefaultBlConsignmentService().isMainItemScanRemaining(actionContext.getData()))
		{
			Messagebox.show(BlCoreConstants.MAIN_ITEM_SCAN_REMAINING_ERROR_MSG, BlCoreConstants.ERROR_TITLE, Messagebox.OK,
					Messagebox.ERROR);
			return new ActionResult<>(BlintegrationConstants.SUCCESS);
		}
		else if (getDefaultBlConsignmentService().isSubpartScanRemaining(actionContext.getData()))
		{
			final List<String> remainingScanSubpartNames = getDefaultBlConsignmentService()
					.getRemainingScanSubpartNames(this.getConsignment());
			Messagebox.show(BlCoreConstants.SUBPART_SCAN_REMAINING_ERROR_MSG_1
					+ String.join(BlCoreConstants.COMMA_SEPRATOR, remainingScanSubpartNames)
					+ BlCoreConstants.SUBPART_SCAN_REMAINING_ERROR_MSG_2, BlCoreConstants.ERROR_TITLE, new Button[]
			{ Button.NO, Button.YES }, BlCustomCancelRefundConstants.OMS_WIDGET_CANCELORDER_CONFIRM_ICON,
					this::processStatusChangeOnEvent);
			return new ActionResult<>(BlintegrationConstants.SUCCESS);
		}
		else
		{
			return processStatusChange(this.getConsignment());
		}
	}
	
	/**
	 * Process status change on consignment on click event.
	 *
	 * @param event
	 *           the event
	 */
	private void processStatusChangeOnEvent(final Event event)
	{
		if (Button.YES.event.equals(event.getName()))
		{
			processStatusChange(this.getConsignment());
		}
	}
	
	/**
	 * Process status change on consignment.
	 *
	 * @param consignmentModel the consignment model
	 * @return the action result
	 */
	private ActionResult<ConsignmentModel> processStatusChange(final ConsignmentModel consignmentModel)
	{
		if (OrderStatus.PAYMENT_CAPTURED.equals(consignmentModel.getOrder().getStatus()) && !ConsignmentStatus.BL_SHIPPED.equals(consignmentModel.getStatus()))
		{
			consignmentModel.getConsignmentEntries()
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

			consignmentModel.setStatus(ConsignmentStatus.BL_SHIPPED);
			getModelService().save(consignmentModel);
			getModelService().refresh(consignmentModel);
			AbstractOrderModel order = consignmentModel.getOrder();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Status updated to {} for consignment {}", consignmentModel.getStatus(),
					consignmentModel.getCode());

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
		else if(ConsignmentStatus.BL_SHIPPED.equals(consignmentModel.getStatus()))
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
		this.sendOutput(SOCKET_OUT_CONTEXT, consignmentModel);
		return new ActionResult<>(BlintegrationConstants.SUCCESS);
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

	/**
	 * @return the defaultBlConsignmentService
	 */
	public BlConsignmentService getDefaultBlConsignmentService()
	{
		return defaultBlConsignmentService;
	}

	/**
	 * @param defaultBlConsignmentService the defaultBlConsignmentService to set
	 */
	public void setDefaultBlConsignmentService(BlConsignmentService defaultBlConsignmentService)
	{
		this.defaultBlConsignmentService = defaultBlConsignmentService;
	}

	/**
	 * @return the consignment
	 */
	public ConsignmentModel getConsignment()
	{
		return consignment;
	}

	/**
	 * @param consignment the consignment to set
	 */
	public void setConsignment(ConsignmentModel consignment)
	{
		this.consignment = consignment;
	}

}
