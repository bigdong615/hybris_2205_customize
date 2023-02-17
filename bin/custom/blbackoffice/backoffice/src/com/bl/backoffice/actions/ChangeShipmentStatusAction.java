package com.bl.backoffice.actions;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Messagebox.Button;

import com.bl.backoffice.consignment.service.BlConsignmentService;
import com.bl.backoffice.widget.controller.order.BlCustomCancelRefundConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.NumberingSystemEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
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
		final ConsignmentModel consignmentmodel = actionContext.getData();

		return (consignmentmodel != null && getBlShipmentCreationService().checkOrderAndConsignmentStatus(consignmentmodel));
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
		if(isUsedOrderOnly(consignmentModel.getOrder())){
      	consignmentModel.setStatus(ConsignmentStatus.BL_SHIPPED);
      	getModelService().save(consignmentModel);
      	AbstractOrderModel order = consignmentModel.getOrder();
      	BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Status updated to {} for consignment {}", consignmentModel.getStatus(),
      			consignmentModel.getCode());
      	if (order.getConsignments().stream()
      			.allMatch(con -> ConsignmentStatus.BL_SHIPPED.equals(con.getStatus())))
      	{
      		order.setStatus(OrderStatus.SOLD_SHIPPED);
      		getModelService().save(order);
      	}
      	this.sendOutput(OUT_CONFIRM, COMPLETE);
      	Messagebox.show("Shipment changed to BL_SHIPPED status", BlintegrationConstants.POPUP_TEXT, Messagebox.OK, "icon");	
		}
		else if(OrderStatus.RECEIVED_MANUAL_REVIEW.equals(consignmentModel.getOrder().getStatus()) || isProductAllocationRemaining(consignmentModel.getOrder()))
		{
			Messagebox.show("Order not mark to BL Shipped due to order being in manual review status (non-allocated items).", BlintegrationConstants.ERROR_TEXT,
					Messagebox.OK, "icon");
		}		
		else if (OrderStatus.PAYMENT_CAPTURED.equals(consignmentModel.getOrder().getStatus()) && !ConsignmentStatus.BL_SHIPPED.equals(consignmentModel.getStatus()))
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
	
	public boolean isUsedOrderOnly(final AbstractOrderModel order)
	{
		return BooleanUtils.isFalse(order.getIsRentalOrder()) && BooleanUtils.isFalse(order.isGiftCardOrder()) 
				&& BooleanUtils.isFalse(order.getIsRetailGearOrder()) && BooleanUtils.isFalse(order.getIsReplacementOrder());
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
	
	/**
	 * Checks if is product allocation remaining.
	 *
	 * @param order the order
	 * @return true, if is product allocation remaining
	 */
	private boolean isProductAllocationRemaining(final AbstractOrderModel order)
	{
		if (Objects.isNull(order))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"ChangeShipmentStatusAction :: isProductAllocationRemaining :: Order is null", StringUtils.EMPTY);
			return true;
		}
		if(BooleanUtils.isTrue(order.isGiftCardOrder()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"ChangeShipmentStatusAction :: isProductAllocationRemaining :: Order : {} is a Gift Card Order",
					order.getCode());
			return false;
		}
		if(BooleanUtils.isTrue(order.getIsRetailGearOrder()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"ChangeShipmentStatusAction :: isProductAllocationRemaining :: Order : {} is a Retail Gear Order",
					order.getCode());
			return false;
		}
		if (CollectionUtils.isEmpty(order.getEntries()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"ChangeShipmentStatusAction :: isProductAllocationRemaining :: Order Entries is empty on Order : {}",
					order.getCode());
			return true;
		}
		if(isUsedGearOrder(order))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"ChangeShipmentStatusAction :: isProductAllocationRemaining :: Order : {} is a Used Gear Order",
					order.getCode());
			return checkAllocationForUsedGearOrder(order);
		}
		final Map<String, Integer> orderEntryProductQtyMap = Maps.newHashMap();
		final Map<String, Integer> allocatedProductQtyMap = Maps.newHashMap();
		final List<BlSerialProductModel> mainItemsList = Lists.newArrayList();

		getSkuCodeWithQtyMapFromOrderEntry(order, orderEntryProductQtyMap);
		order.getConsignments().forEach(
				consignmentModel -> mainItemsList.addAll(getDefaultBlConsignmentService().getMainItemsListFromConsignment(consignmentModel)));
		getAllocatedProductAndQtyMap(allocatedProductQtyMap, mainItemsList);
		for (final Map.Entry<String, Integer> productAndQty : orderEntryProductQtyMap.entrySet())
		{
			if (BooleanUtils.isFalse(allocatedProductQtyMap.containsKey(productAndQty.getKey())))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"ChangeShipmentStatusAction :: isProductAllocationRemaining :: SKU : {} is not allocated on order : {} ",
						productAndQty.getKey(), order.getCode());
				return true;
			}
			if (BooleanUtils.isFalse(allocatedProductQtyMap.get(productAndQty.getKey()).compareTo(productAndQty.getValue()) == 0))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"ChangeShipmentStatusAction :: isProductAllocationRemaining :: SKU : {} is not allocated with specified qty on order entry : {}",
						productAndQty.getKey(), productAndQty.getValue());
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Check allocation for used gear order.
	 *
	 * @param order
	 *           the order
	 * @return true, if successful
	 */
	private boolean checkAllocationForUsedGearOrder(final AbstractOrderModel order)
	{
		final Map<String, Integer> orderEntryProductQtyMap = Maps.newHashMap();
		final Map<String, Integer> allocatedProductQtyMap = Maps.newHashMap();
		final List<BlSerialProductModel> mainItemsList = Lists.newArrayList();

		order.getEntries().forEach(orderEntry -> {
			final String serialCode = orderEntry.getProduct().getCode();
			if (orderEntryProductQtyMap.containsKey(serialCode))
			{
				final Integer qtyCount = orderEntryProductQtyMap.get(serialCode) + BlCoreConstants.INT_ONE;
				orderEntryProductQtyMap.put(serialCode, qtyCount);
			}
			else
			{
				orderEntryProductQtyMap.put(serialCode, orderEntry.getQuantity().intValue());
			}
		});
		order.getConsignments().forEach(consignmentModel -> mainItemsList
				.addAll(getDefaultBlConsignmentService().getMainItemsListFromConsignment(consignmentModel)));
		mainItemsList.forEach(mainItem -> {
			if (allocatedProductQtyMap.containsKey(mainItem.getCode()))
			{
				final Integer qtyCount = allocatedProductQtyMap.get(mainItem.getCode()) + BlCoreConstants.INT_ONE;
				allocatedProductQtyMap.put(mainItem.getCode(), qtyCount);
			}
			else
			{
				allocatedProductQtyMap.put(mainItem.getCode(), BlCoreConstants.INT_ONE);
			}
		});
		for (final Map.Entry<String, Integer> productAndQty : orderEntryProductQtyMap.entrySet())
		{
			if (BooleanUtils.isFalse(allocatedProductQtyMap.containsKey(productAndQty.getKey())))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"ChangeShipmentStatusAction :: checkAllocationForUsedGearOrder :: SERIAL : {} is not allocated on order : {} ",
						productAndQty.getKey(), order.getCode());
				return true;
			}
			if (BooleanUtils.isFalse(allocatedProductQtyMap.get(productAndQty.getKey()).compareTo(productAndQty.getValue()) == 0))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"ChangeShipmentStatusAction :: checkAllocationForUsedGearOrder :: SERIAL : {} is not allocated with specified qty on order entry : {}",
						productAndQty.getKey(), productAndQty.getValue());
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Checks if is used gear order.
	 *
	 * @param order the order
	 * @return true, if is used gear order
	 */
	private boolean isUsedGearOrder(final AbstractOrderModel order)
	{
		return BooleanUtils.isFalse(order.getIsRentalOrder()) && BooleanUtils.isFalse(order.isGiftCardOrder()) 
				&& BooleanUtils.isFalse(order.getIsRetailGearOrder()) && BooleanUtils.isFalse(order.getIsReplacementOrder());
	}

	/**
	 * Gets the allocated product and qty map.
	 *
	 * @param allocatedProductQtyMap
	 *           the allocated product qty map
	 * @param mainItemsList
	 *           the main items list
	 * @return the allocated product and qty map
	 */
	private void getAllocatedProductAndQtyMap(final Map<String, Integer> allocatedProductQtyMap,
			final List<BlSerialProductModel> mainItemsList)
	{
		mainItemsList.forEach(item -> {
			if (Objects.isNull(item.getBlProduct()))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
						"ChangeShipmentStatusAction :: isProductAllocationRemaining :: SKU not assigned on Serial : {}",
						item.getCode());
			}
			else
			{
				final String skuCode = item.getBlProduct().getCode();
				if (allocatedProductQtyMap.containsKey(skuCode))
				{
					final Integer qtyCount = allocatedProductQtyMap.get(skuCode) + BlCoreConstants.INT_ONE;
					allocatedProductQtyMap.put(skuCode, qtyCount);
				}
				else
				{
					allocatedProductQtyMap.put(skuCode, BlCoreConstants.INT_ONE);
				}
			}
		});
	}

	/**
	 * Gets the sku code with qty map from order entry.
	 *
	 * @param order
	 *           the order
	 * @param orderEntryProductQtyMap
	 *           the order entry product qty map
	 * @return the sku code with qty map from order entry
	 */
	private void getSkuCodeWithQtyMapFromOrderEntry(final AbstractOrderModel order,
			final Map<String, Integer> orderEntryProductQtyMap)
	{
		order.getEntries().forEach(orderEntry -> {
			if (checkEligiblityForEntry(orderEntry))
			{
				final String skuCode = orderEntry.getProduct().getCode();
				if (orderEntryProductQtyMap.containsKey(skuCode))
				{
					final Integer qtyCount = orderEntryProductQtyMap.get(skuCode) + BlCoreConstants.INT_ONE;
					orderEntryProductQtyMap.put(skuCode, qtyCount);
				}
				else
				{
					orderEntryProductQtyMap.put(skuCode, orderEntry.getQuantity().intValue());
				}
			}
		});
	}

	/**
	 * Check eligiblity for entry.
	 *
	 * @param orderEntry
	 *           the order entry
	 * @return true, if successful
	 */
	private boolean checkEligiblityForEntry(final AbstractOrderEntryModel orderEntry)
	{
		final String orderCode = orderEntry.getOrder().getCode();
		final String orderEntryPk = orderEntry.getPk().toString();
		if (Objects.isNull(orderEntry.getProduct()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"ChangeShipmentStatusAction :: checkEligiblityForEntry :: Skipping check as No Product Present on OrderEntry : {} on order : {} ",
					orderEntryPk, orderCode);
			return false;
		}
		if (BooleanUtils.isTrue(orderEntry.isBundleMainEntry()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"ChangeShipmentStatusAction :: checkEligiblityForEntry :: Skipping check as Product : {} is a Bundle Product Entry on OrderEntry : {} on order : {} ",
					orderEntry.getProduct().getCode(), orderEntryPk, orderCode);
			return false;
		}
		if (BlCoreConstants.AQUATECH_BRAND_ID.equals(orderEntry.getProduct().getManufacturerAID()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"ChangeShipmentStatusAction :: checkEligiblityForEntry :: Skipping check as Product : {} is a Aqua Tech Product on OrderEntry : {} on order : {} ",
					orderEntry.getProduct().getCode(), orderEntryPk, orderCode);
			return false;
		}
		if (orderEntry.getProduct() instanceof BlProductModel
				&& Objects.nonNull(((BlProductModel) (orderEntry.getProduct())).getNumberSystem())
				&& BooleanUtils.isTrue(((BlProductModel) (orderEntry.getProduct())).getNumberSystem().getCode()
						.equals(NumberingSystemEnum.NONE.getCode())))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"ChangeShipmentStatusAction :: checkEligiblityForEntry :: Skipping check as Product : {} having Number System as NONE on OrderEntry : {} on order : {} ",
					orderEntry.getProduct().getCode(), orderEntryPk, orderCode);
			return false;
		}
		return true;
	}

}
