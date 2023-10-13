/**
 *
 */
package com.bl.backoffice.widget.controller;

import com.bl.backoffice.actions.ChangeShipmentStatusAction;
import com.bl.backoffice.consignment.service.BlConsignmentService;
import com.bl.backoffice.widget.controller.order.BlCustomCancelRefundConstants;
import com.bl.constants.BlloggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.NumberingSystemEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.payment.service.BlPaymentService;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import com.hybris.cockpitng.util.notifications.NotificationService;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.localization.Localization;

import java.util.*;

import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zhtml.Messagebox;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.ListModelList;

import org.zkoss.zul.Messagebox.Button;


/**
 * ########## BL-749 & BL-863 ######################
 *
 * Controller class used to capture the payment for frontdesk team and warehouse agent
 *
 * @author Krishan Vashishth
 *
 */
public class CapturePaymentController extends DefaultWidgetController {

    private static final Logger LOG = Logger.getLogger(CapturePaymentController.class);

    protected static final String OUT_CONFIRM = "confirmOutput";
    private static final String TITLE_MESSG = "Capture payment for order";
    protected static final String COMPLETE = "completed";
    private static final String CONNECTOR = " : ";
    private static final String ERR_MESG_FOR_ALREADY_CAPTURED_ORDER = "error.message.already.captured.order";
    private static final String ERR_MESG_FOR_ORDER_TRANSFER = "error.message.payment.capture.order.transfer";
    private static final String SUCC_MSG_FOR_PAYMENT_CAPTURED = "blbackoffice.capture,payment.order.success.message";
    private static final String SUCC_MSG_FOR_PAYMENT_CAPTURED_GIFT_CARD = "blbackoffice.capture,payment.gift.card.order.success.message";
    private static final String ERR_MSG_FOR_PAYMENT_CAPTURED = "error.message.payment.captured";
    private static final String MESSAGE_BOX_TITLE = "payment.capture.message.box.title";
    private static final String CANCEL_BUTTON = "cancelChanges";
    private static final String CAPTURE_BUTTON = "captureOrderPayment";
    private static final String INPUT_OBJECT = "inputObject";

    @Resource
    private BlPaymentService blPaymentService;
    
  	@Resource(name = "modelService")
  	private ModelService modelService;

    private OrderModel orderModel;
    private ConsignmentModel consignmentModel;

    @Wire
    private Combobox paymentTransactions;
    
    @Resource(name = "defaultBlConsignmentService")
    private BlConsignmentService defaultBlConsignmentService;
	@Resource
	private transient NotificationService notificationService;


	protected static final String SOCKET_OUT_CONTEXT = "blChangeShipmentStatusContext";

    @SocketEvent(socketId = INPUT_OBJECT)
	 public void init(final ConsignmentModel inputObject)
	 {
   	 this.setConsignmentModel(inputObject);
		 if (getDefaultBlConsignmentService().isMainItemScanRemaining(inputObject))
		 {
			 showMessageBox(BlCoreConstants.MAIN_ITEM_SCAN_REMAINING_ERROR_MSG, true);
		 }
		 else
		 {
			 renderPopup();
		 }
	 }

	/**
	 * Render capture payment popup.
	 */
	private void renderPopup()
	{
		final ConsignmentModel inputObject = this.getConsignmentModel();
		 this.getWidgetInstanceManager()
				 .setTitle(new StringBuilder(TITLE_MESSG).append(CONNECTOR).append(inputObject.getOrder().getCode()).toString());
		 if (inputObject.getOrder() instanceof OrderModel)
		 {
			 this.setOrderModel((OrderModel) inputObject.getOrder());
		 }
		 if (CollectionUtils.isNotEmpty(inputObject.getOrder().getPaymentTransactions()))
		 {
			 final ListModelList<PaymentTransactionModel> listModelList = new ListModelList<>();
			 listModelList.addAll(inputObject.getOrder().getPaymentTransactions());
			 listModelList.forEach(listModelList::addToSelection);
			 paymentTransactions.setModel(listModelList);
		 }
	}

    @ViewEvent(componentID = CAPTURE_BUTTON, eventName = Events.ON_CLICK)
    public void capturePayment() {
        BlLogger.logMessage(LOG, Level.DEBUG, "Payment Capturing starts");
		  if (getDefaultBlConsignmentService().isSubpartScanRemaining(getConsignmentModel()))
		  {
			  Map params = new HashMap();
			  params.put("sclass", "myMessagebox");

			  final List<String> remainingScanSubpartNames = getDefaultBlConsignmentService()
					  .getRemainingScanSubpartNames(this.getConsignmentModel());
			  Messagebox.show(BlCoreConstants.SUBPART_SCAN_REMAINING_ERROR_MSG_1
							  + String.join(BlCoreConstants.COMMA_SEPRATOR, remainingScanSubpartNames)
							  + BlCoreConstants.SUBPART_SCAN_REMAINING_ERROR_MSG_2,
					  "Error Occurred", new Button[]
							  { Button.NO, Button.YES },null , Messagebox.ERROR,null,
					  this::capturePaymentOnEvent,params);

		  }
		  else
		  {
			  processCapturePayment();
		  }        
    }
    
    /**
     * Capture payment on click event.
     *
     * @param obj the obj
     */
    private void capturePaymentOnEvent(final Event obj)
	 {
		 if (Button.YES.event.equals(obj.getName()))
		 {
			 processCapturePayment();
		 }
	 }
    
    private void processCapturePayment()
    {
   	 if (getConsignmentModel().isOrderTransferConsignment() || getConsignmentModel()
             .isInternalTransferConsignment()) {
             showMessageBox(Localization.getLocalizedString(ERR_MESG_FOR_ORDER_TRANSFER), true);
             return;
         }
         if (getOrderModel() == null || StringUtils.isEmpty(getOrderModel().getCode()) || getOrderModel().getIsCaptured()
             || OrderStatus.CANCELLING.equals(getOrderModel().getStatus())) {
             showMessageBox(Localization.getLocalizedString(ERR_MESG_FOR_ALREADY_CAPTURED_ORDER), true);
             return;
         }
         if (blPaymentService.capturePaymentForOrder(getOrderModel())) {
            if(Double.compare(getOrderModel().getTotalPrice(), 0.0) == 0 && CollectionUtils.isNotEmpty(getOrderModel().getGiftCard())) {
          	  getOrderModel().setStatus(OrderStatus.PAYMENT_CAPTURED);
          	  getModelService().save(getOrderModel());
      			  getModelService().refresh(getOrderModel());
          	  showMessageBox(Localization.getLocalizedString(SUCC_MSG_FOR_PAYMENT_CAPTURED_GIFT_CARD));
			  changeShipmentStatus();
			} else {
                showMessageBox(Localization.getLocalizedString(SUCC_MSG_FOR_PAYMENT_CAPTURED));
				changeShipmentStatus();
			}
            getOrderModel().setIsCaptured(Boolean.TRUE);
            getOrderModel().setIsAuthorised(Boolean.TRUE);
            getModelService().save(getOrderModel());
   			getModelService().refresh(getOrderModel());
   			
        } else {
      	getOrderModel().setStatus(OrderStatus.RECEIVED_PAYMENT_DECLINED);
      	getModelService().save(getOrderModel());
 			getModelService().refresh(getOrderModel());
  			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Capture is not successful for the order {}", getOrderModel().getCode());
            BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while capturing the payment");
            showMessageBox(Localization.getLocalizedString(ERR_MSG_FOR_PAYMENT_CAPTURED), true);
        }
    }

	/**
	 * To mark Consignment as BL_SHIPPED once payment is successfully captured
	 */
	private void changeShipmentStatus() {
		if (isUsedOrderOnly(consignmentModel.getOrder())) {
			consignmentModel.setStatus(ConsignmentStatus.BL_SHIPPED);
			getModelService().save(consignmentModel);
			AbstractOrderModel order = consignmentModel.getOrder();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Status updated to {} for consignment {}", consignmentModel.getStatus(),
					consignmentModel.getCode());
			if (order.getConsignments().stream()
					.allMatch(con -> ConsignmentStatus.BL_SHIPPED.equals(con.getStatus()))) {
				order.setStatus(OrderStatus.SOLD_SHIPPED);
				getModelService().save(order);
			}
			this.sendOutput(OUT_CONFIRM, COMPLETE);
			notificationService.notifyUser(org.apache.commons.lang.StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
					NotificationEvent.Level.SUCCESS, "Shipment changed to BL_SHIPPED status");

		}else if(OrderStatus.RECEIVED_MANUAL_REVIEW.equals(consignmentModel.getOrder().getStatus()) || isProductAllocationRemaining(consignmentModel.getOrder()))
		{
			showMessageBox("Order not mark to BL Shipped due to order being in manual review status (non-allocated items).",true);
		}
		else {
			consignmentModel.getConsignmentEntries()
					.forEach(consignmentEntry -> consignmentEntry.getSerialProducts().forEach(serialProduct -> {
						if (serialProduct instanceof BlSerialProductModel) {
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
					.allMatch(con -> ConsignmentStatus.BL_SHIPPED.equals(con.getStatus()))) {
				order.setStatus(OrderStatus.SHIPPED);
				getModelService().save(order);
				getModelService().refresh(order);
			}

			notificationService.notifyUser(org.apache.commons.lang.StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
					NotificationEvent.Level.SUCCESS, "Shipment changed to BL_SHIPPED status");

		}
	}

	/**
	 * Method to check whether it is used order or not
	 * @param order
	 * @return
	 */
	public boolean isUsedOrderOnly(final AbstractOrderModel order)
	{
		return BooleanUtils.isFalse(order.getIsRentalOrder()) && BooleanUtils.isFalse(order.isGiftCardOrder())
				&& BooleanUtils.isFalse(order.getIsRetailGearOrder()) && BooleanUtils.isFalse(order.getIsReplacementOrder());
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
					"ChangeShipmentStatusAction :: isProductAllocationRemaining :: Order is null", org.apache.commons.lang.StringUtils.EMPTY);
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
		if (org.apache.commons.collections.CollectionUtils.isEmpty(order.getEntries()))
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


	@ViewEvent(componentID = CANCEL_BUTTON, eventName = Events.ON_CLICK)
    public void close() {
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }

    public OrderModel getOrderModel() {
        return orderModel;
    }

    public void setOrderModel(OrderModel orderModel) {
        this.orderModel = orderModel;
    }

    /**
     * Show message box.
     *
     * @param message     the message
     * @param isErrorMesg the is error mesg
     */
    protected void showMessageBox(final String message, final boolean isErrorMesg) {
		Map params = new HashMap();
		params.put("sclass", "myMessagebox");

		if (isErrorMesg) {
         	Messagebox.show(message,
					"Error Occurred", new Button[]
							{Button.OK},null , Messagebox.ERROR, null,
					null, params);
        } else {
         	notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
					NotificationEvent.Level.SUCCESS, this.getLabel(message));

		}
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }

    /**
     * Show message box.
     *
     * @param message the message
     */
    protected void showMessageBox(final String message) {
        showMessageBox(message, false);
    }

    public ConsignmentModel getConsignmentModel() {
        return consignmentModel;
    }

    public void setConsignmentModel(
        final ConsignmentModel consignmentModel) {
        this.consignmentModel = consignmentModel;
    }

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService the modelService to set
	 */
	public void setModelService(ModelService modelService)
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
	public void setDefaultBlConsignmentService(final BlConsignmentService defaultBlConsignmentService)
	{
		this.defaultBlConsignmentService = defaultBlConsignmentService;
	}
}
