package com.bl.backoffice.widget.controller.order;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.jalo.BlProduct;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.populators.BlCancelOrderPopulator;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zul.Messagebox;

import javax.annotation.Resource;
import java.util.*;

public class BlCancelOrderController extends DefaultWidgetController {
    private static final Logger LOG = Logger.getLogger(BlCancelOrderController.class);
    protected static final String OUT_CONFIRM = "confirmcancellation";

    @Resource(name = "orderDao")
    private BlOrderDao orderDao;

    @Resource(name = "modelService")
    private ModelService modelService;

    @Resource(name = "blStockLevelDao")
    private BlStockLevelDao blStockLevelDao;

    @Resource(name = "blOrderCancelPopulator")
    private BlCancelOrderPopulator blOrderCancelPopulator;

    private OrderModel orderModel;

    @Resource(name = "blEspEventService")
    private DefaultBlESPEventService blEspEventService;



    /**
     * Init cancellation order form.
     * @param inputObject the input object
     */
    @SocketEvent(socketId = BlCustomCancelRefundConstants.INPUT_OBJECT)
    public void initCancellationOrderForm(final OrderModel orderModel) {
     setOrderModel(orderModel);
     if(StringUtils.equalsIgnoreCase(orderModel.getStatus().getCode() , OrderStatus.SHIPPED.getCode())) {
         this.getWidgetInstanceManager()
                 .setTitle("Order cannot be cancelled as it is already Shipped");
         Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_MSG),
                 this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_TITLE) + org.apache.commons.lang3.StringUtils.SPACE
                         + this.getOrderModel().getCode(), new Messagebox.Button[]{Messagebox.Button.NO, Messagebox.Button.YES},
                 BlCustomCancelRefundConstants.OMS_WIDGET_CANCELORDER_CONFIRM_ICON, null);
     }
        this.getWidgetInstanceManager()
                .setTitle("Do you want to cancel this order" + " : " + orderModel.getCode());
    }

    /**
     * This method is used to close the cancel and refund Popup
     */
    @ViewEvent(componentID = "closePopup", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void cancelPopup()
    {
        this.sendOutput(OUT_CONFIRM, "");
    }

    /**
     * Confirm cancellation.
     */
    @ViewEvent(componentID = BlCustomCancelRefundConstants.CONFIRM_CANCELLATION, eventName = BlCustomCancelRefundConstants.ON_CLICK)
    public void confirmCancellation() {
        final AbstractOrderModel order = this.getOrderModel();
        if(null != order) {
            if(CollectionUtils.isNotEmpty(order.getConsignments())) {
                updateStockForCancelledOrder(order.getConsignments() , order);
                this.sendOutput(OUT_CONFIRM, "");
            }
        }
    }



    private void updateStockForCancelledOrder(final Set<ConsignmentModel> consignments, final AbstractOrderModel abstractOrderModel)
    {
        List<String> serialProductCodes = new ArrayList<>();
        for (final ConsignmentModel consignment : consignments)
        {
            consignment.getConsignmentEntries()
                    .forEach(consignmentEntry -> consignmentEntry.getSerialProducts()
                            .forEach(serialProduct -> {
                                if(!StringUtils.equalsIgnoreCase(serialProduct.getProductType().getCode(), ProductTypeEnum.SUBPARTS.getCode())){
                                    updateStockForCancelledProductFromBackoffice(serialProduct,
                                            consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate() , serialProductCodes);
                                }
                            } ));
        }

        if(CollectionUtils.isEmpty(serialProductCodes)){
            abstractOrderModel.getEntries().forEach(abstractOrderEntryModel -> {
               OrderEntryData orderEntryData =  new OrderEntryData();
               orderEntryData.setCancellableQty(abstractOrderEntryModel.getQuantity());
                blOrderCancelPopulator.populate(orderEntryData, (OrderEntryModel) abstractOrderEntryModel);
                modelService.save(abstractOrderEntryModel);
                modelService.refresh(abstractOrderEntryModel);
            });

            abstractOrderModel.setStatus(OrderStatus.CANCELLED);
            BlLogger.logFormattedMessage(LOG , Level.INFO , "Order has been Cancelled {} " , abstractOrderModel.getCode());
            modelService.save(abstractOrderModel);
            modelService.refresh(abstractOrderModel);
            abstractOrderModel.getConsignments().forEach(consignmentModel -> {
                consignmentModel.setStatus(ConsignmentStatus.CANCELLED);
                modelService.save(consignmentModel);
                modelService.refresh(consignmentModel);
            });
            try {
                blEspEventService.sendOrderCanceledEvent((OrderModel) abstractOrderModel);
            }catch(final Exception e)
            {
                BlLogger.logMessage(LOG, Level.ERROR,"Failed to trigger order canceled event" , e);
            }

        }
    }


    public void updateStockForCancelledProductFromBackoffice(final BlProductModel serialProduct, final Date optimizedShippingStartDate,
                                                             Date optimizedShippingEndDate, final List<String> serialProductCodes)
    {
        if(null == optimizedShippingEndDate) {
            optimizedShippingEndDate = BlDateTimeUtils.getNextYearsSameDay();
        }
        final Collection<StockLevelModel> findSerialStockLevelForDate = blStockLevelDao
                .findSerialStockLevelForDate(serialProduct.getCode(), optimizedShippingStartDate, optimizedShippingEndDate);
        if (CollectionUtils.isNotEmpty(findSerialStockLevelForDate))
        {
            findSerialStockLevelForDate.forEach(stockLevel -> {
                stockLevel.setHardAssigned(false);
                stockLevel.setReservedStatus(false);
                stockLevel.setOrder(null);
                ((BlSerialProductModel) serialProduct).setHardAssigned(false); // NOSONAR
                modelService.save(stockLevel);
                modelService.save(serialProduct);
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Reserved status set to {} and Hard Assigned set to {} for serial {}",
                        stockLevel.getReservedStatus(), stockLevel.getHardAssigned(), serialProduct.getCode());
            });
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock level updated for serial {}", serialProduct.getCode());
        }
        else {
            serialProductCodes.add(serialProduct.getCode());
        }
    }


    public OrderModel getOrderModel() {
        return orderModel;
    }

    public void setOrderModel(OrderModel orderModel) {
        this.orderModel = orderModel;
    }
}
