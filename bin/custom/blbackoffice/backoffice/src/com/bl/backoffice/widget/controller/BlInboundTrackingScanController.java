package com.bl.backoffice.widget.controller;

import com.bl.backoffice.wizards.util.OrderTrackingScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.model.BlSerialProductModel;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.*;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

public class BlInboundTrackingScanController extends DefaultWidgetController {

    private static final Logger LOG = Logger.getLogger(BlInboundTrackingScanController.class);
    protected static final String OUT_CONFIRM = "confirmOutput";
    protected static final String COMPLETE = "completed";

    protected static final String IN_SOCKET = "nodeSelected";
    private Textbox textInput;

    @Resource(name = "blInventoryScanToolDao")
    BlInventoryScanToolDao blInventoryScanToolDao;

    private Grid inboundTrackingScanToolData;
    @Wire
    private Div inboundTrackingSacntoolDataHeader;
    @SocketEvent(socketId = IN_SOCKET)
    public void initLoadPage(final Object inputObject)
    {
        //select {o.pk} from {Order as o JOIN Consignment as co on {co.order}={o.pk} JOIN PackagingInfo as pi on {pi.consignment} = {co.pk}} where {pi.inboundTrackingNumber} = '1Z19E5969094203950'
        this.getWidgetInstanceManager()
                .setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.order.consolidation.heading")));
        this.inboundTrackingSacntoolDataHeader.setStyle("resize:none;display:none");

    }

    @ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void cancel()
    {
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }

    @ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_BAR_CODES, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void getInboundTrackingScanData()
    {
        if (StringUtils.isBlank(textInput.getText()))
        {
            this.inboundTrackingSacntoolDataHeader.setStyle("resize:none;display:none");
            Messagebox.show("No Tracking Number to Scan");
        }
        else{
           final List<OrderModel> orderModels =  getBlInventoryScanToolDao().getOrdersWithTrackingNo(textInput.getText().trim());
           List<OrderTrackingScanToolData> orderTrackingScanToolData = new ArrayList<>();
           if(CollectionUtils.isNotEmpty(orderModels)) {
               orderModels.forEach(orderModel -> {
                   LOG.info("Order No is " + orderModel.getCode());
                   orderModel.getConsignments().forEach(consignmentModel -> {
                       consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
                           consignmentEntryModel.getSerialProducts().forEach(blProductModel -> {
                               if (blProductModel instanceof BlSerialProductModel) {
                                   OrderTrackingScanToolData orderTrackingScanToolData1 = new OrderTrackingScanToolData();
                                   orderTrackingScanToolData1.setProductName((((BlSerialProductModel) blProductModel).getBlProduct().getName()));
                                   orderTrackingScanToolData1.setProductCode((((BlSerialProductModel) blProductModel).getBlProduct().getCode()));
                                   orderTrackingScanToolData1.setSerialNo((((BlSerialProductModel) blProductModel).getBarcode()));
                                   orderTrackingScanToolData1.setShipmentStartDate(consignmentEntryModel.getConsignment().getOptimizedShippingStartDate().toString());

                                   orderTrackingScanToolData.add(orderTrackingScanToolData1);
                               }
                           });


                       });
                   });
               });
               this.inboundTrackingSacntoolDataHeader.setStyle("resize:none;display:block");
               this.getInboundTrackingScanToolData().setModel(new ListModelList<>(orderTrackingScanToolData));
               this.getInboundTrackingScanToolData().renderAll();

           }
           else {
               Messagebox.show("Orders are not present with the given Tracking No "+ textInput.getText());
           }
        }
    }

    @ViewEvent(componentID = "checkSerial", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void checkSerialFutureorders()
    {
        Messagebox.show("checkSerial clicked");
        playSound();
    }

    private void playSound() {
        LOG.info("play Sound worked");
    }


    public BlInventoryScanToolDao getBlInventoryScanToolDao()
    {
        return blInventoryScanToolDao;
    }

    public Grid getInboundTrackingScanToolData() {
        return inboundTrackingScanToolData;
    }
}
