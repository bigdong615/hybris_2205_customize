package com.bl.backoffice.widget.controller;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import javax.annotation.Resource;
import java.util.List;

public class BlInboundTrackingScanController extends DefaultWidgetController {

    private static final Logger LOG = Logger.getLogger(BlInboundTrackingScanController.class);
    protected static final String OUT_CONFIRM = "confirmOutput";
    protected static final String COMPLETE = "completed";

    protected static final String IN_SOCKET = "nodeSelected";
    private Textbox textInput;

    @Resource(name = "blInventoryScanToolDao")
    BlInventoryScanToolDao blInventoryScanToolDao;
    @SocketEvent(socketId = IN_SOCKET)
    public void initLoadPage(final Object inputObject)
    {
        //select {o.pk} from {Order as o JOIN Consignment as co on {co.order}={o.pk} JOIN PackagingInfo as pi on {pi.consignment} = {co.pk}} where {pi.inboundTrackingNumber} = '1Z19E5969094203950'
        this.getWidgetInstanceManager()
                .setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.order.consolidation.heading")));

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
            Messagebox.show("No Tracking Number to Scan");
        }
        else{
           final List<OrderModel> orderModels =  getBlInventoryScanToolDao().getOrdersWithTrackingNo(textInput.getText().trim());
           if(CollectionUtils.isNotEmpty(orderModels)) {
               orderModels.forEach(orderModel -> {
                   LOG.info("Order No is " + orderModel.getCode());
               });
           }
           else {
               Messagebox.show("Orders are not present with the given Tracking No "+ textInput.getText());
           }
        }
    }


    public BlInventoryScanToolDao getBlInventoryScanToolDao()
    {
        return blInventoryScanToolDao;
    }

}
