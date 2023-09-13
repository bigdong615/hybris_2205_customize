package com.bl.backoffice.widget.controller;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Div;
import org.zkoss.zul.Grid;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import javax.annotation.Resource;
import javax.sound.sampled.*;
import java.io.IOException;
import java.io.InputStream;
public class ProductAvailCheckController extends DefaultWidgetController {
    private static final Logger LOG = Logger.getLogger(ProductAvailCheckController.class);
    protected static final String OUT_CONFIRM = "confirmOutput";
    protected static final String COMPLETE = "completed";
    private static final int BUFFER_SIZE = 4096;
    protected static final String IN_SOCKET = "nodeSelected";
    private Textbox textInput;
    @Resource(name = "blInventoryScanToolDao")
    BlInventoryScanToolDao blInventoryScanToolDao;

    @Wire
    private Div productAvailCheckToolDataHeader;

    @SocketEvent(socketId = IN_SOCKET)
    public void initLoadPage(final Object textInput) {
        this.getWidgetInstanceManager()
                .setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.product.avail.check.heading ")));
        this.productAvailCheckToolDataHeader.setStyle("resize:none;display:none");
    }

    @ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void cancel() {
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }
}
