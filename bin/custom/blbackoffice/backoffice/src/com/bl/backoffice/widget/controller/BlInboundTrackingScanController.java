package com.bl.backoffice.widget.controller;

import com.bl.backoffice.wizards.util.OrderTrackingScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.constants.BlloggingConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import com.hybris.cockpitng.util.notifications.NotificationService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.*;

import javax.annotation.Resource;
import javax.sound.sampled.*;
import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;

public class BlInboundTrackingScanController extends DefaultWidgetController {

    private static final Logger LOG = Logger.getLogger(BlInboundTrackingScanController.class);
    protected static final String OUT_CONFIRM = "confirmOutput";
    protected static final String COMPLETE = "completed";
    private static final String TRACKING_SCAN_POSITIVE = "blbackoffice.order.inbound.tracking.scan.notification.positive";
    private static final String TRACKING_SCAN_NEGATIVE = "blbackoffice.order.inbound.tracking.scan.notification.negative";
    private static final int BUFFER_SIZE = 4096;

    protected static final String IN_SOCKET = "nodeSelected";
    private Textbox textInput;

    @Resource(name = "blInventoryScanToolDao")
    BlInventoryScanToolDao blInventoryScanToolDao;
    @Resource(name = "blStockLevelDao")
    private BlStockLevelDao blStockLevelDao;

    private Grid inboundTrackingScanToolData;
    @Wire
    private Div inboundTrackingSacntoolDataHeader;

    @Resource
    private transient NotificationService notificationService;

    @SocketEvent(socketId = IN_SOCKET)
    public void initLoadPage(final Object inputObject)
    {
        //select {o.pk} from {Order as o JOIN Consignment as co on {co.order}={o.pk} JOIN PackagingInfo as pi on {pi.consignment} = {co.pk}} where {pi.inboundTrackingNumber} = '1Z19E5969094203950'
        this.getWidgetInstanceManager()
                .setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.order.scan.heading")));
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
                                   orderTrackingScanToolData1.setSerialNo((((BlSerialProductModel) blProductModel).getCode()));
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
           if(CollectionUtils.isNotEmpty(orderTrackingScanToolData)) {
               CheckIfSerialsArePresentInFutureOrders(orderTrackingScanToolData);
           }
        }


    }

    private void CheckIfSerialsArePresentInFutureOrders(List<OrderTrackingScanToolData> orderTrackingScanToolData) {
        Set<String> serialCodes = new HashSet<>();
        orderTrackingScanToolData.forEach(orderTrackingScanToolData1 -> {
            serialCodes.add(orderTrackingScanToolData1.getSerialNo());
        });

        //get stock list of given serials for next 4 business days
        final Date currentDate = Date.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
        final Date nextFourDaysDate = BlDateTimeUtils.getNextFourDaysDate();
        Collection<StockLevelModel> givenSerialStocks = getBlStockLevelDao().findSerialStockLevelsForDateAndCodes(serialCodes,currentDate,nextFourDaysDate,true);
        try {
        if(CollectionUtils.isNotEmpty(givenSerialStocks)){
           boolean stock = givenSerialStocks.stream().anyMatch(stockLevelModel -> StringUtils.isNotBlank(stockLevelModel.getOrder()));

            if(stock){
               // Messagebox.show("One or more items needed for an outbound order");
                notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                        NotificationEvent.Level.SUCCESS, this.getLabel(TRACKING_SCAN_POSITIVE));
                playSound("audios/success-1-6297.wav");
           }
           else {
                //Messagebox.show("No priority items");
                notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                        NotificationEvent.Level.INFO, this.getLabel(TRACKING_SCAN_NEGATIVE));
                playSound("audios/bonk-sound-effect.wav");

           }
        }

        }
        catch (Exception ex){
            LOG.error("error during the sound play " + ex);
        }
    }


    private void playSound(String filePath) throws UnsupportedAudioFileException, IOException, LineUnavailableException {
        LOG.info("play Sound worked");
        String audioFilePath = filePath;
        InputStream inputStream = getClass().getClassLoader().getResourceAsStream(audioFilePath);
        AudioInputStream audioStream = AudioSystem.getAudioInputStream(inputStream);
        AudioFormat audioFormat = audioStream.getFormat();
        DataLine.Info info = new DataLine.Info(SourceDataLine.class, audioFormat);
        SourceDataLine sourceDataLine = (SourceDataLine) AudioSystem.getLine(info);
        sourceDataLine.open(audioFormat);
        sourceDataLine.start();
        byte[] bufferBytes = new byte[BUFFER_SIZE];
        int readBytes = -1;
        while ((readBytes = audioStream.read(bufferBytes)) != -1) {
            sourceDataLine.write(bufferBytes, 0, readBytes);
        }
        sourceDataLine.drain();
        sourceDataLine.close();
        audioStream.close();
    }


    public BlInventoryScanToolDao getBlInventoryScanToolDao()
    {
        return blInventoryScanToolDao;
    }

    public Grid getInboundTrackingScanToolData() {
        return inboundTrackingScanToolData;
    }

    public BlStockLevelDao getBlStockLevelDao() {
        return blStockLevelDao;
    }

    public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
        this.blStockLevelDao = blStockLevelDao;
    }
}
