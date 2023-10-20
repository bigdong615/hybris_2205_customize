package com.bl.backoffice.widget.controller;

import com.bl.backoffice.wizards.util.OrderTrackingScanToolData;
import com.bl.backoffice.wizards.util.SerialData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.constants.BlloggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import com.hybris.cockpitng.util.notifications.NotificationService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
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
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

public class BlInboundTrackingScanController extends DefaultWidgetController {

    private static final Logger LOG = Logger.getLogger(BlInboundTrackingScanController.class);
    protected static final String OUT_CONFIRM = "confirmOutput";
    protected static final String COMPLETE = "completed";
    private static final String TRACKING_SCAN_POSITIVE = "blbackoffice.order.inbound.tracking.scan.notification.positive";
    private static final String TRACKING_SCAN_NEGATIVE = "blbackoffice.order.inbound.tracking.scan.notification.negative";
    private static final int BUFFER_SIZE = 4096;
    private static final String STYLE_SHOW = "resize:none;display:block";
    private static final String STYLE_HIDE = "resize:none;display:none";
    private static final String ADD_TRACK_NO_ERROR = "blbackoffice.order.inbound.tracking.scan.tracking.error";

    protected static final String IN_SOCKET = "nodeSelected";
    private Textbox textInput;

    @Resource(name = "blInventoryScanToolDao")
    BlInventoryScanToolDao blInventoryScanToolDao;
    @Resource(name = "blStockLevelDao")
    private BlStockLevelDao blStockLevelDao;

    private Grid inboundTrackingScanToolData;
    private Grid inboundTrackingScanToolSerialData;
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
        this.inboundTrackingSacntoolDataHeader.setStyle(STYLE_HIDE);

    }

    @ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void cancel()
    {
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }

    @ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_BAR_CODES, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void getInboundTrackingScanData()
    {
        String inputText = textInput.getText();
        if (StringUtils.isBlank(inputText))
        {
            handleNoTrackingNumber();
        }
        else{
           final List<OrderModel> orderModels =  getOrdersByTrackingNumber(inputText);

           if(CollectionUtils.isEmpty(orderModels)) {
               handleNoAssociatedOrders(inputText);
           }
           else {
               List<OrderTrackingScanToolData> orderTrackingScanToolData = processOrders(orderModels);
               updateUIWithScanData(orderTrackingScanToolData);
               checkIfSerialsArePresentInFutureOrders(orderTrackingScanToolData);
           }
        }
    }

    private void handleNoTrackingNumber() {
        this.inboundTrackingSacntoolDataHeader.setStyle(STYLE_HIDE);
        notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                NotificationEvent.Level.FAILURE, this.getLabel(ADD_TRACK_NO_ERROR));
    }

    private List<OrderModel> getOrdersByTrackingNumber(String trackingNumber) {
        return getBlInventoryScanToolDao().getOrdersWithTrackingNo(trackingNumber.trim());
    }

    private void handleNoAssociatedOrders(String trackingNumber) {
        this.inboundTrackingSacntoolDataHeader.setStyle(STYLE_HIDE);
        Messagebox.show("This tracking number [" + trackingNumber + "] is not associated with any orders");
    }

    private List<OrderTrackingScanToolData> processOrders(List<OrderModel> orderModels) {
        List<OrderTrackingScanToolData> orderTrackingScanToolData = new ArrayList<>();

        for (OrderModel orderModel : orderModels) {
            OrderTrackingScanToolData orderTrackingData = createOrderTrackingData(orderModel);
            orderTrackingScanToolData.add(orderTrackingData);
        }

        return orderTrackingScanToolData;
    }

    private OrderTrackingScanToolData createOrderTrackingData(OrderModel orderModel) {
        OrderTrackingScanToolData orderTrackingData = new OrderTrackingScanToolData();

        // Populate orderTrackingData with details from orderModel
        final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
        orderTrackingData.setOrderNumber(orderModel.getCode());
        orderTrackingData.setExpectedReturnDate(formatter.format(orderModel.getConsignments().iterator().next().getOptimizedShippingEndDate()));
        orderTrackingData.setCustomerEmail(orderModel.getUser().getUid());
        orderTrackingData.setRentalStartDate(formatter.format(orderModel.getRentalStartDate()));
        orderTrackingData.setRentalEndDate(formatter.format(orderModel.getRentalEndDate()));
        orderTrackingData.setStatus(orderModel.getStatus());

        // Extract serial product data and populate serialData
        List<SerialData> serialData = extractSerialData(orderModel);
        orderTrackingData.setSerialData(serialData);
        inboundTrackingScanToolSerialData.setModel(new ListModelList<>(serialData));
        inboundTrackingScanToolSerialData.renderAll();

        return orderTrackingData;
    }

    private void updateUIWithScanData(List<OrderTrackingScanToolData> orderTrackingScanToolData) {
        inboundTrackingSacntoolDataHeader.setStyle(STYLE_SHOW);
        inboundTrackingScanToolData.setModel(new ListModelList<>(orderTrackingScanToolData));
        inboundTrackingScanToolData.renderAll();

    }

    private List<SerialData> extractSerialData(OrderModel orderModel) {
        return orderModel.getConsignments().stream()
                .flatMap(consignmentModel -> consignmentModel.getConsignmentEntries().stream())
                .flatMap(consignmentEntryModel -> consignmentEntryModel.getSerialProducts().stream())
                .filter(blProductModel -> blProductModel instanceof BlSerialProductModel)
                .map(blProductModel -> {
                    SerialData sData = new SerialData();
                    // Populate sData with serial product details
                    sData.setItemCode(blProductModel.getCode());
                    if (((BlSerialProductModel) blProductModel).getOcLocationDetails() != null
                            && ((BlSerialProductModel) blProductModel).getOcLocationDetails().getLocationPriority() != null) {
                        sData.setPriority(String.valueOf(((BlSerialProductModel) blProductModel).getOcLocationDetails().getLocationPriority()));
                    } else {
                        sData.setPriority("");
                    }
                    sData.setItemTitle(((BlSerialProductModel) blProductModel).getBlProduct().getName());
                    sData.setSerialNumber(((BlSerialProductModel) blProductModel).getBarcode());
                    return sData;
                })
                .collect(Collectors.toList());
    }

    // A method to check if serials are present in future orders
    private void checkIfSerialsArePresentInFutureOrders(List<OrderTrackingScanToolData> orderTrackingScanToolData) {

        Set<String> serialCodes = orderTrackingScanToolData.stream()
                .flatMap(orderTrackingScanToolData1 -> orderTrackingScanToolData1.getSerialData().stream())
                .map(SerialData::getItemCode)
                .collect(Collectors.toSet());

        final Date currentDate = Date.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
        final Date nextFourDaysDate = BlDateTimeUtils.getNextFourDaysDate();
        Collection<StockLevelModel> givenSerialStocks = getBlStockLevelDao().findALLSerialStockLevelsForDateAndCodes(serialCodes, currentDate, nextFourDaysDate);

        try {
            if (CollectionUtils.isNotEmpty(givenSerialStocks)) {
                boolean stock = givenSerialStocks.stream().anyMatch(stockLevelModel -> StringUtils.isNotBlank(stockLevelModel.getOrder()));
                if (stock) {
                  notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                            NotificationEvent.Level.SUCCESS, this.getLabel(TRACKING_SCAN_POSITIVE));
                   playSound("audios/success-1-6297.wav");
                } else {
                   notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                            NotificationEvent.Level.WARNING, this.getLabel(TRACKING_SCAN_NEGATIVE));
                    playSound("audios/bonk-sound-effect.wav");
                }
            }
        } catch (Exception ex) {
            LOG.error("An error occurred while playing sound", ex);
        }
    }

    private void playSound(String filePath) {
         try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream(filePath);
             AudioInputStream audioStream = AudioSystem.getAudioInputStream(inputStream)) {
             AudioFormat audioFormat = audioStream.getFormat();
             AudioFormat targetFormat = new AudioFormat(AudioFormat.Encoding.PCM_SIGNED,
                     audioFormat.getSampleRate(), audioFormat.getSampleSizeInBits(),audioFormat.getChannels(), audioFormat.getFrameSize(), audioFormat.getFrameRate(),
                      false);

             AudioInputStream targetInputStream = AudioSystem.getAudioInputStream(targetFormat, audioStream);
             DataLine.Info info = new DataLine.Info(SourceDataLine.class, targetFormat);
            try (SourceDataLine sourceDataLine = (SourceDataLine) AudioSystem.getLine(info)) {
                sourceDataLine.open(targetFormat);
                sourceDataLine.start();

                byte[] bufferBytes = new byte[BUFFER_SIZE];
                int readBytes;
                while ((readBytes = targetInputStream.read(bufferBytes)) != -1) {
                    sourceDataLine.write(bufferBytes, 0, readBytes);
                }
                sourceDataLine.drain();
            }
        } catch (UnsupportedAudioFileException | IOException | LineUnavailableException e) {
            LOG.error("An error occurred while playing sound in playSound() method", e);
        }
    }


    public BlInventoryScanToolDao getBlInventoryScanToolDao()
    {
        return blInventoryScanToolDao;
    }

    public Grid getInboundTrackingScanToolData() {
        return inboundTrackingScanToolData;
    }

    public Grid getInboundTrackingScanToolSerialData() {
        return inboundTrackingScanToolSerialData;
    }

    public BlStockLevelDao getBlStockLevelDao() {
        return blStockLevelDao;
    }

    public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
        this.blStockLevelDao = blStockLevelDao;
    }
}
