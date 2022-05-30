package com.bl.facades.populators;

import de.hybris.platform.basecommerce.enums.CancelReason;
import de.hybris.platform.basecommerce.enums.OrderModificationEntryStatus;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.ordercancel.model.OrderCancelRecordEntryModel;
import de.hybris.platform.ordercancel.model.OrderCancelRecordModel;
import de.hybris.platform.ordercancel.model.OrderEntryCancelRecordEntryModel;
import de.hybris.platform.orderhistory.model.OrderHistoryEntryModel;
import de.hybris.platform.ordermodify.model.OrderModificationRecordEntryModel;
import de.hybris.platform.ordermodify.model.OrderModificationRecordModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.log4j.Logger;

import java.util.Date;

public class BlCancelOrderPopulator implements Populator<OrderEntryData , OrderEntryModel> {


    private static final Logger LOG = Logger.getLogger(BlCancelOrderPopulator.class);

    private ModelService modelService;

   // private BlOrderEntryCancelledQuantityHandler blOrderEntryCancelledQuantityHandler;
    @Override
    public void populate(OrderEntryData orderEntryData , OrderEntryModel orderEntryModel) throws ConversionException {
    orderEntryModel.setQuantity(0L);
    try {
        OrderEntryCancelRecordEntryModel orderEntryCancelRecordEntryModel = getModelService().create(OrderEntryCancelRecordEntryModel.class);
        OrderCancelRecordEntryModel orderModificationRecordEntryModel = getModelService().create(OrderCancelRecordEntryModel.class);
        OrderCancelRecordModel orderModificationRecordModel =  getModelService().create(OrderCancelRecordModel.class);
        OrderHistoryEntryModel orderHistoryEntryModel = getModelService().create(OrderHistoryEntryModel.class);
        orderEntryCancelRecordEntryModel.setCancelledQuantity((int) orderEntryData.getCancellableQty());
        orderEntryCancelRecordEntryModel.setCancelReason(CancelReason.NA);
        orderEntryCancelRecordEntryModel.setCode(String.valueOf(orderEntryModel.getPk()));
        orderEntryCancelRecordEntryModel.setOrderEntry(orderEntryModel);
        orderEntryCancelRecordEntryModel.setOriginalOrderEntry(orderEntryModel);
        orderModificationRecordEntryModel.setCode(String.valueOf(orderEntryModel.getPk()));
        orderModificationRecordEntryModel.setTimestamp(new Date());
        orderModificationRecordModel.setOrder(orderEntryModel.getOrder());
        orderModificationRecordEntryModel.setModificationRecord(orderModificationRecordModel);
        orderHistoryEntryModel.setOrder(orderEntryModel.getOrder());
        orderHistoryEntryModel.setTimestamp(new Date());
        orderModificationRecordEntryModel.setOriginalVersion(orderHistoryEntryModel);
        orderModificationRecordEntryModel.setStatus(OrderModificationEntryStatus.SUCCESSFULL);
        orderEntryCancelRecordEntryModel.setModificationRecordEntry(orderModificationRecordEntryModel);
        getModelService().save(orderEntryCancelRecordEntryModel);
        getModelService().refresh(orderEntryCancelRecordEntryModel);
        System.out.println("orderEntryCancelRecordEntryModel" + orderEntryCancelRecordEntryModel.getPk().toString());
        getModelService().save(orderModificationRecordEntryModel);
        getModelService().refresh(orderModificationRecordEntryModel);
        System.out.println("orderModificationRecordEntryModel" + orderModificationRecordEntryModel.getPk().toString());
        getModelService().save(orderModificationRecordModel);
        getModelService().refresh(orderModificationRecordModel);
        System.out.println("orderModificationRecordModel" + orderModificationRecordModel.getPk().toString());
        getModelService().save(orderHistoryEntryModel);
        getModelService().refresh(orderHistoryEntryModel);
        System.out.println("orderHistoryEntryModel" + orderHistoryEntryModel.getPk().toString());
    }
    catch (Exception e) {
        System.out.println(e);
    }
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

}
