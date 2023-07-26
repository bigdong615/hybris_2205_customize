package com.bl.backoffice.widget.controller.order;

import com.bl.core.enums.BlCancelReason;
import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.jalo.order.AbstractOrderEntry;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Grid;
import org.zkoss.zul.ListModelArray;
import org.zkoss.zul.ListModelList;

import java.util.*;

public class BlOrderBillingController extends DefaultWidgetController {

    private static final String IN_SOCKET = "inputObject";
    private OrderModel orderModel;
    @Wire
    private Combobox missingItemToolCombobox;
    private List<String> billingChargesReason = new ArrayList<>();
    @WireVariable
    private transient EnumerationService enumerationService;
    @WireVariable
    private transient BackofficeLocaleService cockpitLocaleService;
    @Wire
    private Grid orderEntries;

    private static final String MISSING_ITEM_TOOL = "customersupportbackoffice.billing.widget.title";
    private transient List<BlOrderBillingItemDTO> orderEntriesForBilling;


    @SocketEvent(socketId = IN_SOCKET)
    public void initPartialRefundForm(final OrderModel inputOrder) {
        this.setOrderModel(inputOrder);
        this.getWidgetInstanceManager().setTitle(new StringBuilder(this.getWidgetInstanceManager()
                .getLabel(MISSING_ITEM_TOOL)).append(
                this.getOrderModel().getCode()).toString());
        getMissingItemReasons();
        this.initializePopupRequiredFields(inputOrder);
        this.getOrderEntries().setModel(new ListModelList<>(this.orderEntriesForBilling));
        this.getOrderEntries().renderAll();
    }

    private void getMissingItemReasons() {
        this.getEnumerationService().getEnumerationValues(ItemBillingChargeTypeEnum.class).forEach(reason ->
                this.billingChargesReason.add(this.getEnumerationService().getEnumerationName(reason, this.getLocale())));

        this.getEnumerationService().getEnumerationValues(ItemBillingChargeTypeEnum.class);
        this.missingItemToolCombobox.setModel(new ListModelArray<>(this.billingChargesReason));
    }

    private void initializePopupRequiredFields(OrderModel inputOrder) {
        this.orderEntriesForBilling = new ArrayList<>();

        inputOrder.getEntries().forEach(abstractOrderEntryModel -> {
            abstractOrderEntryModel.getSerialProducts().forEach(serialProduct -> {
                BlOrderBillingItemDTO itemDTO = new BlOrderBillingItemDTO();
                itemDTO.setProductName(abstractOrderEntryModel.getProduct().getName());
                itemDTO.setSerialNo(serialProduct.getCode());
                itemDTO.setAmount(serialProduct.getRetailPrice());
                itemDTO.setDamageWaiver(Boolean.TRUE);
                itemDTO.setSubtotal(serialProduct.getRetailPrice() * 12 / 100);
                itemDTO.setProcessingFee(Boolean.TRUE);
                itemDTO.setTax(24.05);
                itemDTO.setUnpaidBillNotes("Missing Item " + abstractOrderEntryModel.getProduct().getName() + " - " + "Serial# " + serialProduct.getCode());
                this.orderEntriesForBilling.add(itemDTO);

            });
        });

    }


    public OrderModel getOrderModel() {
        return orderModel;
    }

    public void setOrderModel(final OrderModel orderModel) {
        this.orderModel = orderModel;
    }

    public EnumerationService getEnumerationService() {
        return enumerationService;
    }

    public void setEnumerationService(EnumerationService enumerationService) {
        this.enumerationService = enumerationService;
    }

    public Grid getOrderEntries() {
        return orderEntries;
    }

    public void setOrderEntries(Grid orderEntries) {
        this.orderEntries = orderEntries;
    }

    private Locale getLocale() {
        return this.getCockpitLocaleService().getCurrentLocale();
    }

    private BackofficeLocaleService getCockpitLocaleService() {
        return this.cockpitLocaleService;
    }
}
