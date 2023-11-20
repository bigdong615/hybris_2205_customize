package com.bl.backoffice.wizards.util;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import org.zkoss.zul.ListModelList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ReplacementProductData {

    private String productName;
    private String assignedSerial;
    private String ocLocation;
    private String newSerial;
    private List<String> reason;
    private AbstractOrderEntryModel orderEntry;
    private BlSerialProductModel oldSerial;
    private  String selectedReason;
    private ConsignmentModel consignment;
    private ConsignmentEntryModel consEntry;



   public ReplacementProductData(){
        List<String> reasonList= Arrays.asList("OB","Other");
        this.reason=new ListModelList<>(reasonList);
    }
    public List<String> getReason() {
        return reason;
    }


    public String getProductName() {
        return productName;
    }

    public void setProductName(String productName) {
        this.productName = productName;
    }

    public String getAssignedSerial() {
        return assignedSerial;
    }

    public void setAssignedSerial(String assignedSerial) {
        this.assignedSerial = assignedSerial;
    }

    public String getOcLocation() {
        return ocLocation;
    }

    public void setOcLocation(String ocLocation) {
        this.ocLocation = ocLocation;
    }

    public String getNewSerial() {
        return newSerial;
    }

    public void setNewSerial(String newSerial) {
        this.newSerial = newSerial;
    }
    public AbstractOrderEntryModel getOrderEntry() {
        return orderEntry;
    }

    public void setOrderEntry(AbstractOrderEntryModel orderEntry) {
        this.orderEntry = orderEntry;
    }

    public BlSerialProductModel getOldSerial() {
        return oldSerial;
    }

    public void setOldSerial(BlSerialProductModel oldSerial) {
        this.oldSerial = oldSerial;
    }
    public String getSelectedReason() {
        return selectedReason;
    }

    public void setSelectedReason(String selectedReason) {
        this.selectedReason = selectedReason;
    }
    public ConsignmentModel getConsignment() {
        return consignment;
    }

    public void setConsignment(ConsignmentModel consignment) {
        this.consignment = consignment;
    }

    public ConsignmentEntryModel getConsEntry() {
        return consEntry;
    }

    public void setConsEntry(ConsignmentEntryModel consEntry) {
        this.consEntry = consEntry;
    }


}
