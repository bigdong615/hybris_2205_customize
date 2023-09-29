package com.bl.core.order.actions;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.ReallocateSerialProcessModel;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.logging.BlLogger;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.task.RetryLaterException;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class RemoveSerialAssignToFutureConsignmentAction extends AbstractSimpleDecisionAction<ReallocateSerialProcessModel> {

    private static final Logger LOG = Logger.getLogger(RemoveSerialAssignToFutureConsignmentAction.class);

    private BlConsignmentEntryService blConsignmentEntryService;

    @Value("${bl.serial.status.need.to.remove.from.future.order}")
    private String serialStatus;

    @Override
    public Transition executeAction(ReallocateSerialProcessModel serialProcessModel)
            throws RetryLaterException, Exception {
        BlSerialProductModel blSerialProduct = serialProcessModel.getOldSerialProduct();
        BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Starting remove serial action for serial status {} after replacement of serial {}",blSerialProduct.getSerialStatus().getCode(),blSerialProduct.getCode());

        try {
            if (isEligibleToRemoveSerialFromOrder(blSerialProduct)) {
                getBlConsignmentEntryService().removeSerialFromFutureConsignmentEntry(blSerialProduct);
            }
        }catch (Exception ex){
            BlLogger.logMessage(LOG, Level.ERROR,"Some error occurred while remove serial from consignment :"+blSerialProduct.getCode(),ex);
            return Transition.NOK;
        }
        return Transition.OK;
    }

    private boolean isEligibleToRemoveSerialFromOrder(final BlSerialProductModel blSerialProduct)
    {
        final List<String> serialStatusList = Arrays.asList(serialStatus.split(","));
        return Objects.nonNull(blSerialProduct.getSerialStatus())
                && serialStatusList.contains(blSerialProduct.getSerialStatus().getCode());
    }

    public BlConsignmentEntryService getBlConsignmentEntryService() {
        return blConsignmentEntryService;
    }

    public void setBlConsignmentEntryService(BlConsignmentEntryService blConsignmentEntryService) {
        this.blConsignmentEntryService = blConsignmentEntryService;
    }

}
