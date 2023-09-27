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

import java.util.Objects;

public class RemoveSerialAssignToFutureConsignmentAction extends AbstractSimpleDecisionAction<ReallocateSerialProcessModel> {

    private static final Logger LOG = Logger.getLogger(RemoveSerialAssignToFutureConsignmentAction.class);

    private BlConsignmentEntryService blConsignmentEntryService;

    @Override
    public Transition executeAction(ReallocateSerialProcessModel serialProcessModel)
            throws RetryLaterException, Exception {
        BlSerialProductModel blSerialProduct = serialProcessModel.getOldSerialProduct();
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
        return Objects.nonNull(blSerialProduct.getSerialStatus())
                && (blSerialProduct.getSerialStatus().equals(SerialStatusEnum.REPAIR_NEEDED)
                || blSerialProduct.getSerialStatus().equals(SerialStatusEnum.PARTS_NEEDED));
    }

    public BlConsignmentEntryService getBlConsignmentEntryService() {
        return blConsignmentEntryService;
    }

    public void setBlConsignmentEntryService(BlConsignmentEntryService blConsignmentEntryService) {
        this.blConsignmentEntryService = blConsignmentEntryService;
    }

}
