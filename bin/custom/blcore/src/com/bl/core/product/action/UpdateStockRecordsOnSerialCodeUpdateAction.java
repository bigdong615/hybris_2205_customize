package com.bl.core.product.action;


import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.UpdateStockRecordOnCodeUpdateProcessModel;
import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import de.hybris.platform.task.RetryLaterException;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class UpdateStockRecordsOnSerialCodeUpdateAction extends AbstractSimpleDecisionAction<UpdateStockRecordOnCodeUpdateProcessModel> {

    private static final Logger LOG = Logger.getLogger(UpdateStockRecordsOnSerialCodeUpdateAction.class);

    private BlStockService blStockService;
    /**
     * @param updateStockRecordsOnSerialCodeUpdateModel
     * @return
     * @throws RetryLaterException
     * @throws Exception
     */
    @Override
    public Transition executeAction(UpdateStockRecordOnCodeUpdateProcessModel updateStockRecordsOnSerialCodeUpdateModel) throws RetryLaterException, Exception {

        BlSerialProductModel blSerialProduct = updateStockRecordsOnSerialCodeUpdateModel.getNewSerialProduct();
        String initialValue = updateStockRecordsOnSerialCodeUpdateModel.getOldSerialProduct();
        try {
            getBlStockService().findAndUpdateStockRecordsForSerialCode(blSerialProduct, initialValue);
        }
        catch(final Exception ex){
            BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
                    "Exception occurred while updating the stock records on serial code change event of serial product in update stock action{} ",
                    blSerialProduct.getCode());
            return Transition.NOK;
        }
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                "Successfully updated the stock record after changing the serial product code from {} to {}",
                initialValue, blSerialProduct.getCode());
        return Transition.OK;
    }

    public BlStockService getBlStockService() {
        return blStockService;
    }

    public void setBlStockService(BlStockService blStockService) {
        this.blStockService = blStockService;
    }
}
