package com.bl.core.inventory.cycle.count.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.cycle.count.dao.BlInventoryCycleCountDao;
import com.bl.core.inventory.cycle.count.service.BlInventoryCycleCountService;
import com.bl.core.model.BlInventoryCycleCountModel;
import com.bl.core.model.BlProductModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import javolution.io.Struct;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collection;
import java.util.Date;

/**
 * @author Namrata Lohar
 */
public class DefaultBlInventoryCycleCountService implements BlInventoryCycleCountService {

    private static final Logger LOG = Logger.getLogger(DefaultBlInventoryCycleCountService.class);
    public static final String NO_ACTIVE_INVENTORY_CYCLE_FOUND = "No active inventory cycle found!!";

    @Autowired
    BlInventoryCycleCountDao blInventoryCycleCountDao;

    @Autowired
    UserService userService;

    @Autowired
    ModelService modelService;

    /**
     * {@inheritDoc}
     */
    @Override
    public BlInventoryCycleCountModel getActiveInventoryCycleCount() {
        return getBlInventoryCycleCountDao().getActiveInventoryCycleCount();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlProductModel> getAllActiveSKUsWithSerialStatus() {
        return getBlInventoryCycleCountDao().getAllActiveSKUsWithSerialStatus();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isCurrentCycleEnded(final BlInventoryCycleCountModel blInventoryCycleCountModel) {
        if(null != blInventoryCycleCountModel && blInventoryCycleCountModel.getCurrentCycleCountEndDate().equals(new Date())) {
            blInventoryCycleCountModel.setInventoryCycleCountActive(Boolean.FALSE);
            getModelService().save(blInventoryCycleCountModel);
            getModelService().refresh(blInventoryCycleCountModel);
            return Boolean.TRUE;
        } else {
            BlLogger.logMessage(LOG, Level.DEBUG, NO_ACTIVE_INVENTORY_CYCLE_FOUND);
            return Boolean.FALSE;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createNextInventoryCycleCount() {
        final Collection<BlProductModel> blProductModelCollection = this.getAllActiveSKUsWithSerialStatus();
        if(CollectionUtils.isNotEmpty(blProductModelCollection)) {
            final double perDaySKUs = blProductModelCollection.size() / 30;
        }
    }

    public BlInventoryCycleCountDao getBlInventoryCycleCountDao() {
        return blInventoryCycleCountDao;
    }

    public void setBlInventoryCycleCountDao(BlInventoryCycleCountDao blInventoryCycleCountDao) {
        this.blInventoryCycleCountDao = blInventoryCycleCountDao;
    }

    public UserService getUserService() {
        return userService;
    }

    public void setUserService(UserService userService) {
        this.userService = userService;
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }
}
