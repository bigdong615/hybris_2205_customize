package com.bl.core.inventory.cycle.count.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.InventoryCycleCountStatus;
import com.bl.core.inventory.cycle.count.dao.BlInventoryCycleCountDao;
import com.bl.core.inventory.cycle.count.service.BlInventoryCycleCountService;
import com.bl.core.model.BlInventoryCycleCountDetailsModel;
import com.bl.core.model.BlInventoryCycleCountModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Inventory Cycle Count Service
 *
 * @author Namrata Lohar
 */
public class DefaultBlInventoryCycleCountService implements BlInventoryCycleCountService {

    private static final Logger LOG = Logger.getLogger(DefaultBlInventoryCycleCountService.class);

    @Autowired
    BlInventoryCycleCountDao blInventoryCycleCountDao;

    @Autowired
    UserService userService;

    @Autowired
    ModelService modelService;

    private Collection<BlProductModel> inventorySKUList;

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
        final Collection<BlInventoryCycleCountDetailsModel> allCurrentCycleCountDetails = blInventoryCycleCountModel
                .getInventoryCycleCountProducts();
        if(CollectionUtils.isNotEmpty(allCurrentCycleCountDetails)) {
            if(allCurrentCycleCountDetails.stream().anyMatch(entry -> InventoryCycleCountStatus.READY.equals(
                    entry.getInventoryCycleCountDetailStatus()))) {
                BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.PREVIOUS_CYCLE_COUNT_NOT_ENDED_YET_FOR_CODE,
                        blInventoryCycleCountModel.getInventoryCycleCountCode());
                return Boolean.FALSE;
            } else {
                BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.DEACTIVATING_PREVIOUS_CYCLE_COUNT_WITH_CODE,
                        blInventoryCycleCountModel.getInventoryCycleCountCode());
                blInventoryCycleCountModel.setInventoryCycleCountActive(Boolean.FALSE);
                getModelService().save(blInventoryCycleCountModel);
                getModelService().refresh(blInventoryCycleCountModel);
                return Boolean.TRUE;
            }
        }
        return Boolean.FALSE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createNextInventoryCycleCount() {
        final Collection<BlProductModel> blProductModelCollection = this.getAllActiveSKUsWithSerialStatus();
        if(CollectionUtils.isNotEmpty(blProductModelCollection)) {
            boolean status = Boolean.TRUE;
            final BlInventoryCycleCountModel activeInventoryCount = this.getActiveInventoryCycleCount();
            if(null != activeInventoryCount) {
                status = this.isCurrentCycleEnded(activeInventoryCount);
            }
            if(Boolean.TRUE.equals(status)) {
                final int perDaySKUs = blProductModelCollection.size() / BlInventoryScanLoggingConstants.THIRTY;
                final List<List<BlProductModel>> subLists = Lists.partition((List) blProductModelCollection, perDaySKUs);
                this.createBlInventoryCycleCountModel(activeInventoryCount, subLists, perDaySKUs, blProductModelCollection);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Optional<BlInventoryCycleCountDetailsModel> getAllActiveSKUs() {
        final BlInventoryCycleCountModel activeInventoryCycleCount = this.getActiveInventoryCycleCount();
        if(null != activeInventoryCycleCount) {
            final Collection<BlInventoryCycleCountDetailsModel> allICCDaysSKUs = activeInventoryCycleCount.getInventoryCycleCountProducts();
            if(CollectionUtils.isNotEmpty(allICCDaysSKUs)) {
                return allICCDaysSKUs.stream().filter(entry -> InventoryCycleCountStatus.READY.equals(entry.getInventoryCycleCountDetailStatus())).findFirst();
            }
        }
        return Optional.empty();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean checkIsSKUListMatching(final Collection<String> inputList) {
        final BlInventoryCycleCountDetailsModel blInventoryCycleCountDetailsModel = this.getAllActiveSKUs().orElse(null);
        if(blInventoryCycleCountDetailsModel != null && CollectionUtils.isNotEmpty(inputList) && CollectionUtils.isNotEmpty(
                blInventoryCycleCountDetailsModel.getInventoryCycleCountSKUs())) {
            for(BlProductModel sku : blInventoryCycleCountDetailsModel.getInventoryCycleCountSKUs()) {
                if(inputList.stream().noneMatch(input -> sku.getCode().equals(input))) {
                    return Boolean.FALSE;
                }
            }
            this.setInventorySKUList(blInventoryCycleCountDetailsModel.getInventoryCycleCountSKUs());
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void executeInventoryCycleCount(final Collection<String> serialBarcodes) {
        final Collection<BlProductModel> missingList = new ArrayList<>();
        final Collection<BlProductModel> unexpectedSerialList = new ArrayList<>();
        final Collection<BlProductModel> allSKUList = this.getInventorySKUList();
        if(CollectionUtils.isNotEmpty(allSKUList)) {
            for(final BlProductModel sku : allSKUList) {
                final Collection<BlSerialProductModel> allSerials = sku.getSerialProducts();
                if(CollectionUtils.isNotEmpty(allSerials)) {
                    for(final BlSerialProductModel serial : allSerials) {
                        //TODO: check missing and unexpected and log history of scanning
                    }
                }
            }
        }
    }

    /**
     * This method will create BlInventoryCycleCountModel
     *
     * @param previousInventoryCount count
     * @param subLists list
     * @param perDaySKUs skus
     * @param blProductModelCollection list
     */
    private void createBlInventoryCycleCountModel(final BlInventoryCycleCountModel previousInventoryCount,
                                                  final Collection<List<BlProductModel>> subLists, final int perDaySKUs,
                                                  final Collection<BlProductModel> blProductModelCollection) {
        final BlInventoryCycleCountModel blInventoryCycleCountModel = getModelService().create(BlInventoryCycleCountModel.class);
        blInventoryCycleCountModel.setCurrentCycleCountStartDate(new Date());
        if(null == previousInventoryCount) {
            blInventoryCycleCountModel.setPreviousCycleCountStartDate(null);
            blInventoryCycleCountModel.setPreviousCycleCountEndDate(null);
        } else {
            blInventoryCycleCountModel.setPreviousCycleCountStartDate(previousInventoryCount.getCurrentCycleCountStartDate());
            blInventoryCycleCountModel.setPreviousCycleCountEndDate(previousInventoryCount.getCurrentCycleCountEndDate());
        }
        blInventoryCycleCountModel.setInventoryCycleCountActive(Boolean.TRUE);
        if(CollectionUtils.isNotEmpty(subLists)) {
            final List<BlInventoryCycleCountDetailsModel> allSKUDetailsPerDay = new ArrayList<>();
            final Calendar calendar = Calendar.getInstance();
            int inventoryCycleCountCounter = BlInventoryScanLoggingConstants.ZERO;
            Date previousDate = new Date();
            for (final List<BlProductModel> list : subLists) {
                allSKUDetailsPerDay.add(this.createBlInventoryCycleCountDetailsModel(inventoryCycleCountCounter, calendar, previousDate, list));
            }
            final List<BlProductModel> tail = ((List) blProductModelCollection).subList((perDaySKUs *
                    BlInventoryScanLoggingConstants.TWENTY_NINE) + BlInventoryScanLoggingConstants.ONE, blProductModelCollection.size());
            allSKUDetailsPerDay.add(this.createBlInventoryCycleCountDetailsModel(inventoryCycleCountCounter, calendar, previousDate, tail));
            blInventoryCycleCountModel.setInventoryCycleCountProducts(allSKUDetailsPerDay);
            blInventoryCycleCountModel.setCurrentCycleCountEndDate(previousDate);
            getModelService().save(blInventoryCycleCountModel);
            getModelService().refresh(blInventoryCycleCountModel);
        }
    }

    /**
     * This method will create BlInventoryCycleCountDetailsModel model on inventoryCycleCount
     *
     * @param inventoryCycleCountCounter counter
     * @param calendar calender
     * @param previousDate date
     * @param list list
     * @return BlInventoryCycleCountDetailsModel model
     */
    private BlInventoryCycleCountDetailsModel createBlInventoryCycleCountDetailsModel(int inventoryCycleCountCounter,
                                                                                      final Calendar calendar, Date previousDate,
                                                         final List<BlProductModel> list) {
        final BlInventoryCycleCountDetailsModel blInventoryCycleCountDetailsModel = getModelService().create(
                BlInventoryCycleCountDetailsModel.class);
        blInventoryCycleCountDetailsModel.setInventoryCycleCountDate(this.getNextWorkingDate(previousDate, calendar));
        previousDate = blInventoryCycleCountDetailsModel.getInventoryCycleCountDate();
        inventoryCycleCountCounter = inventoryCycleCountCounter + BlInventoryScanLoggingConstants.ONE;
        blInventoryCycleCountDetailsModel.setInventoryCycleCountCode(BlInventoryScanLoggingConstants.ICC_DAY
                + inventoryCycleCountCounter);
        blInventoryCycleCountDetailsModel.setInventoryCycleCountSKUs(list);
        getModelService().save(blInventoryCycleCountDetailsModel);
        getModelService().refresh(blInventoryCycleCountDetailsModel);
        return blInventoryCycleCountDetailsModel;
    }

    /**
     * This method will return next working date
     * @param previousDate date
     * @param calendar calender
     * @return date
     */
    private Date getNextWorkingDate(final Date previousDate, final Calendar calendar) {
        calendar.setTime(previousDate);
        int dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK);
        if (dayOfWeek == Calendar.FRIDAY) {
            calendar.add(Calendar.DATE, BlInventoryScanLoggingConstants.THREE);
        } else {
            calendar.add(Calendar.DATE, BlInventoryScanLoggingConstants.ONE);
        }
        return calendar.getTime();
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

    public Collection<BlProductModel> getInventorySKUList() {
        return inventorySKUList;
    }

    public void setInventorySKUList(Collection<BlProductModel> inventorySKUList) {
        this.inventorySKUList = inventorySKUList;
    }
}
