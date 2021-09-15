package com.bl.core.inventory.cycle.count.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.InventoryCycleCountStatus;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.inventory.cycle.count.dao.BlInventoryCycleCountDao;
import com.bl.core.inventory.cycle.count.service.BlInventoryCycleCountService;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.model.*;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

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
    BlInventoryScanToolDao blInventoryScanToolDao;

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
    public boolean createNextInventoryCycleCount() {
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
                return Boolean.TRUE;
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.PREVIOUS_INVENTORY_CYCLE_NOT_COMPLETED_WITH_CODE,
                        activeInventoryCount.getInventoryCycleCountCode());
                return status;
            }
        }
        return Boolean.FALSE;
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
    public BlInventoryCycleCountScanHistoryModel executeInventoryCycleCount(final Collection<String> serialBarcodes) {
        final Map<BlProductModel, List<BlSerialProductModel>> scannedSKUsAndSerials = new HashMap<>();
        final List<BlSerialProductModel> missingList = new ArrayList<>();

        for(final BlProductModel sku : this.getInventorySKUList()) {
            this.makeMissingAndUnexpectedListFromScannedData(serialBarcodes, missingList, scannedSKUsAndSerials,
                    serialBarcodes, sku);
        }
        return this.logInventoryCycleCountScanHistory(scannedSKUsAndSerials, missingList, serialBarcodes);
    }

    /**
     * This method will iterate over serials of SKU and check against the scanned data and prepare different lists
     *
     * @param serialBarcodes list
     * @param scannedSKUsAndSerials map
     * @param missingList list
     * @param modifiedScannedSerials serials
     * @param sku activeSKU
     */
    private void makeMissingAndUnexpectedListFromScannedData(final Collection<String> serialBarcodes, final List<BlSerialProductModel> missingList,
                                                             final Map<BlProductModel, List<BlSerialProductModel>> scannedSKUsAndSerials,
                                                             final Collection<String> modifiedScannedSerials, final BlProductModel sku) {
        if(CollectionUtils.isNotEmpty(sku.getSerialProducts())) {
            final List<BlSerialProductModel> successScannedSerials = new ArrayList<>();
            for(final BlSerialProductModel serial : sku.getSerialProducts()) {
                if(serialBarcodes.stream().anyMatch(barcode -> barcode.equals(serial.getBarcode()))) {
                    successScannedSerials.add(serial);
                    modifiedScannedSerials.remove(serial.getBarcode());
                } else {
                    if(!SerialStatusEnum.SHIPPED.equals(serial.getSerialStatus())) {
                        missingList.add(serial);
                    }
                }
            }
            scannedSKUsAndSerials.put(sku, successScannedSerials);
        }
    }

    /**
     * This method will log the scan history to DB
     *
     * @param scannedSKUsAndSerials mainList
     * @param missingList missing serials list
     * @param modifiedScannedSerials unexpected serials list
     * @return BlInventoryCycleCountScanHistoryModel scanHistory
     */
    private BlInventoryCycleCountScanHistoryModel logInventoryCycleCountScanHistory(final Map<BlProductModel, List<BlSerialProductModel>> scannedSKUsAndSerials,
                                                                                    final List<BlSerialProductModel> missingList,
                                                                                    final Collection<String> modifiedScannedSerials) {
        final BlInventoryCycleCountScanHistoryModel blInventoryCycleCountScanHistoryModel = getModelService()
                .create(BlInventoryCycleCountScanHistoryModel.class);

        blInventoryCycleCountScanHistoryModel.setScannedSKUsAndSerials(scannedSKUsAndSerials);
        blInventoryCycleCountScanHistoryModel.setMissingSerialsForScan(missingList);
        blInventoryCycleCountScanHistoryModel.setUnexpectedScannedSerials((List<BlSerialProductModel>) this.getBlInventoryScanToolDao()
                .getSerialProductsByBarcode(modifiedScannedSerials));
        blInventoryCycleCountScanHistoryModel.setScannedUser(userService.getCurrentUser());
        blInventoryCycleCountScanHistoryModel.setScannedTime(new Date());

        getModelService().save(blInventoryCycleCountScanHistoryModel);
        getModelService().refresh(blInventoryCycleCountScanHistoryModel);

        return blInventoryCycleCountScanHistoryModel;
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
        if(CollectionUtils.isNotEmpty(subLists)) {
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

            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.CREATING_NEW_INVENTORY_CYCLE_COUNT_FROM_TO,
                    blInventoryCycleCountModel.getCurrentCycleCountStartDate(), blInventoryCycleCountModel.getCurrentCycleCountEndDate());

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

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SUCCESSFULLY_CREATED_NEW_INVENTORY_CYCLE_COUNT_FOR_CODE_FROM_TO,
                    blInventoryCycleCountModel.getInventoryCycleCountCode(), blInventoryCycleCountModel.getCurrentCycleCountStartDate(),
                    blInventoryCycleCountModel.getCurrentCycleCountEndDate());
        } else {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FAILED_TO_CREATE_INVENTORY_CYCLE_AS_SKU_LIST_IS_EMPTY);
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
    private BlInventoryCycleCountDetailsModel createBlInventoryCycleCountDetailsModel(int inventoryCycleCountCounter, final Calendar calendar,
                                                                                      Date previousDate, final List<BlProductModel> list) {
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

        BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.CREATED_INVENTORY_CYCLE_FOR_DATE,
                blInventoryCycleCountDetailsModel.getInventoryCycleCountCode(), blInventoryCycleCountDetailsModel.getInventoryCycleCountDate());
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

    public BlInventoryScanToolDao getBlInventoryScanToolDao() {
        return blInventoryScanToolDao;
    }

    public void setBlInventoryScanToolDao(BlInventoryScanToolDao blInventoryScanToolDao) {
        this.blInventoryScanToolDao = blInventoryScanToolDao;
    }
}
