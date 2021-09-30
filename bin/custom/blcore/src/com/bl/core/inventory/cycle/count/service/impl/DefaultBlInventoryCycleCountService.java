package com.bl.core.inventory.cycle.count.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.InventoryCycleCountSerialStatus;
import com.bl.core.enums.InventoryCycleCountStatus;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.inventory.cycle.count.dao.BlInventoryCycleCountDao;
import com.bl.core.inventory.cycle.count.service.BlInventoryCycleCountService;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.model.*;
import com.bl.logging.BlLogger;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.commons.lang.time.DateUtils;

import java.util.*;

/**
 * Inventory Cycle Count Service
 *
 * @author Namrata Lohar
 */
public class DefaultBlInventoryCycleCountService implements BlInventoryCycleCountService {

    private static final Logger LOG = Logger.getLogger(DefaultBlInventoryCycleCountService.class);

    private BlInventoryCycleCountDao blInventoryCycleCountDao;
    private BlInventoryScanToolDao blInventoryScanToolDao;
    private UserService userService;
    private ModelService modelService;

    private Collection<BlInventoryCycleCountProductDetailsModel> inventorySKUList;

    private String inventoryDayCode;
    private Date inventoryDayDate;

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
        final Collection<BlProductModel> results = getBlInventoryCycleCountDao().getAllActiveSKUsWithSerialStatus();
        if(CollectionUtils.isNotEmpty(results)) {
            final Collection<BlProductModel> activeSKUs = new ArrayList<>();
            for(final BlProductModel sku : results) {
                if(CollectionUtils.isNotEmpty(sku.getSerialProducts()) && StringUtils.isNotEmpty(sku.getProductId())) {
                    activeSKUs.add(sku);
                }
            }
            return CollectionUtils.isEmpty(activeSKUs) ? CollectionUtils.EMPTY_COLLECTION : activeSKUs;
        }
        return CollectionUtils.EMPTY_COLLECTION;
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
                BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.DEACTIVATED_PREVIOUS_CYCLE_COUNT_WITH_CODE,
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
                List<List<BlProductModel>> subLists;
                if(blProductModelCollection.size() < BlInventoryScanLoggingConstants.THIRTY) {
                    subLists = this.createBatch(blProductModelCollection, blProductModelCollection.size());
                } else {
                    subLists = this.createBatch(blProductModelCollection, BlInventoryScanLoggingConstants.THIRTY);
                }
                this.createBlInventoryCycleCountModel(activeInventoryCount, subLists);
                return Boolean.TRUE;
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.PREVIOUS_INVENTORY_CYCLE_NOT_COMPLETED_WITH_CODE,
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
        final Optional<BlInventoryCycleCountDetailsModel> blInventoryCycleCountDetailsModel = this.getAllActiveSKUs();
        if(blInventoryCycleCountDetailsModel.isPresent() && CollectionUtils.isNotEmpty(inputList) &&
            CollectionUtils.isNotEmpty(blInventoryCycleCountDetailsModel.get().getInventoryCycleCountSKUs())) {
            for(BlInventoryCycleCountProductDetailsModel sku : blInventoryCycleCountDetailsModel.get().getInventoryCycleCountSKUs()) {
                if(inputList.stream().noneMatch(input -> sku.getInventoryCycleCountProductId().equals(input))) {
                    return Boolean.FALSE;
                }
            }
            this.setInventorySKUList(blInventoryCycleCountDetailsModel.get().getInventoryCycleCountSKUs());
            this.setInventoryDayCode(blInventoryCycleCountDetailsModel.get().getInventoryCycleCountCode());
            this.setInventoryDayDate(blInventoryCycleCountDetailsModel.get().getInventoryCycleCountDate());
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String executeInventoryCycleCount(final Collection<String> serialBarcodes) {
        final Map<BlProductModel, List<BlSerialProductModel>> scannedSKUsAndSerials = new HashMap<>();
        final List<BlSerialProductModel> missingList = new ArrayList<>();
        
        for(final BlInventoryCycleCountProductDetailsModel sku : this.getInventorySKUList()) {
            this.makeMissingAndUnexpectedListFromScannedData(serialBarcodes, missingList, scannedSKUsAndSerials,
                    serialBarcodes, sku.getInventoryCycleCountProduct());
        }

        final BlInventoryCycleCountModel blInventoryCycleCountModel = this.getActiveInventoryCycleCount();
        if(null != blInventoryCycleCountModel){
            this.isCurrentCycleCountDaysEnded(blInventoryCycleCountModel);
            if(Boolean.TRUE.equals(this.logInventoryCycleCountScanHistory(scannedSKUsAndSerials, missingList, serialBarcodes))) {
                return blInventoryCycleCountModel.getInventoryCycleCountCode();
            }
        }
        return StringUtils.EMPTY;
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
                this.createMissingAndSuccessScanList(serialBarcodes, missingList, modifiedScannedSerials, successScannedSerials, serial);
            }
            scannedSKUsAndSerials.put(sku, successScannedSerials);
        }
    }

    /**
     * This method will create missing and success scanned list
     *
     * @param serialBarcodes barcodes
     * @param missingList list
     * @param modifiedScannedSerials serials
     * @param successScannedSerials success
     * @param serial serial
     */
    private void createMissingAndSuccessScanList(final Collection<String> serialBarcodes, final List<BlSerialProductModel> missingList,
                                                 final Collection<String> modifiedScannedSerials, final List<BlSerialProductModel> successScannedSerials,
                                                 final BlSerialProductModel serial) {
        if(serialBarcodes.stream().anyMatch(barcode -> barcode.equals(serial.getBarcode()))) {
            successScannedSerials.add(serial);
            modifiedScannedSerials.remove(serial.getBarcode());
        } else {
            if(SerialStatusEnum.SHIPPED.equals(serial.getSerialStatus())) {
                if(serial.getAssociatedShippedConsignment() != null && DateUtils.isSameDay(Calendar.getInstance().getTime(),
                        serial.getAssociatedShippedConsignment().getOptimizedShippingEndDate())) {
                    missingList.add(serial);
                }
            } else {
                missingList.add(serial);
            }
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
    private boolean logInventoryCycleCountScanHistory(final Map<BlProductModel, List<BlSerialProductModel>> scannedSKUsAndSerials,
                                                      final List<BlSerialProductModel> missingList, final Collection<String> modifiedScannedSerials) {
        boolean successStatus = Boolean.FALSE;
        boolean missingStatus = Boolean.FALSE;
        boolean unExpectedStatus = Boolean.FALSE;

        if(MapUtils.isNotEmpty(scannedSKUsAndSerials)) {
            final Collection<List<BlSerialProductModel>> allSerials = scannedSKUsAndSerials.values();
            for(final List<BlSerialProductModel> serials : allSerials) {
                successStatus = this.generateAndLogICCScanHistory(serials, InventoryCycleCountSerialStatus.REGULAR);
            }
        }

        if(CollectionUtils.isNotEmpty(missingList)) {
            missingStatus = this.generateAndLogICCScanHistory(missingList, InventoryCycleCountSerialStatus.MISSING);
        }

        if(CollectionUtils.isNotEmpty(modifiedScannedSerials)) {
            final List<BlSerialProductModel> allUnexpectedSerial = (List<BlSerialProductModel>) this.getBlInventoryScanToolDao()
                    .getSerialProductsByBarcode(modifiedScannedSerials);
            if(CollectionUtils.isNotEmpty(allUnexpectedSerial)) {
                unExpectedStatus = this.generateAndLogICCScanHistory(allUnexpectedSerial, InventoryCycleCountSerialStatus.UNEXPECTED);
            }
        }

        this.setDayStatusToCompleted(successStatus, missingStatus, unExpectedStatus);
        return Boolean.TRUE.equals(successStatus) && (Boolean.FALSE.equals(missingStatus) && Boolean.FALSE.equals(unExpectedStatus))
                    ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * This method will set status of ICC_DAY_* as COMPLETED is history logged for the ICC_Day
     *
     * @param successStatus success
     * @param missingStatus missing
     * @param unExpectedStatus unexpected
     */
    private void setDayStatusToCompleted(final boolean successStatus, final boolean missingStatus, final boolean unExpectedStatus) {
        if(Boolean.TRUE.equals(successStatus) || Boolean.TRUE.equals(missingStatus) || Boolean.TRUE.equals(unExpectedStatus)) {
            final Optional<BlInventoryCycleCountDetailsModel> blInventoryCycleCountDetailsModel = this.getAllActiveSKUs();
            if (blInventoryCycleCountDetailsModel.isPresent()) {
                final BlInventoryCycleCountDetailsModel detailsModel = blInventoryCycleCountDetailsModel.get();
                detailsModel.setInventoryCycleCountDetailStatus(InventoryCycleCountStatus.COMPLETED);
                getModelService().save(detailsModel);
                getModelService().refresh(detailsModel);

                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SET_INVENTORY_CYCLE_COUNT_DAY_COMPLETED,
                        this.getInventoryDayCode());
            }
        }
    }

    /**
     * This method will log history on serials
     *
     * @param allSerial serials
     * @param serialStatus status
     */
    private boolean generateAndLogICCScanHistory(final List<BlSerialProductModel> allSerial, final InventoryCycleCountSerialStatus serialStatus) {
        boolean status = Boolean.FALSE;
        for(final BlSerialProductModel serial : allSerial) {
            status = Boolean.TRUE;
            final BlInventoryCycleCountScanHistoryModel blInventoryCycleCountScanHistoryModel = getModelService()
                    .create(BlInventoryCycleCountScanHistoryModel.class);

            blInventoryCycleCountScanHistoryModel.setInventoryCycleCountCode(this.getActiveInventoryCycleCount() != null ?
                    this.getActiveInventoryCycleCount().getInventoryCycleCountCode() : StringUtils.EMPTY);
            blInventoryCycleCountScanHistoryModel.setInventoryCycleCountDayCode(this.getInventoryDayCode());
            blInventoryCycleCountScanHistoryModel.setInventoryCycleCountDayDate(this.getInventoryDayDate());
            blInventoryCycleCountScanHistoryModel.setInventoryCycleCountSerialStatus(serialStatus);
            blInventoryCycleCountScanHistoryModel.setScannedUser(userService.getCurrentUser());
            blInventoryCycleCountScanHistoryModel.setScannedTime(new Date());
            blInventoryCycleCountScanHistoryModel.setSerialNumber(serial.getCode());
            blInventoryCycleCountScanHistoryModel.setBarcodeNumber(serial.getBarcode());
            this.seRunReportSpecificDataOnHistory(blInventoryCycleCountScanHistoryModel, serial);
            this.setOrderLevelValuesOnScanHistory(serial, blInventoryCycleCountScanHistoryModel);

            getModelService().save(blInventoryCycleCountScanHistoryModel);
            getModelService().refresh(blInventoryCycleCountScanHistoryModel);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.CREATING_NEW_PRODUCT_INVENTORY_CYCLE_COUNT_HISTORY,
                    this.getInventoryDayCode(), blInventoryCycleCountScanHistoryModel.getScannedUser());
        }
        return status;
    }

    /**
     * This method will set some data specific to RUN Report on history
     *
     * @param blInventoryCycleCountScanHistoryModel model
     * @param serial serial
     */
    private void seRunReportSpecificDataOnHistory(final BlInventoryCycleCountScanHistoryModel blInventoryCycleCountScanHistoryModel,
                                                  final BlSerialProductModel serial) {
        blInventoryCycleCountScanHistoryModel.setHomeBaseLocation(serial.getWarehouseLocation() == null ? StringUtils.EMPTY
                : serial.getWarehouseLocation().getName());
        blInventoryCycleCountScanHistoryModel.setProductName(serial.getBlProduct() != null ? serial.getBlProduct().getName()
                : serial.getName());
        blInventoryCycleCountScanHistoryModel.setOc(serial.getOcLocation());
        final String ocParentLocation = serial.getOcLocationDetails() != null && serial.getOcLocationDetails()
                .getParentInventoryLocation() != null ? serial.getOcLocationDetails().getParentInventoryLocation().getCode()
                : StringUtils.EMPTY;
        blInventoryCycleCountScanHistoryModel.setOcParent(serial.getOcLocationDetails() != null ? ocParentLocation : StringUtils.EMPTY);
        blInventoryCycleCountScanHistoryModel.setProductStatus(serial.getSerialStatus() != null ? serial.getSerialStatus().getCode() :
                StringUtils.EMPTY);
    }

    /**
     * This method will set some order level attributes on scan history model
     *
     * @param serial serial
     * @param blInventoryCycleCountScanHistoryModel model
     */
    private void setOrderLevelValuesOnScanHistory(final BlSerialProductModel serial, final BlInventoryCycleCountScanHistoryModel blInventoryCycleCountScanHistoryModel) {
        if(serial.getAssociatedShippedConsignment() != null) {
            final ConsignmentModel consignmentModel = serial.getAssociatedShippedConsignment();
            blInventoryCycleCountScanHistoryModel.setLastOrderShippedDate(consignmentModel.getOptimizedShippingStartDate());
            blInventoryCycleCountScanHistoryModel.setOrderStatus(consignmentModel.getStatus() == null ? StringUtils.EMPTY :
                    consignmentModel.getStatus().getCode());
            blInventoryCycleCountScanHistoryModel.setLastOrderNumber(consignmentModel.getOrder() == null ? StringUtils.EMPTY :
                    consignmentModel.getOrder().getCode());
        } else {
            blInventoryCycleCountScanHistoryModel.setLastOrderShippedDate(null);
            blInventoryCycleCountScanHistoryModel.setOrderStatus(null);
            blInventoryCycleCountScanHistoryModel.setLastOrderNumber(null);
        }
    }

    /**
     * This method will create BlInventoryCycleCountModel
     *
     * @param previousInventoryCount count
     * @param subLists list
     */
    private void createBlInventoryCycleCountModel(final BlInventoryCycleCountModel previousInventoryCount,
                                                  final Collection<List<BlProductModel>> subLists) {
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

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.CREATING_NEW_INVENTORY_CYCLE_COUNT_FROM_TO,
                    blInventoryCycleCountModel.getCurrentCycleCountStartDate(), blInventoryCycleCountModel.getCurrentCycleCountEndDate());

            final List<BlInventoryCycleCountDetailsModel> allSKUDetailsPerDay = new ArrayList<>();
            final Calendar calendar = Calendar.getInstance();
            int inventoryCycleCountCounter = BlInventoryScanLoggingConstants.ONE;
            Date previousDate = new Date();
            for (final List<BlProductModel> list : subLists) {
                final BlInventoryCycleCountDetailsModel blInventoryCycleCountDetailsModel = this.createBlInventoryCycleCountDetailsModel(
                        inventoryCycleCountCounter, calendar, previousDate, list);
                allSKUDetailsPerDay.add(blInventoryCycleCountDetailsModel);
                inventoryCycleCountCounter = inventoryCycleCountCounter + BlInventoryScanLoggingConstants.ONE;
                previousDate = blInventoryCycleCountDetailsModel.getInventoryCycleCountDate();
            }
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
    private BlInventoryCycleCountDetailsModel createBlInventoryCycleCountDetailsModel(final int inventoryCycleCountCounter, final Calendar calendar,
                                                                                      final Date previousDate, final List<BlProductModel> list) {
        final BlInventoryCycleCountDetailsModel blInventoryCycleCountDetailsModel = getModelService().create(
                BlInventoryCycleCountDetailsModel.class);
        blInventoryCycleCountDetailsModel.setInventoryCycleCountDate(this.getNextWorkingDate(previousDate, calendar, inventoryCycleCountCounter));
        blInventoryCycleCountDetailsModel.setInventoryCycleCountCode(BlInventoryScanLoggingConstants.ICC_DAY + inventoryCycleCountCounter);
        final List<BlInventoryCycleCountProductDetailsModel> blInventoryCycleCountProductDetailsModels = new ArrayList<>();
        for(BlProductModel productModel : list) {
            final BlInventoryCycleCountProductDetailsModel blInventoryCycleCountProductDetailsModel = getModelService().create(
                    BlInventoryCycleCountProductDetailsModel.class);
            blInventoryCycleCountProductDetailsModel.setInventoryCycleCountProduct(productModel);
            blInventoryCycleCountProductDetailsModel.setInventoryCycleCountProductId(productModel.getProductId());
            blInventoryCycleCountProductDetailsModel.setInventoryCycleCountDayCode(blInventoryCycleCountDetailsModel.getInventoryCycleCountCode());
            getModelService().save(blInventoryCycleCountProductDetailsModel);
            getModelService().refresh(blInventoryCycleCountProductDetailsModel);
            blInventoryCycleCountProductDetailsModels.add(blInventoryCycleCountProductDetailsModel);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.CREATING_NEW_PRODUCT_INVENTORY_CYCLE_COUNT_FOR_SKU,
                    blInventoryCycleCountDetailsModel.getInventoryCycleCountCode(), productModel.getProductId());
        }
        blInventoryCycleCountDetailsModel.setInventoryCycleCountSKUs(blInventoryCycleCountProductDetailsModels);
        getModelService().save(blInventoryCycleCountDetailsModel);
        getModelService().refresh(blInventoryCycleCountDetailsModel);
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.CREATED_INVENTORY_CYCLE_FOR_DATE,
                blInventoryCycleCountDetailsModel.getInventoryCycleCountCode(), blInventoryCycleCountDetailsModel.getInventoryCycleCountDate());
        return blInventoryCycleCountDetailsModel;

    }

    /**
     * This method will set ICC COMPLETED
     * @param blInventoryCycleCountModel cycleCount
     */
    public void isCurrentCycleCountDaysEnded(final BlInventoryCycleCountModel blInventoryCycleCountModel) {
        final Collection<BlInventoryCycleCountDetailsModel> allCurrentCycleCountDetails = blInventoryCycleCountModel
                .getInventoryCycleCountProducts();
        if(CollectionUtils.isNotEmpty(allCurrentCycleCountDetails) && allCurrentCycleCountDetails.stream().noneMatch(
            entry -> InventoryCycleCountStatus.READY.equals(entry.getInventoryCycleCountDetailStatus()))) {
            BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.PREVIOUS_CYCLE_COUNT_ENDED_FOR_CODE,
                    blInventoryCycleCountModel.getInventoryCycleCountCode());
            blInventoryCycleCountModel.setInventoryCycleCountStatus(InventoryCycleCountStatus.COMPLETED);
            blInventoryCycleCountModel.setInventoryCycleCountActive(Boolean.FALSE);
            getModelService().save(blInventoryCycleCountModel);
            getModelService().refresh(blInventoryCycleCountModel);
        }
    }

    /**
     * This method will partition into THIRTY lists
     *
     * @param originalList list
     * @param batch_size partition list
     * @return all partitions
     */
    private List<List<BlProductModel>> createBatch(final Collection<BlProductModel> originalList, final int batch_size) {
        final int Length = originalList.size();
        final int chunkSize = Length / batch_size;
        final int residual = Length-chunkSize*batch_size;
        final List<Integer> listNums = new ArrayList<>();
        for (int i = 0; i < batch_size; i++) {
            listNums.add(chunkSize);
        }
        for (int i = 0; i < residual; i++) {
            listNums.set(i, listNums.get(i) + 1);
        }
        final List<Integer> listIndex = new ArrayList<>();
        int cumulative = 0;
        for (int i = 0; i < batch_size; i++) {
            listIndex.add(cumulative);
            cumulative += listNums.get(i);
        }
        listIndex.add(cumulative);
        final List<List<BlProductModel>> listOfChunks = new ArrayList<>();
        for (int i = 0; i < batch_size; i++) {
            listOfChunks.add(new ArrayList<>(originalList).subList(listIndex.get(i), listIndex.get(i + 1)));
        }
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.CHUNKS_FOR_THIRTY_DAYS, listOfChunks);
        return listOfChunks;
    }

    /**
     * This method will return next working date
     *
     * @param previousDate date
     * @param calendar calender
     * @param inventoryDayCounter counter
     * @return date
     */
    private Date getNextWorkingDate(final Date previousDate, final Calendar calendar, final int inventoryDayCounter) {
        if(inventoryDayCounter == BlInventoryScanLoggingConstants.ONE) {
            calendar.setTime(new Date());
        } else {
            calendar.setTime(previousDate);
            int dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK);
            if (dayOfWeek == Calendar.FRIDAY) {
                calendar.add(Calendar.DATE, BlInventoryScanLoggingConstants.THREE);
            } else {
                calendar.add(Calendar.DATE, BlInventoryScanLoggingConstants.ONE);
            }
        }
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.CALENDER_TIME, calendar.getTime());
        return calendar.getTime();
    }

    public BlInventoryCycleCountDao getBlInventoryCycleCountDao() {
        return blInventoryCycleCountDao;
    }

    public void setBlInventoryCycleCountDao(final BlInventoryCycleCountDao blInventoryCycleCountDao) {
        this.blInventoryCycleCountDao = blInventoryCycleCountDao;
    }

    public UserService getUserService() {
        return userService;
    }

    public void setUserService(final UserService userService) {
        this.userService = userService;
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(final ModelService modelService) {
        this.modelService = modelService;
    }

    public BlInventoryScanToolDao getBlInventoryScanToolDao() {
        return blInventoryScanToolDao;
    }

    public void setBlInventoryScanToolDao(final BlInventoryScanToolDao blInventoryScanToolDao) {
        this.blInventoryScanToolDao = blInventoryScanToolDao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getInventoryDayCode() {
        return inventoryDayCode;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setInventoryDayCode(final String inventoryDayCode) {
        this.inventoryDayCode = inventoryDayCode;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Date getInventoryDayDate() {
        return inventoryDayDate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setInventoryDayDate(final Date inventoryDayDate) {
        this.inventoryDayDate = inventoryDayDate;
    }

    public Collection<BlInventoryCycleCountProductDetailsModel> getInventorySKUList() {
        return inventorySKUList;
    }

    public void setInventorySKUList(final Collection<BlInventoryCycleCountProductDetailsModel> inventorySKUList) {
        this.inventorySKUList = inventorySKUList;
    }
}
