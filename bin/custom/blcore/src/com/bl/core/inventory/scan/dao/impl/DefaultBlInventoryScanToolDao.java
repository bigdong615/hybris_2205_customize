package com.bl.core.inventory.scan.dao.impl;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.util.Config;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;


/** *
 * @author Namrata Lohar
 */
public class DefaultBlInventoryScanToolDao implements BlInventoryScanToolDao {

    private static final Logger LOG = Logger.getLogger(DefaultBlInventoryScanToolDao.class);

    @Autowired
    private FlexibleSearchService flexibleSearchService;

    /**
     * {@inheritDoc}
     */
    @Override
    public BlInventoryLocationModel getInventoryLocationById(final String locationId) {
        final String barcodeList = "SELECT {pk} FROM {BlInventoryLocation!} WHERE {code} = ?locationId";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("locationId", locationId);
        final List<BlInventoryLocationModel> results = getFlexibleSearchService().<BlInventoryLocationModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FETCH_INVENTORY_LOC + locationId);
        return CollectionUtils.isNotEmpty(results) ? results.get(0) : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcodes) {
        final String barcodeList = "SELECT {bsp.pk} FROM {BlSerialProduct! as bsp}, {CatalogVersion as cv}, {Catalog as c}, " +
                "{ArticleApprovalStatus as aas} WHERE {cv.catalog} = {c.pk} and {bsp.catalogVersion} = {cv.pk} and {c.id} = 'blProductCatalog' and {cv.version} = 'Online'" +
                "and {aas.code} = 'approved' and {barcode} in (?barcodeList)";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("barcodeList", barcodes);
        final List<BlSerialProductModel> results = getFlexibleSearchService().<BlSerialProductModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FETCH_SERIAL_PROD + barcodes);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BlInventoryScanConfigurationModel getConfigKeyFromScanConfiguration(final String key) {
        final String barcodeList = "SELECT {pk} FROM {BlInventoryScanConfiguration!} WHERE {blScanConfigKey} = ?key";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("key", key);
        final List<BlInventoryScanConfigurationModel> results = getFlexibleSearchService().<BlInventoryScanConfigurationModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FETCH_CONFIG_VALUE + key);
        return CollectionUtils.isNotEmpty(results) ? results.get(0) : null;
    }


    /**
     * {@inheritDoc}
     */
    @Override
	public PackagingInfoModel getPackageInfoByCode(final String lastScannedItem) {
   	 final String barcodeList = "SELECT {pk} FROM {PackagingInfo!} WHERE {trackingNumber} = ?lastScannedItem";
       final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
       query.addQueryParameter(BlInventoryScanLoggingConstants.LAST_SCANNED_ITEM, lastScannedItem);
       final List<PackagingInfoModel> results = getFlexibleSearchService().<PackagingInfoModel>search(query).getResult();
       BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FETCH_CONFIG_VALUE, lastScannedItem);
       return CollectionUtils.isEmpty(results) ? null : results.get(0);
    }
    /**
 	 * {@inheritDoc}
 	 */
 	@Override
 	public Collection<PackagingInfoModel> getPackageForSerials(final Collection<String> barcodes)
 	{
 		final String barcodeList = "select distinct({pkg.pk}) from {PackagingInfo as pkg}, {BlSerialProduct as serial},{Consignment as c}, "
 				+ "{ConsignmentStatus as cs} where {pkg.serialProducts} LIKE CONCAT('%',CONCAT({serial.pk},'%')) and {pkg.consignment} = {c.pk} "
 				+ "and {c.status} = {cs.pk} and {cs.code} in ('SHIPPED', 'PARTIALLY_UNBOXED') and {serial.barcode} in (?barcodeList)";
 		final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
 		query.addQueryParameter("barcodeList", barcodes);
 		final List<PackagingInfoModel> results = getFlexibleSearchService().<PackagingInfoModel> search(query).getResult();
 		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FETCH_PACKAGE_DETAILS, barcodes, results.size());
 		return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
 	}

 	/**
 	 * {@inheritDoc}
 	 */
 	@Override
 	public Collection<ConsignmentModel> getTodaysShippingOrders()
 	{
 		BlLogger.logMessage(LOG, Level.INFO, "DefaultBlInventoryScanToolDao : getTodaysShippingOrders");
 		final StringBuilder barcodeList = new StringBuilder();
 		barcodeList.append("select distinct({c:pk}) from {Consignment as c} where ");
 		barcodeList.append(Config.isSQLServerUsed() ? "CONVERT(VARCHAR,{c:optimizedShippingStartDate},110) = ?currentDate" 
 				: "to_char({c:optimizedShippingStartDate},'MM-dd-yyyy') = ?currentDate");
 		final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
 		final String currentDateUsingCalendar = BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date());
 		query.addQueryParameter("currentDate",currentDateUsingCalendar);
 		BlLogger.logFormatMessageInfo(LOG, Level.INFO,"DefaultBlInventoryScanToolDao : PST date : {}", currentDateUsingCalendar);
 		final List<ConsignmentModel> results = getFlexibleSearchService().<ConsignmentModel> search(query).getResult();
 		BlLogger.logFormatMessageInfo(LOG, Level.INFO,"DefaultBlInventoryScanToolDao" + BlInventoryScanLoggingConstants.FETCH_OUT_ORDER_DETAILS, results.size());
 		return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
 	}

 	/**
 	 * {@inheritDoc}
 	 */
 	@Override
 	public Collection<ConsignmentModel> getAllConsignmentForSerial(final String serial)
 	{
 		BlLogger.logMessage(LOG, Level.INFO, "DefaultBlInventoryScanToolDao : getAllConsignmentForSerial");
 		final String barcodeList = "select distinct({c:pk}) from {Consignment as c}, {ConsignmentEntry as ce}, {BlSerialProduct as serial}, "
 				+ "{ConsignmentStatus as cs} where {c:status} = {cs:pk} and {ce:consignment} = {c:pk} and {ce:serialProducts} LIKE CONCAT('%',CONCAT({serial.pk},'%')) "
 				+ "and {serial.code} = ?serial and {serial.dirtyPriorityStatus} = 0 and {cs.code} in ('SHIPPED', 'PARTIALLY_UNBOXED', 'UNBOXED', 'DELIVERY_COMPLETED')";
 		final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
 		query.addQueryParameter("serial", serial);
 		final List<ConsignmentModel> results = getFlexibleSearchService().<ConsignmentModel> search(query).getResult();
 		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "DefaultBlInventoryScanToolDao" + BlInventoryScanLoggingConstants.FETCH_OUT_ORDER_SERIAL, serial, results.size());
 		return CollectionUtils.isNotEmpty(results) ? results : null;
 	}

 	/**
 	 * {@inheritDoc}
 	 */
 	@Override
 	public Collection<ConsignmentModel> getTodaysShippingConsignments(final String serial)
 	{
 		BlLogger.logMessage(LOG, Level.INFO, "DefaultBlInventoryScanToolDao : getTodaysShippingConsignments");
 		final StringBuilder barcodeList = new StringBuilder();
 		barcodeList.append("select distinct({c:pk}) from {Consignment as c}, {ConsignmentEntry as ce}, {BlSerialProduct as serial}, {ConsignmentStatus as cs} ");
 		barcodeList.append("where {ce:consignment} = {c:pk} and {ce:serialProducts} LIKE CONCAT('%',CONCAT({serial.pk},'%')) ");
 		barcodeList.append("and {serial.code} = ?serial and {serial.dirtyPriorityStatus} = 0 and "); 		
 		barcodeList.append(Config.isSQLServerUsed() ? "CONVERT(VARCHAR,{c:optimizedShippingStartDate},110) = ?currentDate" 
 				: "to_char({c:optimizedShippingStartDate},'MM-dd-yyyy') = ?currentDate");
 		final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList.toString());
 		query.addQueryParameter("serial", serial);
 		query.addQueryParameter("currentDate",
 				BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()));
 		final List<ConsignmentModel> results = getFlexibleSearchService().<ConsignmentModel> search(query).getResult();
 		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "DefaultBlInventoryScanToolDao" + BlInventoryScanLoggingConstants.FETCH_OUT_TODAYS_ORDER_SERIAL, serial, results.size());
 		return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
 	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<BlSerialProductModel> getAllSerialsByBinLocation(final String binLocationId) {
		final String serialsOnLocation = "SELECT {bsp.pk} FROM {BlSerialProduct! as bsp}, {CatalogVersion as cv}, {Catalog as c}, " +
				"{ArticleApprovalStatus as aas} WHERE {cv.catalog} = {c.pk} and {c.id} = 'blProductCatalog'" +
				"and {aas.code} = 'approved' and {bsp.ocLocation}= ?binLocationId";
		final FlexibleSearchQuery query = new FlexibleSearchQuery(serialsOnLocation);
		query.addQueryParameter(BlCoreConstants.BIN_LOCATION_ID, binLocationId);
		final List<BlSerialProductModel> results = getFlexibleSearchService().<BlSerialProductModel>search(query).getResult();
		BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FETCH_SERIAL_PROD_LOC , binLocationId);
		return CollectionUtils.isEmpty(results) ? Collections.emptyList() : results;
	}

	public FlexibleSearchService getFlexibleSearchService() {
        return flexibleSearchService;
    }

    public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService) {
        this.flexibleSearchService = flexibleSearchService;
    }
}
