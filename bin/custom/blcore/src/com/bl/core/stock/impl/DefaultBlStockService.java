package com.bl.core.stock.impl;

import de.hybris.platform.catalog.enums.ArticleApprovalStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.exceptions.BusinessException;
import de.hybris.platform.servicelayer.exceptions.ModelRemovalException;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.stock.BlStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;


/**
 * It is used to create the stock level
 *
 * @author Moumita
 */
public class DefaultBlStockService implements BlStockService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlStockService.class);
	private ModelService modelService;
	private BlProductDao productDao;
	private BlStockLevelDao blStockLevelDao;

	/**
	 * {@inheritDoc}
	 *
	 * @throws BusinessException
	 */
	@Override
	public void createStockLevelForSkuProductsByDate(final List<BlProductModel> skus, Date startDate, Date endDate)
			throws BusinessException
	{
		if (null != startDate && null != endDate && startDate.before(endDate)) {
			startDate = (BlDateTimeUtils.getFormattedStartDay(startDate)).getTime();
			endDate = (BlDateTimeUtils.getFormattedStartDay(endDate)).getTime();
			endDate = DateUtils.addDays(endDate, 1);
			final Date lastFutureDate = BlDateTimeUtils
					.getFormattedStartDay(DateUtils.addDays(new Date(), 365))
					.getTime();
			if (lastFutureDate.after(startDate) && lastFutureDate.after(endDate)) {
				final Collection<BlProductModel> skuProducts = getSkuProducts(skus);
				createStockLevelForSerialProductsForGivenDates(skuProducts, startDate, endDate);
			} else {
				BlLogger.logFormatMessageInfo(LOG, Level.WARN,
						"Stock can only be created till 365 days from today"
								+ " and the selected duration from start date {} and end date {} is beyond 365 days",
						startDate, endDate);
			}
		}
		else
		{
			throw new BusinessException("Start and end date can not be null Or Start date should be before end date");
		}
	}

	/**
	 * It get the sku products
	 * @param skus
	 * @return Collection<BlProductModel>
	 */
	private Collection<BlProductModel> getSkuProducts(final List<BlProductModel> skus)
	{
		if (CollectionUtils.isEmpty(skus))
		{
			return getProductDao().getAllActiveSkuProducts();
		}
		return skus.stream().filter(sku -> sku.getApprovalStatus().equals(ArticleApprovalStatus.APPROVED))
				.collect(Collectors.toList());
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createOneDayStockLevelForAllSkuProducts() {
		final Collection<BlProductModel> skuProducts = getProductDao().getAllActiveSkuProducts();
		createStockLevelForAllActiveSerialProducts(skuProducts, BlDateTimeUtils.getNextYearsSameDay());
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createStockRecordsForNewSerialProducts(final BlSerialProductModel blSerialProduct)
	{
		final LocalDate currentDate = LocalDate.now();
		final LocalDate formattedEndDate = BlDateTimeUtils.getNextYearsSameDay().toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		//This check is added for safer side. If user changing the statuses from one status to another.
		//So, to avoid creating stocks this check is added.
		final Collection<StockLevelModel> stockLevels = getStockLevelModelsBasedOnDates(blSerialProduct);
		if (CollectionUtils.isEmpty(stockLevels))
		{
				Stream.iterate(currentDate, date -> date.plusDays(1)).limit(ChronoUnit.DAYS.between(currentDate, formattedEndDate))
						.forEach(date -> createStockLevelForSerial(blSerialProduct,
								Date.from(date.atStartOfDay(ZoneId.systemDefault()).toInstant())));
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.WARN, "Stock already exist for serial product {} ", blSerialProduct.getCode());
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void findAndDeleteStockRecords(final BlSerialProductModel blSerialProduct)
	{
		final Collection<StockLevelModel> stockLevels = getStockLevelModelsBasedOnDates(blSerialProduct);
		if (CollectionUtils.isNotEmpty(stockLevels))
		{
			try {
				getModelService().removeAll(stockLevels);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock records removed for serial product {} "
								+ "to end date", blSerialProduct.getCode());
				if (null != blSerialProduct.getWarehouseLocation())
				{
					//creating stock level for a particular serial because serial is no more a rental product now
					//and it becomes the used gear product.
					createStockLevelForSerial(blSerialProduct, null);
				}
			} catch(final ModelRemovalException ex) {
				BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
						"Exception occurred while deleting the stock records for the serial product {} ",
						blSerialProduct.getCode());
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void findAndUpdateStockRecords(final BlSerialProductModel blSerialProduct, final boolean reservedStatus)
	{
		if (Boolean.FALSE.equals(blSerialProduct.getForRent())) {
			final StockLevelModel stockLevel = getBlStockLevelDao().findStockLevelForUsedGearSerial(blSerialProduct.getCode());
			stockLevel.setSerialStatus(blSerialProduct.getSerialStatus());
			saveStockRecord(stockLevel, reservedStatus);
		} else {
			final Collection<StockLevelModel> stockLevels = getStockLevelModelsBasedOnDates(
					blSerialProduct);
			stockLevels.forEach(stockLevel -> {
				stockLevel.setSerialStatus(blSerialProduct.getSerialStatus());
				saveStockRecord(stockLevel, reservedStatus);
			});
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void findAndUpdateStockRecordsForParticularDuration(final BlSerialProductModel blSerialProduct,
			final boolean reservedStatus, final Date startDate, final Date endDate)
	{
		final Collection<StockLevelModel> stockLevels = getBlStockLevelDao().findSerialStockLevelForDate(blSerialProduct.getCode(),
				startDate, endDate);
		stockLevels.forEach(stockLevel -> saveStockRecord(stockLevel, reservedStatus));
	}

	/**
	 * It saves the stock record after updates
	 * @param stockLevel
	 * @param reservedStatus
	 */
	private void saveStockRecord(final StockLevelModel stockLevel, final boolean reservedStatus)
	{
		stockLevel.setReservedStatus(reservedStatus);
		try {
			getModelService().save(stockLevel);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock {} updated for serial product {} for the date {} ",
					stockLevel.getPk(), stockLevel.getSerialProductCode(), stockLevel.getDate());
		}
		catch(final ModelSavingException ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Exception occurred while saving the stock record {} of the serial product {} for the date {} ",
					stockLevel.getPk(), stockLevel.getSerialProductCode(), stockLevel.getDate());
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void findAndUpdateWarehouseInStockRecords(final BlSerialProductModel blSerialProduct)
	{
		if (Boolean.FALSE.equals(blSerialProduct.getForRent())) {
			final StockLevelModel stockLevel = getBlStockLevelDao().findStockLevelForUsedGearSerial(blSerialProduct.getCode());
			upDateWarehouseAndSaveStockRecord(stockLevel, blSerialProduct.getWarehouseLocation());
		} else {
			final Collection<StockLevelModel> stockLevels = getStockLevelModelsBasedOnDates(
					blSerialProduct);
			stockLevels.forEach(stockLevel ->
				upDateWarehouseAndSaveStockRecord(stockLevel, blSerialProduct.getWarehouseLocation())
			);
		}
	}

	/**
	 * It fetches the stock records from current date to last future date
	 * @param blSerialProduct the serial product
	 * @return Collection<StockLevelModel>
	 */
	private Collection<StockLevelModel> getStockLevelModelsBasedOnDates(
			final BlSerialProductModel blSerialProduct) {
		final Date currentDate = Date
				.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
		final Date futureDate = BlDateTimeUtils.getNextYearsSameDay();
		return getBlStockLevelDao()
				.findSerialStockLevelForDate(blSerialProduct.getCode(),
						currentDate, futureDate);
	}

	/**
	 * It updates the warehouse in stock records and save it
	 * @param stockLevel
	 * @param warehouseLocation
	 */
	private void upDateWarehouseAndSaveStockRecord(final StockLevelModel stockLevel, final WarehouseModel warehouseLocation) {
		stockLevel.setWarehouse(warehouseLocation);
		try {
			getModelService().save(stockLevel);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Warehouse {} updated for stock record {} of serial product {} ",
					warehouseLocation.getCode(), stockLevel.getPk(), stockLevel.getSerialProductCode());
		}
		catch(final ModelSavingException ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Exception occurred while updating the warehouse {} in stock record {} after update for the serial product {} ",
					warehouseLocation.getCode(), stockLevel.getPk(), stockLevel.getSerialProductCode());
		}
	}

	/**
	 * It gets the active serials associated to the sku and creates stock for the serials
	 *
	 * @param skuProducts
	 * @param fromDate
	 * @param toDate
	 */
	private void createStockLevelForSerialProductsForGivenDates(final Collection<BlProductModel> skuProducts, final Date fromDate,
			final Date toDate)
	{
		final LocalDate formattedStartDate = fromDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		final LocalDate formattedEndDate = toDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		for (final BlProductModel skuProduct : skuProducts)
		{
			for (final BlSerialProductModel serial : skuProduct.getSerialProducts())
			{
				createStockForActiveSerials(formattedStartDate, formattedEndDate, serial);
			}
		}
	}

	/**
	 * It creates the stocks for the active serials
	 *
	 * @param formattedStartDate
	 * @param formattedEndDate
	 * @param serial
	 */
	private void createStockForActiveSerials(final LocalDate formattedStartDate, final LocalDate formattedEndDate,
			final BlSerialProductModel serial) {
		if (null != serial.getSerialStatus() && !((SerialStatusEnum.COMING_FROM_PURCHASE).equals(serial.getSerialStatus()))
				&& null != serial.getWarehouseLocation())
		{
			if (Boolean.FALSE.equals(serial.getForRent()))
			{
				findAndCreateStockLevelForUsedGearSerial(serial);
			}
			else
			{
				Stream.iterate(formattedStartDate, date -> date.plusDays(1))
						.limit(ChronoUnit.DAYS.between(formattedStartDate, formattedEndDate))
						.forEach(date -> findAndCreateStockLevelForSerial(serial, date));
			}
		}
	}

	/**
	 * It gets all the active associated serials of the sku
	 *
	 * @param skuProducts
	 * @param date
	 */
	private void createStockLevelForAllActiveSerialProducts(final Collection<BlProductModel> skuProducts, final Date date)
	{
		for (final BlProductModel skuProduct : skuProducts)
		{
			for (final BlSerialProductModel serial : skuProduct.getSerialProducts())
			{
				if (null != serial.getSerialStatus() && !((SerialStatusEnum.COMING_FROM_PURCHASE).equals(serial.getSerialStatus()))
						&& null != serial.getWarehouseLocation())
				{
					final Date stockDate = Boolean.FALSE.equals(serial.getForRent()) ? null : date;
					createStockLevelForSerial(serial, stockDate);
				}
			}
		}
	}

	/**
	 * It first checks whether the stock already exists or not, if not, it creates the stock
	 *
	 * @param serial
	 * @param localDate
	 */
	private void findAndCreateStockLevelForSerial(final BlSerialProductModel serial, final LocalDate localDate)
	{
		final Date date = Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
		final Collection<StockLevelModel> stockLevelModel = getBlStockLevelDao().findSerialStockLevelForDate(serial.getCode(),
				date, date);
		if (CollectionUtils.isEmpty(stockLevelModel))
		{
			createStockLevelForSerial(serial, date);
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.WARN, "Stock already exist for serial product {} for the date {}",
					serial.getCode(),
					date);
		}

	}

	/**
	 * It first checks whether the stock already exists or not, if not, it creates the stock
	 *
	 * @param serial
	 */
	private void findAndCreateStockLevelForUsedGearSerial(final BlSerialProductModel serial)
	{
		final StockLevelModel stockLevelModel = getBlStockLevelDao().findStockLevelForUsedGearSerial(serial.getCode());
		if (Objects.isNull(stockLevelModel))
		{
			createStockLevelForSerial(serial, null);
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.WARN, "Stock already exist for serial product {}", serial.getCode());
		}

	}


	/**
	 * It creates the stock level for the sku and the serial
	 *
	 * @param serial
	 * @param date
	 */
	private void createStockLevelForSerial(final BlSerialProductModel serial, final Date date)
	{
		final StockLevelModel stockLevel = getModelService().create(StockLevelModel.class);
		stockLevel.setDate(date == null ? null : BlDateTimeUtils.getFormattedStartDay(date).getTime());
		stockLevel.setProductCode(serial.getBlProduct().getCode());
		stockLevel.setWarehouse(serial.getWarehouseLocation());
		stockLevel.setAvailable(0);
		stockLevel.setSerialProductCode(serial.getCode());
		stockLevel.setSerialStatus(serial.getSerialStatus());
		if(null != serial.getIsBufferedInventory()) {
			stockLevel.setBufferedInventory(serial.getIsBufferedInventory());
		}
		try
		{
			getModelService().save(stockLevel);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock created for serial product {} for the date {}", serial, date);
		}
		catch (final ModelSavingException ex)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Stock not created for the serial product {}", serial.getCode());
		}
	}

	/**
	 * This method created to get stock level for serial products
	 * @param serialCode the serial product code
	 * @param startDate the rental start date
	 * @param endDate the rental end date
	 */
	public void findStockLevelForExtendOrderSerialProducts(final String serialCode , final Date startDate , final Date endDate){
		getBlStockLevelDao().findSerialStockLevelForDateFromNonBufferInv(serialCode, startDate, endDate);
	}

	/**
	 * {@inheritDoc}
	 * @param blSerialProduct serial product
	 */
	public void findAndUpdateBufferInvInStockRecords(final BlSerialProductModel blSerialProduct) {
		final Collection<StockLevelModel> stockLevels = getStockLevelModelsBasedOnDates(blSerialProduct);
		stockLevels.forEach(stockLevel -> {
			stockLevel.setBufferedInventory(blSerialProduct.getIsBufferedInventory());
			getModelService().save(stockLevel);
		});
		updateSkuBufferInventoryPercentage(blSerialProduct);
	}

	/**
	 * It calculates buffer inventory percentage and updates it at SKU product level
	 * @param blSerialProduct
	 */
	private void updateSkuBufferInventoryPercentage(final BlSerialProductModel blSerialProduct) {
		final BlProductModel blProductModel = blSerialProduct.getBlProduct();
		final int totalActiveProducts = blProductModel.getSerialProducts().stream()
				.filter(blSerialProductModel -> null != blSerialProductModel.getSerialStatus() && blSerialProductModel
						.getSerialStatus().equals(SerialStatusEnum.ACTIVE)).collect(Collectors.toList()).size();
		final int totalBufferProducts = blProductModel.getSerialProducts().stream()
				.filter(blSerialProductModel -> isBufferInvProduct(blSerialProductModel, blSerialProduct)).collect(Collectors.toList()).size();
		if(totalBufferProducts > 0  && totalActiveProducts > 0) {
			final double bufferInventoryPercentage = ((double) totalBufferProducts / (double) totalActiveProducts) * BlCoreConstants
					.DIVIDE_BY_HUNDRED;
			blProductModel.setBufferedInventoryPercentage(bufferInventoryPercentage);
		} else {
			blProductModel.setBufferedInventoryPercentage(0.0);
		}
		getModelService().save(blProductModel);
	}

	/**
	 * It checks whether the product is buffer inventory or not
	 * @param serialProductModel
	 * @param blSerialProductModel
	 * @return true if the product is buffer inventory
	 */
	private boolean isBufferInvProduct(final BlSerialProductModel serialProductModel,
			final BlSerialProductModel blSerialProductModel) {
		return blSerialProductModel.getCode().equals(serialProductModel.getCode()) ? (null != blSerialProductModel
				.getIsBufferedInventory() && blSerialProductModel.getIsBufferedInventory()) : (null != serialProductModel
				.getIsBufferedInventory() && serialProductModel.getIsBufferedInventory());
	}


	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * @return the productDao
	 */
	public BlProductDao getProductDao()
	{
		return productDao;
	}

	/**
	 * @param productDao
	 *           the productDao to set
	 */
	public void setProductDao(final BlProductDao productDao)
	{
		this.productDao = productDao;
	}

	/**
	 * @return the blStockLevelDao
	 */
	public BlStockLevelDao getBlStockLevelDao()
	{
		return blStockLevelDao;
	}

	/**
	 * @param blStockLevelDao
	 *           the blStockLevelDao to set
	 */
	public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao)
	{
		this.blStockLevelDao = blStockLevelDao;
	}

}
