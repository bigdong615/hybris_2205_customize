package com.bl.core.stock.impl;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.text.ParseException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.stock.BlStockLevelDao;


/**
 * Junit test for BlCommerceStockServiceImpl
 * @author Moumita
 */
@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlCommerceStockServiceTest
{
	private static final String productCode = "Canon_24-105mm_f4_IS";
	@InjectMocks
	private final DefaultBlCommerceStockService blCommerceStockService = new DefaultBlCommerceStockService();

	@Mock
	private BlStockLevelDao blStockLevelDao;

	@Mock
	private WarehouseModel external_Matsudo_warehouse, warehouse_e, warehouse_s;

	private List<WarehouseModel> warehouseModels;
	private Collection<StockLevelModel> stocks;
	private Date startDate;
	private Date endDate;
	private LocalDateTime rentalStartDay;
	private StockLevelModel stock1;
	private StockLevelModel stock2;
	private StockLevelModel stock3;
	private StockLevelModel stock4;
	private StockLevelModel stock5;
	private StockLevelModel stock6;
	private StockLevelModel stock7;
	private StockLevelModel stock8;
	private StockLevelModel stock9;
	private StockLevelModel stock10;

	@Before
	public void setup()
	{
		//MockitoAnnotations.initMocks(this);
		warehouseModels = new ArrayList<>();
		stocks = new ArrayList<>();
		stock1 = new StockLevelModel();
		stock2 = new StockLevelModel();
		stock3 = new StockLevelModel();
		stock4 = new StockLevelModel();
		stock5 = new StockLevelModel();
		stock6 = new StockLevelModel();
		stock7 = new StockLevelModel();
		stock8 = new StockLevelModel();
		stock9 = new StockLevelModel();
		stock10 = new StockLevelModel();
		stubStockLevels();
	}

	private void stubStockLevels() {
		warehouseModels.add(external_Matsudo_warehouse);
		warehouseModels.add(warehouse_e);
		warehouseModels.add(warehouse_s);
		final Calendar calendar = Calendar.getInstance();
		calendar.set(2021, 03, 26);
		startDate = calendar.getTime();
		calendar.set(2021, 03, 27);
		endDate = calendar.getTime();
		stock1.setReservedStatus(false);
		stock1.setDate(startDate);
		stock2.setReservedStatus(false);
		stock2.setDate(startDate);
		stock3.setReservedStatus(true);
		stock3.setDate(startDate);
		stock4.setReservedStatus(true);
		stock4.setDate(startDate);
		stock5.setReservedStatus(true);
		stock5.setDate(startDate);
		stock6.setReservedStatus(false);
		stock6.setDate(endDate);
		stock7.setReservedStatus(false);
		stock7.setDate(endDate);
		stock8.setReservedStatus(true);
		stock8.setDate(endDate);
		stock9.setReservedStatus(true);
		stock9.setDate(endDate);
		stock10.setReservedStatus(true);
		stock10.setDate(endDate);
	}

	/**
	 * This is to check the condition for out of stock
	 */
	@Test
	public void testGetStockForDateForOOS()
	{
		final StockLevelStatus status = blCommerceStockService.getStockLevelStatus(warehouseModels, productCode, startDate, endDate);
		Assert.assertEquals(StockLevelStatus.OUTOFSTOCK, status);
	}

	/**
	 * This is to check the condition for out of stock when stock for any of the dates are not present
	 * in the inventory table
	 */
	@Test
	public void testWhenStockIsNotPresentForAllDays() throws ParseException {
		final Collection<StockLevelModel> stocks = new ArrayList<>();
		stocks.add(stock1);
		stocks.add(stock2);
		Mockito.when(blStockLevelDao.findStockLevelForDate(productCode, warehouseModels, startDate, endDate))
				.thenReturn(stocks);
		final StockLevelStatus status = blCommerceStockService.getStockLevelStatus(warehouseModels, productCode, startDate, endDate);
		Assert.assertEquals(StockLevelStatus.OUTOFSTOCK, status);
	}

	/**
	 * This is to check the condition for low stock
	 */
	@Test
	public void testGetStockForDateForLowStock() throws ParseException {
		stocks.add(stock1);
		stocks.add(stock2);
		stocks.add(stock3);
		stocks.add(stock4);
		stocks.add(stock5);
		stocks.add(stock6);
		stocks.add(stock7);
		stocks.add(stock8);
		stocks.add(stock9);
		stocks.add(stock10);
		Mockito.when(blStockLevelDao.findStockLevelForDate(productCode, warehouseModels, startDate, endDate))
				.thenReturn(stocks);
		final StockLevelStatus status = blCommerceStockService.getStockLevelStatus(warehouseModels, productCode, startDate, endDate);
		Assert.assertEquals(StockLevelStatus.LOWSTOCK, status);
	}

	/**
	 * This is to check the condition for in stock
	 */
	@Test
	public void testGetStockForDateForInStock() throws ParseException {
		stocks.add(stock1);
		stocks.add(stock2);
		stocks.add(stock5);
		stocks.add(stock6);
		Mockito.when(blStockLevelDao.findStockLevelForDate(productCode, warehouseModels, startDate, endDate))
				.thenReturn(stocks);
		final StockLevelStatus status = blCommerceStockService.getStockLevelStatus(warehouseModels, productCode, startDate, endDate);
		Assert.assertEquals(StockLevelStatus.INSTOCK, status);
	}

}
