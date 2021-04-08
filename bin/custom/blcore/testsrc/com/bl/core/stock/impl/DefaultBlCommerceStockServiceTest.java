package com.bl.core.stock.impl;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.bl.core.stock.BlStockLevelDao;


/**
 * @author Moumita
 *
 * Junit test for BlCommerceStockServiceImpl
 */
@UnitTest
public class DefaultBlCommerceStockServiceTest
{
	private static final String productCode = "Canon_24-105mm_f4_IS";
	@InjectMocks
	private final DefaultBlCommerceStockService blCommerceStockService = new DefaultBlCommerceStockService();

	@Mock
	private BlStockLevelDao blStockLevelDao;

	@Mock
	private WarehouseModel external_Matsudo_warehouse, warehouse_e, warehouse_s;

	@Before
	public void setup()
	{
		MockitoAnnotations.initMocks(this);
	}

	/**
	 * This is to check the condition for out of stock
	 */
	@Test
	public void testGetStockForDateForOOS()
	{
		final List<WarehouseModel> warehouseModels = new ArrayList<WarehouseModel>();
		warehouseModels.add(external_Matsudo_warehouse);
		warehouseModels.add(warehouse_e);
		warehouseModels.add(warehouse_s);
		final LocalDateTime startDate = LocalDateTime.of(2022, 3, 6, 0, 0);
		final LocalDateTime endDate = LocalDateTime.of(2022, 3, 8, 0, 0);
		//		StockLevelStatus status = blCommerceStockService.getStockLevelStatus(warehouseModels, productCode, startDate, endDate);
		//		Assert.assertEquals(StockLevelStatus.OUTOFSTOCK, status);
	}

	/**
	 * This is to check the condition for in stock
	 */
	@Test
	public void testGetStockForDateForInStock() throws ParseException {
		final List<WarehouseModel> warehouseModels = new ArrayList<WarehouseModel>();
		warehouseModels.add(external_Matsudo_warehouse);
		warehouseModels.add(warehouse_e);
		warehouseModels.add(warehouse_s);
		final LocalDateTime startDate = LocalDateTime.of(2022, 3, 3, 0, 0);
		final Collection<StockLevelModel> stocks = new ArrayList<>();
		final StockLevelModel stock1 = new StockLevelModel();
		stock1.setReservedStatus(false);
		final StockLevelModel stock2 = new StockLevelModel();
		stock2.setReservedStatus(true);
		stocks.add(stock1);
		stocks.add(stock2);
		final Date date = (new SimpleDateFormat("yyyy-MM-dd")).parse("2022-03-03");
		final LocalDateTime endDate = LocalDateTime.of(2022, 3, 5, 0, 0);
		//		Mockito.when(blStockLevelDao.findStockLevelForDate(productCode, warehouseModels, date))
		//				.thenReturn(stocks);
		//		StockLevelStatus status = blCommerceStockService.getStockLevelStatus(warehouseModels, productCode, startDate, endDate);
		//		Assert.assertEquals(StockLevelStatus.INSTOCK, status);
	}

	/**
	 * This is to check the condition for low stock
	 */
	@Test
	public void testGetStockForDateForLowStock() throws ParseException {
		final List<WarehouseModel> warehouseModels = new ArrayList<WarehouseModel>();
		warehouseModels.add(external_Matsudo_warehouse);
		warehouseModels.add(warehouse_e);
		warehouseModels.add(warehouse_s);
		final LocalDateTime startDate = LocalDateTime.of(2022, 3, 3, 0, 0);
		final Collection<StockLevelModel> stocks = new ArrayList<>();
		final StockLevelModel stock1 = new StockLevelModel();
		stock1.setReservedStatus(false);
		final StockLevelModel stock2 = new StockLevelModel();
		stock2.setReservedStatus(true);
		final StockLevelModel stock3 = new StockLevelModel();
		stock3.setReservedStatus(false);
		final StockLevelModel stock4 = new StockLevelModel();
		stock4.setReservedStatus(true);
		final StockLevelModel stock5 = new StockLevelModel();
		stock5.setReservedStatus(true);
		stocks.add(stock1);
		stocks.add(stock2);
		stocks.add(stock3);
		stocks.add(stock4);
		stocks.add(stock5);
		final Date date = (new SimpleDateFormat("yyyy-MM-dd")).parse("2022-03-03");
		final LocalDateTime endDate = LocalDateTime.of(2022, 3, 5, 0, 0);
		//		Mockito.when(blStockLevelDao.findStockLevelForDate(productCode, warehouseModels, date))
		//				.thenReturn(stocks);
		//		StockLevelStatus status = blCommerceStockService.getStockLevelStatus(warehouseModels, productCode, startDate, endDate);
		//		Assert.assertEquals(StockLevelStatus.LOWSTOCK, status);
	}

}
