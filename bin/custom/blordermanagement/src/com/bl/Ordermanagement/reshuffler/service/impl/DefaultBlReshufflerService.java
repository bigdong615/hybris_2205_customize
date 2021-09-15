package com.bl.Ordermanagement.reshuffler.service.impl;

import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.reshuffler.service.BlReshufflerService;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.stock.BlCommerceStockService;
import com.google.common.collect.Sets;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.log4j.Logger;

/**
 * It is to allocate the unallocated products
 * @author Moumita
 */
public class DefaultBlReshufflerService implements BlReshufflerService
{
	  private static final Logger LOG = Logger.getLogger(DefaultBlReshufflerService.class);
	  private BlOrderDao orderDao;
	  private BlCommerceStockService blCommerceStockService;
	  private BaseStoreService baseStoreService;
		private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;
		private ModelService modelService;
		private BlAssignSerialService blAssignSerialService;
		private BlAllocationService blAllocationService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	  public void processIncompleteOrders() {
	    final Date currentDate = Date
	        .from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
	    final List<AbstractOrderModel> todayOrdersToBeProcessed = getOrderDao().getIncompleteOrdersToBeProcessed(currentDate, currentDate);
	    //Sort the orders by order total price
	    final List<AbstractOrderModel> ordersSortedByTotalPrice = todayOrdersToBeProcessed.stream()
	        .sorted(Comparator.comparing(AbstractOrderModel::getTotalPrice).reversed())
	        .collect(Collectors.toList());
	    final BaseStoreModel baseStoreModel = getBaseStoreService().getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
	    //Get all warehouses
	    final List<WarehouseModel> warehouses = baseStoreModel.getWarehouses();
	    //It filters the orders which needs to be processed and ignore the orders which contains the SKU (when total number
	    // of sku needed for orders, will ship on same day, is not sufficient to fulfill from main and buffer inventory
	    final Map<AbstractOrderModel, Set<String>> filteredOrders = filterOrdersForProcessing(ordersSortedByTotalPrice,
	        warehouses, currentDate);
	    processOrders(filteredOrders, warehouses);
	  }

	  private void processOrders(final Map<AbstractOrderModel, Set<String>> filteredOrders,
	      final List<WarehouseModel> warehouses) {
	    filteredOrders.entrySet().forEach(entry -> {
	      //It gets the preferred warehouse
	      final WarehouseModel location = getBlDeliveryStateSourcingLocationFilter().applyFilter(entry.getKey());
	      fulfillFromWH(location, entry, warehouses);
	    });
	  }


	  private WarehouseModel getAnotherWarehouse(final Collection<WarehouseModel> warehouses, final WarehouseModel preferredWH) {
	    final Optional<WarehouseModel> warehouse = warehouses.stream().filter(warehouseModel -> !(warehouseModel.getCode()
	        .equals(preferredWH.getCode()))).findFirst();
	    return warehouse.isPresent() ? warehouse.get() : null;
	  }

	  private void fulfillFromWH(final WarehouseModel location, final Entry<AbstractOrderModel, Set<String>> entry,
	      final List<WarehouseModel> warehouses) {
	    final AbstractOrderModel order = entry.getKey();
	    final Set<String> productCodes = entry.getValue();
	    final WarehouseModel anotherWH = getAnotherWarehouse(warehouses, location);
	    final Set<String> modifiedProductCodes = modifyProductCodes(order, productCodes, location, anotherWH);
	    boolean noSplitting = false;
				modifiedProductCodes.addAll(productCodes);
				final Collection<StockLevelModel> stockLevels = getStocks(modifiedProductCodes, location,
						entry.getKey());
				final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
						.groupBySkuProductWithAvailability(stockLevels);
				noSplitting = isSourcingNoSplittingPossible(modifiedProductCodes, availabilityMap, order);
				if (noSplitting) {
					final Collection<AbstractOrderEntryModel> orderEntries = getOrderEntries(order, modifiedProductCodes);
					final SourcingContext context = createSourcingContext(orderEntries);
					final SourcingLocation sourcingLocation = createSourcingLocation(availabilityMap, location,context);
					assignSerialFromLocation(context, sourcingLocation);
					//if order has consignment from another WH, delete that and release the stock
					//createConsignment
					createConsignment(order, context, location);
				}
			 else {
				final Set<String> modifiedProductForOtherWH = modifyProductCodes(order, productCodes,
						anotherWH, location);
					modifiedProductForOtherWH.addAll(productCodes);
					final Collection<StockLevelModel> stockLevelsFromOtherWh = getStocks(modifiedProductForOtherWH,
							anotherWH, entry.getKey());
					final Map<String, List<StockLevelModel>> availabilityMapForOtherWH = getBlCommerceStockService()
							.groupBySkuProductWithAvailability(stockLevelsFromOtherWh);
					noSplitting = isSourcingNoSplittingPossible(modifiedProductForOtherWH, availabilityMapForOtherWH, order);
					if (noSplitting) {
						//if order has consignment from another WH, delete that and release the stock
						//createConsignment
						final Collection<AbstractOrderEntryModel> orderEntries = getOrderEntries(order, modifiedProductForOtherWH);
						final SourcingContext context = createSourcingContext(orderEntries);
						final SourcingLocation sourcingLocation = createSourcingLocation(availabilityMapForOtherWH, anotherWH, context);
						assignSerialFromLocation(context, sourcingLocation);
						createConsignment(order, context, anotherWH);
					}
			}
			if(!noSplitting) {
				fulfillFromMultipleWarehouses(entry, location, anotherWH, productCodes);
			}
	  }

	  private SourcingLocation createSourcingLocation(final Map<String, List<StockLevelModel>> availabilityMap,
				final WarehouseModel warehouseModel, final SourcingContext context) {
			final SourcingLocation sourcingLocation = new SourcingLocation();
			sourcingLocation.setWarehouse(warehouseModel);
			sourcingLocation.setContext(context);
			final Set<SourcingLocation> sourcingLocations = new HashSet<>();
			sourcingLocations.add(sourcingLocation);
			context.setSourcingLocations(sourcingLocations);
			sourcingLocation.setAvailabilityMap(availabilityMap);
			return sourcingLocation;
		}

	private void assignSerialFromLocation(final SourcingContext context, final SourcingLocation sourcingLocation) {
		final List<AtomicBoolean> allEntrySourceComplete = new ArrayList<>();
		SourcingResult result = new SourcingResult();
		final SourcingLocation finalSourcingLocation = context.getSourcingLocations().iterator().next();
		context.getOrderEntries().forEach(entry -> {
			Long quantity = 0L;
			final Long splitConsignmentQuantity = entry.getSplitConsignmentQuantity();
			if(Objects.nonNull(entry.getSplitConsignmentQuantity()) && entry.getSplitConsignmentQuantity() > 0) {
				quantity = entry.getSplitConsignmentQuantity();
				getModelService().save(entry);
			} else {
				quantity = entry.getUnAllocatedQuantity();
			}
			getBlAssignSerialService().fulfillEachEntry(context, result, finalSourcingLocation, entry, allEntrySourceComplete, quantity);
		});
	}

	private Collection<AbstractOrderEntryModel> getOrderEntries(final AbstractOrderModel order,
			final Set<String> modifiedProductCodes) {
	  	return order.getEntries().stream().filter(entry -> modifiedProductCodes.contains(entry.getProduct()
					.getCode())).collect(Collectors.toList());
	}

	private SourcingContext createSourcingContext(final Collection<AbstractOrderEntryModel> orderEntries) {
		final SourcingContext context = new SourcingContext();
		context.setOrderEntries(orderEntries);
		final SourcingResults result = new SourcingResults();
		result.setResults(Sets.newHashSet());
		result.setComplete(Boolean.FALSE);
		context.setResult(result);
		return context;
	}

	private void fulfillFromMultipleWarehouses(final Entry<AbstractOrderModel,
			Set<String>> entry, final WarehouseModel location, final WarehouseModel anotherWH, final Set<String> productCodes) {
	    final Map<WarehouseModel, List<String>> warehouseWithProducts = new HashMap<>();
	    final AbstractOrderModel order = entry.getKey();
	    final Collection<StockLevelModel> stockLevels = getStocks(productCodes, location, order);
			final Collection<StockLevelModel> stockLevelsFromOtherWH = getStocks(productCodes, anotherWH, order);
		final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
				.groupBySkuProductWithAvailability(stockLevels);
		final Map<String, List<StockLevelModel>> availabilityMapForOtherWH = getBlCommerceStockService()
				.groupBySkuProductWithAvailability(stockLevelsFromOtherWH);
		final AtomicBoolean allEntriesCanBeFulfilled = new AtomicBoolean(Boolean.TRUE);
			if(CollectionUtils.isNotEmpty(stockLevels) && CollectionUtils.isNotEmpty(stockLevelsFromOtherWH)) {
				final List<String> products = new ArrayList<>();
				final List<String> productsFromOtherWH = new ArrayList<>();
				entry.getValue().forEach(skuProduct -> {
					populateProductFulfillDetailsForBothWH(availabilityMap, skuProduct, order, products,
							availabilityMapForOtherWH, productsFromOtherWH, allEntriesCanBeFulfilled);
				});
				warehouseWithProducts.put(location, products);
				warehouseWithProducts.put(anotherWH, productsFromOtherWH);
			} else if(CollectionUtils.isNotEmpty(stockLevels)) {
				final List<String> products = new ArrayList<>();
				entry.getValue().forEach(skuProduct -> {
					populateProductFulfillDetails(availabilityMap, skuProduct, order, products, allEntriesCanBeFulfilled);
				});
				warehouseWithProducts.put(location, products);
			} else if(CollectionUtils.isNotEmpty(stockLevelsFromOtherWH)) {
				final List<String> productsFromOtherWH = new ArrayList<>();
				entry.getValue().forEach(skuProduct -> {
					populateProductFulfillDetails(availabilityMapForOtherWH, skuProduct, order, productsFromOtherWH, allEntriesCanBeFulfilled);
				});
				warehouseWithProducts.put(anotherWH, productsFromOtherWH);
			}
			if(allEntriesCanBeFulfilled.get()) {
				warehouseWithProducts.entrySet().forEach(entryPerWH -> {
					final Set<String> products = new HashSet<>();
					entryPerWH.getValue().forEach(product -> {
						products.add(product);
					});
					if (entryPerWH.getKey().equals(location)) {
						final Collection<AbstractOrderEntryModel> entries = getOrderEntries(order, products);
						final SourcingContext context = createSourcingContext(entries);
						final SourcingLocation sourcingLocation = createSourcingLocation(availabilityMap,
								location, context);
						assignSerialFromLocation(context, sourcingLocation);
						createConsignment(order, context, location);
					} else {
						if (entryPerWH.getKey().equals(anotherWH)) {
							final Collection<AbstractOrderEntryModel> orderEntries = getOrderEntries(order,
									products);
							final SourcingContext context = createSourcingContext(orderEntries);
							final SourcingLocation sourcingLocation = createSourcingLocation(
									availabilityMapForOtherWH, anotherWH, context);
							assignSerialFromLocation(context, sourcingLocation);
							//create consignment
							createConsignment(order, context, anotherWH);
						}
					}
				});
			}
	  }

	private void populateProductFulfillDetailsForBothWH(final Map<String, List<StockLevelModel>> availabilityMap, final String skuProduct,
			final AbstractOrderModel order, final List<String> products, final Map<String, List<StockLevelModel>> availabilityMapForOtherWH,
			final List<String> productsFromOtherWH, AtomicBoolean allEntriesCanBeFulfilled) {
		final Long availableQty = getAvailabilityForProduct(skuProduct, availabilityMap);
		final Optional<AbstractOrderEntryModel> orderEntryModel = order.getEntries().stream()
				.filter(orderEntry ->
						orderEntry.getProduct().getCode()
								.equals(skuProduct)).findFirst();
		final Long availableQtyFromOtherWH = getAvailabilityForProduct(skuProduct, availabilityMapForOtherWH);
		if(orderEntryModel.isPresent()) {
			final AbstractOrderEntryModel orderEntry = orderEntryModel.get();
			final Long unallocatedQty = orderEntry.getUnAllocatedQuantity();
			if(orderEntry.getUnAllocatedQuantity() <= availableQty) {
				products.add(orderEntry.getProduct().getCode());
			} else if(orderEntry.getUnAllocatedQuantity() <= availableQtyFromOtherWH) {
				productsFromOtherWH.add(orderEntry.getProduct().getCode());
			} else if(orderEntry.getUnAllocatedQuantity() <= availableQty + availableQtyFromOtherWH) {
				products.add(orderEntry.getProduct().getCode());
				orderEntry.setSplitConsignmentQuantity(availableQty);
				orderEntry.setUnAllocatedQuantity(orderEntry.getUnAllocatedQuantity() - availableQty);
				productsFromOtherWH.add(orderEntry.getProduct().getCode());
				getModelService().save(orderEntry);
			} else {
				allEntriesCanBeFulfilled.set(Boolean.FALSE);
			}
		}
	}

	  private void populateProductFulfillDetails(final Map<String, List<StockLevelModel>> availabilityMap, String skuProduct,
				final AbstractOrderModel order, final List<String> productWithQty, AtomicBoolean allEntriesCanBeFulfilled) {
			final Long availableQty = getAvailabilityForProduct(skuProduct, availabilityMap);
			final Optional<AbstractOrderEntryModel> orderEntryModel = order.getEntries().stream()
					.filter(orderEntry ->
							orderEntry.getProduct().getCode()
									.equals(skuProduct)).findFirst();
			if(orderEntryModel.isPresent()) {
				final Long unallocatedQty = orderEntryModel.get().getUnAllocatedQuantity();
				if(orderEntryModel.get().getUnAllocatedQuantity() <= availableQty) {
					productWithQty.add(orderEntryModel.get().getProduct().getCode());
				} else {
					allEntriesCanBeFulfilled.set(Boolean.FALSE);
				}
			}
		}


	private Set<String> modifyProductCodes(final AbstractOrderModel order, final Set<String> productCodes,
	      final WarehouseModel preferredWH, final WarehouseModel otherWH) {
	    final Set<String> products = new HashSet<>();
	    final Optional<ConsignmentModel> consignment = order.getConsignments().stream().filter(consignmentModel ->
	        consignmentModel.getWarehouse().getCode().equals(otherWH.getCode())).findAny();
	    if(consignment.isPresent()) {
	      consignment.get().getConsignmentEntries().forEach(consignmentEntryModel -> {
	        consignmentEntryModel.getSerialProducts().forEach(serialProduct -> {
	        	if(serialProduct instanceof BlSerialProductModel) {
							products.add(((BlSerialProductModel) serialProduct).getBlProduct().getCode());
						}
	        });
	      });
	    }
	    return products;
	  }

	  private void createConsignment(final AbstractOrderModel order, final SourcingContext context,
				final WarehouseModel warehouseModel) {
			final Set<SourcingResult> sourcingResults = context.getResult().getResults();
			final SourcingResult result = CollectionUtils.isNotEmpty(sourcingResults) ? sourcingResults.iterator().next() :
					new SourcingResult();
			final ConsignmentModel consignment = getConsignment(warehouseModel, order);
			if(Objects.isNull(consignment)) {
				getBlAllocationService().createConsignment(order,
						BlCoreConstants.CONSIGNMENT_PROCESS_PREFIX + order.getCode(),
						result);
			} else {
				final Set<ConsignmentEntryModel> entries = context.getOrderEntries().stream().map(entryModel ->
						getBlAllocationService().createConsignmentEntry(entryModel, entryModel.getQuantity(), consignment,
							context.getResult().getResults().iterator().next())
				).collect(Collectors.toSet());
				entries.addAll(consignment.getConsignmentEntries());
				consignment.setConsignmentEntries(entries);
				getModelService().save(consignment);
			}
	  }

	  private boolean isSourcingNoSplittingPossible(final Set<String> modifiedProductCodes, final
	      Map<String, List<StockLevelModel>> availabilityMap, final AbstractOrderModel order) {
	    return modifiedProductCodes.stream().allMatch(skuProduct -> {
	      final Long availableQty = getAvailabilityForProduct(skuProduct, availabilityMap);
	      final Optional<AbstractOrderEntryModel> orderEntryModel = order.getEntries().stream().filter(orderEntry ->
	            orderEntry.getProduct().getCode()
	                .equals(skuProduct)).findFirst();
	      return orderEntryModel.get().getUnAllocatedQuantity() <= availableQty;
	    });
	  }

	  private Long getAvailabilityForProduct(final String skuProduct, final
	      Map<String, List<StockLevelModel>> availabilityMap) {
	    Long stockLevel = 0L;
	    if (MapUtils.isNotEmpty(availabilityMap)) {
	      final List<StockLevelModel> stockLevelList = Objects.nonNull(availabilityMap.get(skuProduct)) ?
						availabilityMap.get(skuProduct) : Collections.emptyList();
	      final Set<String> serialProductCodes = stockLevelList.stream()
	          .map(StockLevelModel::getSerialProductCode).collect(Collectors.toSet());

	      stockLevel = Long.valueOf(serialProductCodes.size());

	    }
	    return stockLevel;
	  }

	  private Collection<StockLevelModel> getStocks(final Set<String> productCodes, final WarehouseModel location,
	      final AbstractOrderModel order) {
	  	final ConsignmentModel consignment = getConsignment(location, order);
	  	final Date shippingStartDate = Objects.nonNull(consignment) ? consignment.getOptimizedShippingStartDate() :
					 order.getActualRentalStartDate();
			final Date shippingEndDate = Objects.nonNull(consignment) ? consignment.getOptimizedShippingEndDate() :
					order.getActualRentalEndDate();
	    final Set<String> productsToFulfillFromBuffer = new HashSet<>(productCodes);
	    final Collection<StockLevelModel> stockLevels = getBlCommerceStockService().getStockForProductCodesAndDate(productCodes,
	        location, shippingStartDate, shippingEndDate, Boolean.FALSE);
	    final Set<String> productCodesWithStock = new HashSet<>();
	    stockLevels.stream().forEach(stockLevelModel ->
	        productCodesWithStock.add(stockLevelModel.getProductCode()));
	    productsToFulfillFromBuffer.removeAll(productCodesWithStock);
	    if(CollectionUtils.isNotEmpty(productsToFulfillFromBuffer)) {
	      final Collection<StockLevelModel> stockLevelsFromBuffer = getBlCommerceStockService()
	          .getStockForProductCodesAndDate(productsToFulfillFromBuffer,
	              location, order.getActualRentalStartDate(),
	              order.getActualRentalEndDate(), Boolean.TRUE);
	      stockLevels.addAll(stockLevelsFromBuffer);
	    }
	    return stockLevels;
	  }

	private ConsignmentModel getConsignment(final WarehouseModel location, final AbstractOrderModel order) {
	  	final Optional<ConsignmentModel> consignment = order.getConsignments().stream().filter(consignmentModel ->
					consignmentModel.getWarehouse().getCode().equals(location.getCode())).findFirst();
	  	if(consignment.isPresent()) {
	  		return consignment.get();
			}
	  	return null;
	}

	private Map<AbstractOrderModel, Set<String>> filterOrdersForProcessing(final List<AbstractOrderModel> todayOrdersToBeProcessed,
	      final List<WarehouseModel> warehouses, final Date currentDate) {
	    //This map will contain order id with unallocated products
	    final Map<AbstractOrderModel, Set<String>> ordersWithUnallocatedProducts = new LinkedHashMap<>();
	    //This map will contain unallocated products with quantity
	    final Map<String, Long> unallocatedProductWithQty = new HashMap<>();
	    todayOrdersToBeProcessed.stream().forEach(order -> {
	      //List of unallocated products
	      final Set<String> unallocatedProducts = new HashSet<>();
	      order.getEntries().forEach(entry -> {
	        final Long unallocatedQty = entry.getUnAllocatedQuantity();
	        if(Objects.nonNull(unallocatedQty) && unallocatedQty > 0) {
	          final String productCode = entry.getProduct().getCode();
	          unallocatedProductWithQty.put(productCode, (Objects.nonNull(unallocatedProductWithQty.get(productCode)) ?
	              unallocatedProductWithQty.get(productCode) : 0) + entry.getUnAllocatedQuantity());
	          unallocatedProducts.add(productCode);
	        }
	      });
	      ordersWithUnallocatedProducts.put(order, unallocatedProducts);
	    });
	    // List of unallocated products code
	    final List<String> unallocatedProductList = unallocatedProductWithQty.keySet()
	        .stream().collect(Collectors.toList());
	    //It finds stock for unallocated products for the day when the products will ship out
	    final Map<String, Long> getStockForUnallocatedProducts = getBlCommerceStockService().
	        getStockForUnallocatedProduct(unallocatedProductList, warehouses, currentDate, currentDate);
	    //The products which are available to fulfill
	    final List<String> availableProducts = new ArrayList<>();
	    unallocatedProductWithQty.entrySet().forEach(entry -> {
	      final Optional<Entry<String, Long>> matchedProducts = getStockForUnallocatedProducts.entrySet().stream()
	          .filter(entryForUnallocatedProducts ->
	              (entryForUnallocatedProducts.getKey().equals(entry.getKey()) &&
	                  entryForUnallocatedProducts.getValue() >= (entry.getValue()))).findFirst();
	      if(matchedProducts.isPresent()) {
	        availableProducts.add(matchedProducts.get().getKey());
	      }
	    });
	    if(CollectionUtils.isNotEmpty(availableProducts)) {
				//The orders which will not be considered in reshuffler job, will be in manual review only
				ordersWithUnallocatedProducts.entrySet().removeIf(entry -> Collections
						.disjoint(entry.getValue(), availableProducts));
			}
	    return ordersWithUnallocatedProducts;
	  }

	  public BlOrderDao getOrderDao() {
	    return orderDao;
	  }

	  public void setOrderDao(final BlOrderDao orderDao) {
	    this.orderDao = orderDao;
	  }

	public BlCommerceStockService getBlCommerceStockService() {
		return blCommerceStockService;
	}

	public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
		this.blCommerceStockService = blCommerceStockService;
	}

	public BaseStoreService getBaseStoreService() {
		return baseStoreService;
	}

	public void setBaseStoreService(BaseStoreService baseStoreService) {
		this.baseStoreService = baseStoreService;
	}

	public BlDeliveryStateSourcingLocationFilter getBlDeliveryStateSourcingLocationFilter() {
		return blDeliveryStateSourcingLocationFilter;
	}

	public void setBlDeliveryStateSourcingLocationFilter(
			BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter) {
		this.blDeliveryStateSourcingLocationFilter = blDeliveryStateSourcingLocationFilter;
	}

	public ModelService getModelService() {
		return modelService;
	}

	public void setModelService(ModelService modelService) {
		this.modelService = modelService;
	}

	public BlAssignSerialService getBlAssignSerialService() {
		return blAssignSerialService;
	}

	public void setBlAssignSerialService(
			BlAssignSerialService blAssignSerialService) {
		this.blAssignSerialService = blAssignSerialService;
	}

	public BlAllocationService getBlAllocationService() {
		return blAllocationService;
	}

	public void setBlAllocationService(
			BlAllocationService blAllocationService) {
		this.blAllocationService = blAllocationService;
	}

}
