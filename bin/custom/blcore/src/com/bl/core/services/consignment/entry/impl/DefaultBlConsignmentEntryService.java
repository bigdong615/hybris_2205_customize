package com.bl.core.services.consignment.entry.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlOptionsModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.session.SessionService;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * This service class is use to perform custom bussiness logic on consignment entry or consignment
 *
 * @author Ravikumar
 *
 */
public class DefaultBlConsignmentEntryService implements BlConsignmentEntryService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlConsignmentEntryService.class);
	private BlConsignmentDao blConsignmentDao;
	private ModelService modelService;
	private SessionService sessionService;
	private SearchRestrictionService searchRestrictionService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void removeSerialFromFutureConsignmentEntry(final BlSerialProductModel blSerialProductModel)
	{
		final List<ConsignmentEntryModel> consignmentEntriesForSerialCodeAndDate = getBlConsignmentDao()
				.getConsignmentEntriesForSerialCodeAndDate(blSerialProductModel,
						BlDateTimeUtils.getFormattedStartDay(new Date()).getTime());
		if (CollectionUtils.isNotEmpty(consignmentEntriesForSerialCodeAndDate))
		{
			consignmentEntriesForSerialCodeAndDate.forEach(consignmentEntry -> {
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
						"Performing removal of serial : {} from consignment entry with PK : {}", blSerialProductModel.getCode(),
						consignmentEntry.getPk().toString());
				final Set<BlSerialProductModel> updatedSerialList = getUpdatedSerialList(consignmentEntry, blSerialProductModel);
				consignmentEntry.setSerialProducts(Lists.newArrayList(updatedSerialList));
				consignmentEntry.setItems(Maps.newHashMap());
				setItemsMap(consignmentEntry, updatedSerialList);
				getModelService().save(consignmentEntry);
				getModelService().refresh(consignmentEntry);
				final AbstractOrderEntryModel orderEntry = consignmentEntry.getOrderEntry();
				updateUnallotedQuantityOnOrderEntry(orderEntry, updatedSerialList);
				final ConsignmentModel consignment = consignmentEntry.getConsignment();
				changeStatusOnConsignment(consignment);
				final AbstractOrderModel order = consignment.getOrder();
				changeStatusOnOrder(order);
				if(CollectionUtils.isEmpty(updatedSerialList)) 
				{
					getModelService().remove(consignmentEntry);
					getModelService().refresh(consignment);
				}
			});
		}
	}

	/**
	 * Update unalloted quantity on order entry.
	 *
	 * @param orderEntry
	 *           the order entry
	 */
	private void updateUnallotedQuantityOnOrderEntry(final AbstractOrderEntryModel orderEntry, final Set<BlSerialProductModel> updatedSerialList)
	{
		if (Objects.nonNull(orderEntry))
		{
			Long unAllocatedQuantity = ObjectUtils.defaultIfNull(orderEntry.getUnAllocatedQuantity(), Long.valueOf(0));
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Before increasing unAllotedQuantity : {}", unAllocatedQuantity);
			unAllocatedQuantity = unAllocatedQuantity + 1;
			orderEntry.setUnAllocatedQuantity(unAllocatedQuantity);
			orderEntry.setSerialProducts(Lists.newArrayList(updatedSerialList));
			getModelService().save(orderEntry);
			getModelService().refresh(orderEntry);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "After increasing unAllotedQuantity : {}", unAllocatedQuantity);
		}
	}

	/**
	 * Gets the updated serial list.
	 *
	 * @param consignmentEntry
	 *           the consignment entry
	 * @param serial
	 *           the serial
	 * @return the updated serial list
	 */
	private Set<BlSerialProductModel> getUpdatedSerialList(final ConsignmentEntryModel consignmentEntry,
			final BlSerialProductModel serial)
	{
		final Set<BlSerialProductModel> updatedList = Sets.newHashSet();
		consignmentEntry.getSerialProducts().forEach(serialProduct -> {
			if (serialProduct instanceof BlSerialProductModel && !serialProduct.getPk().toString().equals(serial.getPk().toString())
					&& !serialProduct.getProductType().equals(ProductTypeEnum.SUBPARTS))
			{
				updatedList.add(((BlSerialProductModel) serialProduct));
			}
		});
		return updatedList;
	}

	/**
	 * Change status on consignment.
	 *
	 * @param consignment
	 *           the consignment
	 */
	private void changeStatusOnConsignment(final ConsignmentModel consignment)
	{
		if (Objects.nonNull(consignment))
		{
			consignment.setStatus(ConsignmentStatus.MANUAL_REVIEW);
			getModelService().save(consignment);
			getModelService().refresh(consignment);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing Consignment : {} status to MANUAL_REVIEW",
					consignment.getCode());
		}

	}

	/**
	 * Change status on order.
	 *
	 * @param order
	 *           the order
	 */
	private void changeStatusOnOrder(final AbstractOrderModel order)
	{
		if (Objects.nonNull(order))
		{
			order.setStatus(OrderStatus.RECEIVED_MANUAL_REVIEW);
			getModelService().save(order);
			getModelService().refresh(order);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing Order : {} status to MANUAL_REVIEW", order.getCode());
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setItemsMap(final ConsignmentEntryModel entry, final Set<BlSerialProductModel> serialProductModels)
	{
		final Map<String, ItemStatusEnum> itemsMap = new HashMap<>();
		final Map<BlProductModel, Integer> allSerialSubPartProducts = new HashMap<>();
		serialProductModels.forEach(serial -> {
			itemsMap.put(serial.getCode(), ItemStatusEnum.NOT_INCLUDED);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Serial product with code {} added to the products list on consignment entry with consignment code {}",
					serial.getCode(), entry.getConsignment().getCode());

			final Map<BlProductModel, Integer> subPartsForGivenSerial =	getSessionService().executeInLocalView(new SessionExecutionBody()
			{
				@Override
				public Map<BlProductModel, Integer> execute()
				{
					getSearchRestrictionService().disableSearchRestrictions();
					if (null != serial.getBlProduct() && MapUtils.isNotEmpty(serial.getBlProduct().getSubpartsQty()))
					{
						return serial.getBlProduct().getSubpartsQty();
					}
					return Maps.newHashMap();
				}
			});
			addingSubpartToMap(allSerialSubPartProducts,subPartsForGivenSerial);
		});

		putSubPartProductsInToItemsMap(entry, itemsMap, allSerialSubPartProducts);
		putProductOptionsInToItemsMap(entry, itemsMap);
		entry.setItems(itemsMap);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setItemsMapForInternalTransferOrders(final ConsignmentEntryModel entry,
			final AbstractOrderEntryModel orderEntry) {

		final Map<String, ItemStatusEnum> itemsMap =
				null == entry.getItems() ? new HashMap<>() : entry.getItems();

		itemsMap.put(orderEntry.getProduct().getCode(), ItemStatusEnum.NOT_INCLUDED);

		final List<BlProductModel> products = new ArrayList<>();

		for (int i = 0; i < entry.getQuantity(); i++) {
			products.add((BlProductModel) orderEntry.getProduct());
		}

		entry.setSerialProducts(products);
		entry.setItems(itemsMap);

		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
				"Product with code {} added to the products list on consignment entry with consignment code {}",
				orderEntry.getProduct().getCode(), entry.getConsignment().getCode());
	}

	/**
	 * Add subpart product to items map of consignment entry.
	 *
	 * @param consignmentEntry
	 * @param itemsMap
	 * @param allSerialSubPartProducts
	 */
	private void putSubPartProductsInToItemsMap(final ConsignmentEntryModel consignmentEntry,
			final Map<String, ItemStatusEnum> itemsMap, final Map<BlProductModel, Integer> allSerialSubPartProducts)
	{
		final Map<BlProductModel, Integer> allSerialSubPartProductMap = new HashMap<>();
		final Map<BlProductModel, Integer> subPartsMap = new HashMap<>(allSerialSubPartProducts);
		for (final Map.Entry<BlProductModel, Integer> subPartEntry : subPartsMap.entrySet())
		{
			if (null != allSerialSubPartProductMap.get(subPartEntry.getKey()))
			{
				allSerialSubPartProductMap.put(subPartEntry.getKey(),
						allSerialSubPartProductMap.get(subPartEntry.getValue()));
			}
			else
			{
				allSerialSubPartProductMap.put(subPartEntry.getKey(), subPartEntry.getValue());
			}
		}
		allSerialSubPartProductMap.entrySet().forEach(mapEntry -> {
			final BlProductModel subPartProduct = mapEntry.getKey();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Sub part with code {} and quantity {} added to the products list on consignment entry.", subPartProduct.getCode(),
					mapEntry.getValue());
			if (mapEntry.getValue() == 1)
			{
				itemsMap.put(subPartProduct.getName(), ItemStatusEnum.NOT_INCLUDED);
				addSubPartToConsignmentEntry(consignmentEntry, subPartProduct);
			}
			else
			{
				for (int i = 1; i <= mapEntry.getValue(); i++)
				{
					itemsMap.put(subPartProduct.getName() + BlCoreConstants.DOUBLE_HYPHEN + i, ItemStatusEnum.NOT_INCLUDED);
					addSubPartToConsignmentEntry(consignmentEntry, subPartProduct);
				}
			}
		});
	}

	/**
	 * This method used to map subpart and its total count for particular serial.
	 * @param allSerialSubPartProducts
	 * @param subPartsForGivenSerial
	 */
	private void addingSubpartToMap(final Map<BlProductModel, Integer> allSerialSubPartProducts,final Map<BlProductModel, Integer> subPartsForGivenSerial){
		subPartsForGivenSerial.forEach( (productKey,quantity) ->{
		if(allSerialSubPartProducts.containsKey(productKey)){
			Integer existingQuantity =allSerialSubPartProducts.get(productKey);
			allSerialSubPartProducts.put(productKey,existingQuantity+quantity);
		}else{
			allSerialSubPartProducts.put(productKey,quantity);
		}
	} );
	}

	/**
	 * Update consignment entry with product options.
	 *
	 * @param consignmentEntry
	 * @param itemsMap
	 */
	private void putProductOptionsInToItemsMap(final ConsignmentEntryModel consignmentEntry,
			final Map<String, ItemStatusEnum> itemsMap)
	{
		final AbstractOrderEntryModel orderEntry = consignmentEntry.getOrderEntry();
		if (Objects.nonNull(orderEntry) && CollectionUtils.isNotEmpty(orderEntry.getOptions())
				&& Objects.nonNull(orderEntry.getOptions().get(0)))
		{
			final BlOptionsModel optionsModel = orderEntry.getOptions().get(0);
			if (consignmentEntry.getQuantity() == 1)
			{
				itemsMap.put(optionsModel.getName(), ItemStatusEnum.NOT_INCLUDED);
				addProductOptionsToConsignmentEntry(consignmentEntry, optionsModel);
			}
			else
			{
				for (int i = 1; i <= consignmentEntry.getQuantity(); i++)
				{
					itemsMap.put(optionsModel.getName() + BlCoreConstants.DOUBLE_HYPHEN + i, ItemStatusEnum.NOT_INCLUDED);
					addProductOptionsToConsignmentEntry(consignmentEntry, optionsModel);
				}
			}
		}
	}

	/**
	 * Add subpart product to consignment entry.
	 *
	 * @param consignmentEntry
	 * @param subPartProduct
	 */
	private void addSubPartToConsignmentEntry(final ConsignmentEntryModel consignmentEntry, final BlProductModel subPartProduct)
	{
		consignmentEntry.getSerialProducts().add(subPartProduct);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
				"Sub part with name {} added to the products list on consignment entry with consignment code {}",
				subPartProduct.getName(), consignmentEntry.getConsignment().getCode());
	}

	/**
	 * Add product options to consignment entry.
	 *
	 * @param consignmentEntry
	 * @param optionsModel
	 */
	private void addProductOptionsToConsignmentEntry(final ConsignmentEntryModel consignmentEntry,
			final BlOptionsModel optionsModel)
	{
		final List<BlOptionsModel> productOptionsToAdd = CollectionUtils.isNotEmpty(consignmentEntry.getOptions())
				? new ArrayList<>(consignmentEntry.getOptions())
				: new ArrayList<>();
		productOptionsToAdd.add(optionsModel);
		consignmentEntry.setOptions(productOptionsToAdd);

		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
				"Product option with name {} added to the options list on consignment entry with consignment code {}",
				optionsModel.getName(), consignmentEntry.getConsignment().getCode());
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void assignSerialAndOrderCodeOnBillingCharges(final ConsignmentEntryModel consignmentEntryModel)
	{
		if (Objects.nonNull(consignmentEntryModel) && MapUtils.isNotEmpty(consignmentEntryModel.getBillingCharges()))
		{
			final String orderCode = getOrderCodeFromConsignmentEntry(consignmentEntryModel);
			doSetSerialAndOrderCodeOnCharges(consignmentEntryModel, orderCode);
		}
	}

	/**
	 * Do set serial and order code on charges.
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 * @param interceptorContext
	 *           the interceptor context
	 * @param orderCode
	 *           the order code
	 */
	private void doSetSerialAndOrderCodeOnCharges(final ConsignmentEntryModel consignmentEntryModel, final String orderCode)
	{
		consignmentEntryModel.getBillingCharges().forEach((serialCode, listOfCharges) -> {
			if (CollectionUtils.isNotEmpty(listOfCharges))
			{
				listOfCharges.forEach(charge -> {
					charge.setOrderCode(orderCode);
					charge.setSerialCode(serialCode);
					getModelService().save(charge);
				});
			}
		});
	}

	/**
	 * Gets the order code from consignment entry.
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 * @return the order code from consignment entry
	 */
	private String getOrderCodeFromConsignmentEntry(final ConsignmentEntryModel consignmentEntryModel)
	{
		return Objects.nonNull(consignmentEntryModel) && Objects.nonNull(consignmentEntryModel.getConsignment())
				&& Objects.nonNull(consignmentEntryModel.getConsignment().getOrder())
						? consignmentEntryModel.getConsignment().getOrder().getCode()
						: StringUtils.EMPTY;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public ConsignmentEntryModel getConsignmentEntryFromOrderForSerial(final OrderModel order, final String serialCode)
	{
		ConsignmentEntryModel consignmentEntryModel = null;
		if (StringUtils.isNotBlank(serialCode) && Objects.nonNull(order) && CollectionUtils.isNotEmpty(order.getConsignments()))
		{
			final Set<ConsignmentModel> consignments = order.getConsignments();
			for (final ConsignmentModel consignment : consignments)
			{
				final Set<ConsignmentEntryModel> consignmentEntries = consignment.getConsignmentEntries();
				if (CollectionUtils.isNotEmpty(consignmentEntries) && Objects.isNull(consignmentEntryModel))
				{
					final Optional<ConsignmentEntryModel> consignmentEntryFromConsignment = consignmentEntries.stream()
							.filter(consignmentEntry -> consignmentEntry.getSerialProducts().stream()
									.anyMatch(product -> serialCode.equalsIgnoreCase(product.getCode())))
							.findFirst();
					consignmentEntryModel = consignmentEntryFromConsignment.isPresent() ? consignmentEntryFromConsignment.get() : null;
				}
			}
		}
		return consignmentEntryModel;
	}

	/**
	 * @return the blConsignmentDao
	 */
	public BlConsignmentDao getBlConsignmentDao()
	{
		return blConsignmentDao;
	}

	/**
	 * @param blConsignmentDao
	 *           the blConsignmentDao to set
	 */
	public void setBlConsignmentDao(final BlConsignmentDao blConsignmentDao)
	{
		this.blConsignmentDao = blConsignmentDao;
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
	 * @return the sessionService
	 */
	public SessionService getSessionService()
	{
		return sessionService;
	}

	/**
	 * @param sessionService
	 *           the sessionService to set
	 */
	public void setSessionService(final SessionService sessionService)
	{
		this.sessionService = sessionService;
	}

	/**
	 * @return the searchRestrictionService
	 */
	public SearchRestrictionService getSearchRestrictionService()
	{
		return searchRestrictionService;
	}

	/**
	 * @param searchRestrictionService
	 *           the searchRestrictionService to set
	 */
	public void setSearchRestrictionService(final SearchRestrictionService searchRestrictionService)
	{
		this.searchRestrictionService = searchRestrictionService;
	}

}
