package com.bl.backoffice.wizards.handler;

import com.bl.constants.BlloggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.BillingInfoStatus;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.order.impl.DefaultBlCalculationService;
import com.bl.core.services.order.BlOrderService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.hybris.cockpitng.dataaccess.facades.object.exceptions.ObjectSavingException;
import com.hybris.cockpitng.engine.WidgetInstanceManager;
import com.hybris.cockpitng.widgets.baseeditorarea.DefaultEditorAreaLogicHandler;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created to call avalara while saving the order from cscockpit
 *  @author Manikandan
 */
public class BlDefaultEditorAreaLogicHandler extends DefaultEditorAreaLogicHandler {

  private static final Logger LOG = Logger.getLogger(BlDefaultEditorAreaLogicHandler.class);

  private DefaultBlCalculationService defaultBlCalculationService;

	@Resource(name = "modelService")
	ModelService modelService;

	@Resource(name = "blStockLevelDao")
	private BlStockLevelDao blStockLevelDao;

	@Resource(name = "blOrderService")
	BlOrderService blOrderService;
	@Resource(name = "orderDao")
	private BlOrderDao orderDao;



	private DefaultBlESPEventService defaultBlESPEventService;

	private static final List<String> LIST_OF_OC_LOCATIONS = Arrays
			.asList(BlCoreConstants.FEDEX, BlCoreConstants.UPS,BlCoreConstants.USPS);


	/**
   * This method call when order is saving
   */
  @Override
  public Object performSave(WidgetInstanceManager widgetInstanceManager, Object currentObject) throws ObjectSavingException {
    if (currentObject instanceof OrderModel) {
			OrderModel orderModel = (OrderModel) currentObject;
			orderModel.setCalculated(false);
			final List<AbstractOrderEntryModel> previousChangedOrderEntriesList = getPreviousChangedOrderEntrysList(
					orderModel);
			final List<BlSerialProductModel> blSerialProductModels = new ArrayList<>();
			// checking is any bundle entry present or not either previous or after add.
			if (CollectionUtils.isNotEmpty(getBundleEntryList(orderModel.getEntries(),previousChangedOrderEntriesList))) {

				// creating and updating order entries if any bundle entry created.
				createEntryForBundleIfBundleProductAdded(orderModel.getEntries(),currentObject);

				// if any entry remove .
				final List<AbstractOrderEntryModel> cloneOfOriginalList = new ArrayList<>();
				cloneOfOriginalList.addAll(previousChangedOrderEntriesList);

				if (CollectionUtils.isNotEmpty(cloneOfOriginalList)) {
					final List<AbstractOrderEntryModel> updatedOrderEntry = orderModel.getEntries();
					cloneOfOriginalList.removeIf(updatedOrderEntry::contains);

					if (CollectionUtils.isNotEmpty(cloneOfOriginalList)) {
						final List<AbstractOrderEntryModel> bundleEntryRemoveList = cloneOfOriginalList.stream().
								filter(entry -> entry.isBundleMainEntry() || entry.isBundleEntry())
								.collect(Collectors.toList());
						if (CollectionUtils.isNotEmpty(bundleEntryRemoveList)) {
							final List<AbstractOrderEntryModel> allExistingBundleEntryList = previousChangedOrderEntriesList
									.stream()
									.filter(entry -> entry.isBundleMainEntry() || entry.isBundleEntry()).collect(
											Collectors.toList());
							final List<AbstractOrderEntryModel> removeEntryList = gettingAllRemovedEntry(
									cloneOfOriginalList, allExistingBundleEntryList);



							checkRemovedEntriesSerials(removeEntryList , orderModel , blSerialProductModels);
							// removing consignment from removed entry list
							removeEntryFromConsignment(orderModel,
									removeEntryList.stream().filter(entryModel -> !entryModel.isBundleMainEntry())
											.collect(
													Collectors.toList()));

							// getting Remaining entry list.
							previousChangedOrderEntriesList.removeIf(removeEntryList::contains);
							//update and save remaining entry.
							((OrderModel) currentObject).setEntries(previousChangedOrderEntriesList);
						} else {
							checkRemovedEntriesSerials(cloneOfOriginalList , orderModel , blSerialProductModels);
							removeEntryFromConsignment(orderModel, cloneOfOriginalList);
						}
					}
				}

			} else {
				if (CollectionUtils.isNotEmpty(previousChangedOrderEntriesList)) {
					final List<AbstractOrderEntryModel> updatedOrderEntry = orderModel.getEntries();
					previousChangedOrderEntriesList.removeIf(updatedOrderEntry::contains);
				}
				checkRemovedEntriesSerials(previousChangedOrderEntriesList , orderModel , blSerialProductModels);
				removeEntryFromConsignment(orderModel, previousChangedOrderEntriesList);

			}
        orderModel.getEntries().forEach(abstractOrderEntryModel -> {
        	abstractOrderEntryModel.setCalculated(Boolean.FALSE);
				});
     	final Object object = super.performSave(widgetInstanceManager, currentObject); // to call parent class before recalculating order.
			try {
				if (BooleanUtils.isFalse(orderModel.getInternalTransferOrder())) {
					getDefaultBlCalculationService().recalculateOrderForTax(orderModel);
					try {
						performSendOrderPullBackItemsRemovedESPEventService(blSerialProductModels , orderModel);
					}
					catch (final Exception e) {
						BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY ,  e,
								"BlDefaultEditorAreaLogicHandler :Exception while performing Order pull back removed ESP Event for order {}", orderModel.getCode());
					}
				}
			} catch (CalculationException e) {
				BlLogger.logMessage(LOG, Level.ERROR, "Error while BlDefaultEditorAreaLogicHandler", e);
			}


      return object;
     }

		//  BL-1311: changes related to tracking new bill creation.
		if (currentObject instanceof ConsignmentEntryModel) {
			final ConsignmentEntryModel consignmentEntryModel = (ConsignmentEntryModel) currentObject;
			final Map<String, List<BlItemsBillingChargeModel>> previousChangedBillingCharges = getPreviousChangedBillingCharges(
					consignmentEntryModel);
			final Map<String, List<BlItemsBillingChargeModel>> newBillingCharges = consignmentEntryModel
					.getBillingCharges();
			final AtomicBoolean isNewBillAdded = new AtomicBoolean(Boolean.FALSE);
			final AbstractOrderModel orderModel = consignmentEntryModel.getOrderEntry().getOrder();
			if (newBillingCharges.size() > previousChangedBillingCharges.size()) {
				isNewBillAdded.set(Boolean.TRUE);
			} else {
				newBillingCharges.forEach((serialCode, billingChargeModels) -> {
					final List<BlItemsBillingChargeModel> blItemsBillingChargeModels = previousChangedBillingCharges
							.get(serialCode);
					final List cloneOfNewBillingChargesList = new ArrayList<BlItemsBillingChargeModel>();
					cloneOfNewBillingChargesList.addAll(billingChargeModels);
					cloneOfNewBillingChargesList.removeIf(blItemsBillingChargeModels::contains);
					if (CollectionUtils.isNotEmpty(cloneOfNewBillingChargesList)) {
						isNewBillAdded.set(Boolean.TRUE);
					}
				});
			}
			if (isNewBillAdded.get()) {
				orderModel.setOrderBillModifiedDate(new Date());
				orderModel.setUpdatedTime(new Date());
				modelService.save(orderModel);
				modelService.refresh(orderModel);
				BlLogger.logFormattedMessage(LOG, Level.DEBUG,
						"Bill created for order {} at updated time {} and also order got modified at {}",
						orderModel.getCode(), orderModel.getUpdatedTime(),
						orderModel.getOrderBillModifiedDate());
			}
		}

		//  BL-1311: changes related to tracking updation on bill.
		if (currentObject instanceof BlItemsBillingChargeModel) {
			final BlItemsBillingChargeModel billingChargeModel = (BlItemsBillingChargeModel) currentObject;
			if (billingChargeModel.getOrderCode() != null) {
				final AbstractOrderModel order = orderDao.getOrderByCode(billingChargeModel.getOrderCode());
				if (order != null) {
					order.setOrderBillModifiedDate(new Date());
					order.setUpdatedTime(new Date());
					modelService.save(order);
					modelService.refresh(order);
					BlLogger.logFormattedMessage(LOG, Level.DEBUG,
							"Bill updated for order {} at updated time {} and also order got modified at {}",
							order.getCode(), order.getUpdatedTime(),
							order.getOrderBillModifiedDate());
				}
			}
			billingChargeModel.setUpdatedBillTime(new Date());
			billingChargeModel.setBillingStatus(BillingInfoStatus.NEW_BILL);
		}
		if (currentObject instanceof AddressModel) {
			final AddressModel addressModel = (AddressModel) currentObject;
			if (addressModel.getOwner() instanceof OrderModel) {
				final OrderModel orderModel = (OrderModel) addressModel.getOwner();
				orderModel.setOrderModifiedDate(new Date());
				modelService.save(orderModel);
				modelService.refresh(orderModel);
				BlLogger.logFormattedMessage(LOG, Level.DEBUG,
						"Address got updated for order {} at updated time {}",
						orderModel.getCode(), orderModel.getOrderModifiedDate());
			}
		}
    return super.performSave(widgetInstanceManager , currentObject);
  }

	/**
	 * This method created to add serial products from removed entries
	 * @param removeEntryList entries removed from order
	 * @param orderModel order model to get updated
	 * @param blSerialProductModels list of serial products
	 */
	private void checkRemovedEntriesSerials(final List<AbstractOrderEntryModel> removeEntryList, final OrderModel orderModel,
			final List<BlSerialProductModel> blSerialProductModels) {
		if(CollectionUtils.isNotEmpty(removeEntryList) && StringUtils.equalsIgnoreCase(orderModel.getStatus().getCode() ,
				OrderStatus.SHIPPED.getCode())){
			addSerialProductToList(removeEntryList , blSerialProductModels);
		}
	}

	/**
	 * This method created to add the serial product list
	 * @param previousChangedOrderEntriesList removed entries
	 * @param blSerialProductModels list of serial products
	 */
	private void addSerialProductToList(final List<AbstractOrderEntryModel> previousChangedOrderEntriesList, final List<BlSerialProductModel> blSerialProductModels) {
			previousChangedOrderEntriesList.forEach(abstractOrderEntryModel -> {
				if(CollectionUtils.isNotEmpty(abstractOrderEntryModel.getConsignmentEntries())) {
					abstractOrderEntryModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
						if(CollectionUtils.isNotEmpty(consignmentEntryModel.getSerialProducts())) {
							consignmentEntryModel.getSerialProducts().forEach(blProductModel -> {
								if (StringUtils.equalsIgnoreCase(consignmentEntryModel.getConsignment().getStatus().getCode(),
										ConsignmentStatus.BL_SHIPPED.getCode())
										&& blProductModel instanceof BlSerialProductModel  && StringUtils.isNotBlank(((BlSerialProductModel) blProductModel).getOcLocation())
										&& LIST_OF_OC_LOCATIONS.contains(((BlSerialProductModel) blProductModel).getOcLocation().toLowerCase())) {
									blSerialProductModels.add((BlSerialProductModel) blProductModel);}
							});
						} });
				} });
	}


	/**
	 * method is used to remove entry from consignment
	 *
	 * @param orderModel
	 * @param previousChangedOrderEntrysList
	 */
	private void removeEntryFromConsignment(final OrderModel orderModel, final List<AbstractOrderEntryModel> previousChangedOrderEntrysList)
	{
		if (CollectionUtils.isNotEmpty(previousChangedOrderEntrysList))
		{
			final AbstractOrderEntryModel previousChangedOrderEntry = previousChangedOrderEntrysList
					.iterator().next();
			final String orderEntrySkuPk = previousChangedOrderEntry.getProduct().getPk().toString();
			final List<ConsignmentEntryModel> consignmentEntryToRemove = new ArrayList<>();
			final List<ConsignmentModel> consignmentToRemove = new ArrayList<>();
			for (final ConsignmentModel consignment : orderModel.getConsignments())
			{
				removeConsignmentEntry(orderEntrySkuPk, consignmentEntryToRemove, consignment);
			}
			modelService.removeAll(consignmentEntryToRemove);
			removeConsignment(orderModel, consignmentToRemove);
			modelService.removeAll(consignmentToRemove);
			orderModel.setOrderModifiedDate(new Date());
			orderModel.setUpdatedTime(new Date());
		}
	}

	/**
	 * method is used remove consignment entry if no serial is available in it
	 *
	 * @param orderEntrySkuPk
	 * @param consignmentEntryToRemove
	 * @param consignment
	 */
	private void removeConsignmentEntry(final String orderEntrySkuPk, final List<ConsignmentEntryModel> consignmentEntryToRemove,
			final ConsignmentModel consignment)
	{
		for (final ConsignmentEntryModel consignmentEntry : consignment.getConsignmentEntries())
		{
			final List<BlProductModel> updatedSerialList = new ArrayList<>();
			consignmentEntry.getSerialProducts().forEach(serial -> {
				if (serial instanceof BlSerialProductModel
						&& !orderEntrySkuPk.equals(((BlSerialProductModel) serial).getBlProduct().getPk().toString())) // NOSONAR
				{
					updatedSerialList.add(serial);
				}
				else
				{
					updateStockForSerial(consignment, serial);
				}
			});
			if (CollectionUtils.isEmpty(updatedSerialList))
			{
				consignmentEntryToRemove.add(consignmentEntry);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment Entry {} removed", consignmentEntry);
			}
			else
			{
				consignmentEntry.setSerialProducts(updatedSerialList);
				modelService.save(consignmentEntry);
				modelService.refresh(consignmentEntry);
			}
		}
	}

	/**
	 * method is used to update stock from removed entry
	 * @param consignment
	 * @param serial
	 */
	private void updateStockForSerial(final ConsignmentModel consignment, final BlProductModel serial)
	{
		if (serial instanceof BlSerialProductModel)
		{
			final Collection<StockLevelModel> findSerialStockLevelForDate = blStockLevelDao.findSerialStockLevelForDate(
					serial.getCode(), consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate());
			if (CollectionUtils.isNotEmpty(findSerialStockLevelForDate))
			{
				findSerialStockLevelForDate.forEach(stockLevel -> {
					stockLevel.setHardAssigned(false);
					stockLevel.setReservedStatus(false);
					stockLevel.setOrder(null);
					((BlSerialProductModel) serial).setHardAssigned(false); // NOSONAR
					modelService.save(stockLevel);
					modelService.save(serial);
				});
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock level updated for serial {}",serial );
			}
		}
	}

	/**
	 * method is used to remove consignment if all the consignment entries has been removed
	 *
	 * @param orderModel
	 * @param consignmentToRemove
	 */
	private void removeConsignment(final OrderModel orderModel, final List<ConsignmentModel> consignmentToRemove)
	{
		for (final ConsignmentModel consignment : orderModel.getConsignments())
		{
			modelService.refresh(consignment);
			if (CollectionUtils.isEmpty(consignment.getConsignmentEntries()))
			{
				consignmentToRemove.add(consignment);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment {} removed", consignment.getCode());
			}
		}
	}

	/**
	 * method is used to get the original value for order entry
	 * @param orderModel
	 * @return
	 */
	private List<AbstractOrderEntryModel> getPreviousChangedOrderEntrysList(final AbstractOrderModel orderModel)
	{
		final Object previousValue = orderModel.getItemModelContext().getOriginalValue(BlloggingConstants.ORIGINAL_VALUE);
		if (previousValue instanceof List)
		{
			return Lists.newArrayList((List) previousValue);
		}
		return Collections.emptyList();
	}

	/**
	 * method is used to get the original value for billing list Map.
	 * @param consignmentEntryModel
	 * @return
	 */
	private Map<String, List<BlItemsBillingChargeModel>> getPreviousChangedBillingCharges(final ConsignmentEntryModel consignmentEntryModel)
	{
		final Object previousValue = consignmentEntryModel.getItemModelContext().getOriginalValue(
				BlloggingConstants.BILLING_CHARGES);
		if (previousValue instanceof Map)
		{
			return (Map<String, List<BlItemsBillingChargeModel>>)  previousValue;
		}
		return (Map<String, List<BlItemsBillingChargeModel>>) Collections.emptyList();
	}

	/**
	 * This method used to check is bundle entry.
	 */
	private List<AbstractOrderEntryModel> getBundleEntryList(
			final List<AbstractOrderEntryModel> updatedList,
			final List<AbstractOrderEntryModel> previousList) {
		List<AbstractOrderEntryModel> bundleOrderEntries = updatedList.stream()
				.filter(entry -> entry.isBundleMainEntry() || entry.isBundleEntry()).collect(
						Collectors.toList());
		bundleOrderEntries = CollectionUtils.isNotEmpty(bundleOrderEntries) ? bundleOrderEntries
				: previousList.stream()
						.filter(entry -> entry.isBundleMainEntry() || entry.isBundleEntry()).collect(
								Collectors.toList());
		return bundleOrderEntries;
	}

	/**
	 * Creating entry for bundle product.
	 */
	private void createEntryForBundleIfBundleProductAdded(
			final List<AbstractOrderEntryModel> updatedList, final Object currentObject) {
		// To check any bundle entry added.
		final List<AbstractOrderEntryModel> newCreatedMainBundleEntry = updatedList.stream()
				.filter(entry -> entry.isBundleMainEntry() && !entry.isEntryCreated()).collect(
						Collectors.toList());
		if (CollectionUtils.isNotEmpty(newCreatedMainBundleEntry)) {
			newCreatedMainBundleEntry.forEach(entry -> {
				blOrderService.createAllEntryForBundleProduct(entry);
			});
			((OrderModel) currentObject)
					.setEntries(newCreatedMainBundleEntry.get(0).getOrder().getEntries());
		}
	}

	/**
	 * Getting all entries which need to remove.
	 */
	private List<AbstractOrderEntryModel> gettingAllRemovedEntry(
			final List<AbstractOrderEntryModel> cloneOfOriginalList,
			final List<AbstractOrderEntryModel> allExistingBundleEntryList) {
		List<AbstractOrderEntryModel> removeEntry = new ArrayList<>();
		cloneOfOriginalList.forEach(entry -> {
			if (entry.isBundleMainEntry() || entry.isBundleEntry()) {
				removeEntry.addAll(allExistingBundleEntryList.stream().filter(
						entryModel -> entry.getBundleProductCode()
								.equals(entryModel.getBundleProductCode())).collect(
						Collectors.toList()));
			} else {
				removeEntry.add(entry);
			}
		});
		// removing duplicate entry which present in remove entry list and return it.
		return new ArrayList<>(
				new HashSet<>(removeEntry));
	}



	/**
	 * This method created to trigger the ESP event for order pull back items removed ESP event
	 * @param blSerialProductModels items removed from order
	 * @param orderModel order model to get the values
	 */
	private void performSendOrderPullBackItemsRemovedESPEventService(final List<BlSerialProductModel> blSerialProductModels, final OrderModel orderModel) {
		if(CollectionUtils.isNotEmpty(blSerialProductModels)) {
			getDefaultBlESPEventService().sendOrderPullBackItemsRemoved(orderModel , blSerialProductModels);
		}
	}

  public DefaultBlCalculationService getDefaultBlCalculationService() {
    return defaultBlCalculationService;
  }

  public void setDefaultBlCalculationService(
      DefaultBlCalculationService defaultBlCalculationService) {
    this.defaultBlCalculationService = defaultBlCalculationService;
  }

	public DefaultBlESPEventService getDefaultBlESPEventService() {
		return defaultBlESPEventService;
	}

	public void setDefaultBlESPEventService(
			DefaultBlESPEventService defaultBlESPEventService) {
		this.defaultBlESPEventService = defaultBlESPEventService;
	}

}
