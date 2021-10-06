package com.bl.backoffice.wizards.handler;

import com.bl.constants.BlloggingConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.impl.DefaultBlCalculationService;
import com.bl.core.services.order.BlOrderService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.hybris.cockpitng.dataaccess.facades.object.exceptions.ObjectSavingException;
import com.hybris.cockpitng.engine.WidgetInstanceManager;
import com.hybris.cockpitng.widgets.baseeditorarea.DefaultEditorAreaLogicHandler;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
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
			// checking is any bundle entry present or not either previous or after add.
			List<AbstractOrderEntryModel> bundleOrderEntries = orderModel.getEntries().stream()
					.filter(entry -> entry.isBundleMainEntry() || entry.isBundleEntry()).collect(
							Collectors.toList());
			bundleOrderEntries = CollectionUtils.isNotEmpty(bundleOrderEntries) ? bundleOrderEntries
					: previousChangedOrderEntriesList.stream()
							.filter(entry -> entry.isBundleMainEntry() || entry.isBundleEntry()).collect(
									Collectors.toList());

			if (CollectionUtils.isNotEmpty(bundleOrderEntries)) {

				// To check any bundle entry added.
				List<AbstractOrderEntryModel> newCreatedMainBundleEntry = orderModel.getEntries().stream()
						.filter(entry -> entry.isBundleMainEntry() && !entry.isEntryCreated()).collect(
								Collectors.toList());
				// if new bundle entry added then creating corresponding entry : creating start.
				if (CollectionUtils.isNotEmpty(newCreatedMainBundleEntry)) {
					newCreatedMainBundleEntry.forEach(entry -> {
						blOrderService.createAllEntryForBundleProduct(entry);
					});
					((OrderModel) currentObject)
							.setEntries(newCreatedMainBundleEntry.get(0).getOrder().getEntries());
				}
				//  creation and update entries done.

				// if any entry remove .
				final List<AbstractOrderEntryModel> cloneOfOriginalList = new ArrayList<>();
				cloneOfOriginalList.addAll(previousChangedOrderEntriesList);

				if (CollectionUtils.isNotEmpty(cloneOfOriginalList)) {
					final List<AbstractOrderEntryModel> updatedOrderEntry = orderModel.getEntries();
					cloneOfOriginalList.removeIf(updatedOrderEntry::contains);

					if (CollectionUtils.isNotEmpty(cloneOfOriginalList)) {
						List<AbstractOrderEntryModel> bundleEntryRemoveList = cloneOfOriginalList.stream().
								filter(entry -> entry.isBundleMainEntry() || entry.isBundleEntry())
								.collect(Collectors.toList());
						if (CollectionUtils.isNotEmpty(bundleEntryRemoveList)) {
							List<AbstractOrderEntryModel> removeEntry = new ArrayList<>();
							final List<AbstractOrderEntryModel> allExistingBundleEntryList = previousChangedOrderEntriesList
									.stream()
									.filter(entry -> entry.isBundleMainEntry() || entry.isBundleEntry()).collect(
											Collectors.toList());

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
							// removing duplicate entry which present in remove entry list
							List<AbstractOrderEntryModel> removeEntryList = new ArrayList<>(
									new HashSet<>(removeEntry));
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
							removeEntryFromConsignment(orderModel, cloneOfOriginalList);
						}
					}
				}

			} else {
				if (CollectionUtils.isNotEmpty(previousChangedOrderEntriesList)) {
					final List<AbstractOrderEntryModel> updatedOrderEntry = orderModel.getEntries();
					previousChangedOrderEntriesList.removeIf(updatedOrderEntry::contains);
				}
				removeEntryFromConsignment(orderModel, previousChangedOrderEntriesList);

			}
        orderModel.getEntries().forEach(abstractOrderEntryModel -> {
        	abstractOrderEntryModel.setCalculated(Boolean.FALSE);
				});
     	final Object object = super.performSave(widgetInstanceManager, currentObject); // to call parent class before recalculating order.
			try {
				if (BooleanUtils.isFalse(orderModel.getInternalTransferOrder())) {
					getDefaultBlCalculationService().recalculateOrderForTax(orderModel);
				}
			} catch (CalculationException e) {
				BlLogger.logMessage(LOG, Level.ERROR, "Error while BlDefaultEditorAreaLogicHandler", e);
			}
      return object;
     }
    return super.performSave(widgetInstanceManager , currentObject);
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

  public DefaultBlCalculationService getDefaultBlCalculationService() {
    return defaultBlCalculationService;
  }

  public void setDefaultBlCalculationService(
      DefaultBlCalculationService defaultBlCalculationService) {
    this.defaultBlCalculationService = defaultBlCalculationService;
  }

}
