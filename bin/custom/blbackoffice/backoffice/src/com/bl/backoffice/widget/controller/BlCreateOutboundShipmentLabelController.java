package com.bl.backoffice.widget.controller;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.internal.dao.GenericDao;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import javax.annotation.Resource;

import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Lists;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.CarrierEnum;
import com.bl.core.model.OptimizedShippingMethodModel;
import com.bl.core.services.order.BlOrderService;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.facades.BlCreateShipmentFacade;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Maps;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


/**
 * This class is responsible to create an outbound shipping label
 *
 * @author Ravikumar
 */

public class BlCreateOutboundShipmentLabelController extends DefaultWidgetController
{
	@Wire
	private Combobox shippingTypeComboBox;
	@Wire
	private Combobox optimizedShippingMethodComboBox;
	@Wire
	private Textbox deliveryDate;
	@Wire
	private Textbox destinationState;
	@Wire
	private Checkbox signatureSelection;

	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	@Resource(name = "blCreateShipmentFacade")
	private BlCreateShipmentFacade blCreateShipmentFacade;

	@Resource(name = "blOptimizedShippingMethodGenericDao")
	private GenericDao<OptimizedShippingMethodModel> blOptimizedShippingMethodGenericDao;

	@Resource(name = "blShipmentCreationService")
	private DefaultBLShipmentCreationService blShipmentCreationService;

	@Resource(name = "modelService")
	private ModelService modelService;

	@Resource(name = "blOrderService")
	private BlOrderService blOrderService;

	private ListModelList<String> shippingTypeList = new ListModelList<>();
	private ListModelList<String> optimizedShippingMethodList = new ListModelList<>();

	Map<String, OptimizedShippingMethodModel> optimizedMethodList = Maps.newHashMap();

	ConsignmentModel selectedConsignment = new ConsignmentModel();

	private static final Logger LOG = Logger.getLogger(BlCreateOutboundShipmentLabelController.class);

	/**
	 * BLS-148 - This method is used to load the UPS as default value at the time of opening the popup
	 *
	 * @param inputObject
	 */
	@SocketEvent(socketId = BlInventoryScanLoggingConstants.SOCKET_ID)
	public void initCustomerAddressForm(final ConsignmentModel inputObject)
	{
		selectedConsignment = inputObject;
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.outbound.label.heading")));
		shippingTypeList = new ListModelList<>(getShippingTypeList());
		shippingTypeList.addToSelection(CarrierEnum.UPS.getCode());
		shippingTypeComboBox.setModel(shippingTypeList);
		setOptimizedShippingMethodComboBox(CarrierEnum.UPS.getCode());
		deliveryDate.setValue(getDeliveryDateFromOrder(inputObject));
		destinationState.setValue(getDestinationStateValue(inputObject));
		setSignatureRequiredForUPS();
	}

	private String getDeliveryDateFromOrder(final ConsignmentModel consignment)
	{
		final AbstractOrderModel orderModel = consignment.getOrder();
		if(Objects.nonNull(orderModel))
		{
			if(getBlOrderService().isRentalOrderOnly(orderModel) && Objects.nonNull(orderModel.getRentalStartDate()))
			{
				return convertDateToString(orderModel.getRentalStartDate(), BlCoreConstants.DELIVERY_DATE_FORMAT,
						TimeZone.getTimeZone(BlDeliveryModeLoggingConstants.ZONE_PST));
			}
			if(getBlOrderService().isUsedOrderOnly(orderModel) && Objects.nonNull(orderModel.getActualRentalStartDate()))
			{
				return convertDateToString(orderModel.getActualRentalStartDate(), BlCoreConstants.DELIVERY_DATE_FORMAT,
						TimeZone.getTimeZone(BlDeliveryModeLoggingConstants.ZONE_PST));
			}
		}
		return StringUtils.EMPTY;
	}

	private String getDestinationStateValue(final ConsignmentModel consignment)
	{
		final AbstractOrderModel orderModel = consignment.getOrder();
		if(Objects.nonNull(orderModel) && Objects.nonNull(orderModel.getDeliveryAddress())
				&& Objects.nonNull(orderModel.getDeliveryAddress().getRegion()))
		{
			final String destinationStateName = orderModel.getDeliveryAddress().getRegion().getName();
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Destination State - {}", destinationStateName);
			return destinationStateName;
		}
		return StringUtils.EMPTY;
	}

	/**
	 * This method is used to close the Generate Outbound Label Popup
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	/**
	 * this method will be used to generate outbound label shipment
	 *
	 * @throws IOException
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.GENERATE_OUTBOUND_LABEL, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void generateOutboundLabel() throws IOException
	{
		
		final Map<String, Integer> sequenceMap = new HashedMap();
		getModelService().refresh(selectedConsignment);
		final List<PackagingInfoModel> packages = selectedConsignment.getPackaginginfos();
		final int packageCount = packages.size();
		final Map<String, Integer> sequenceNumber = getBlShipmentCreationService().getSequenceNumber(sequenceMap, packages,
				packageCount);
		final String selectedShippingType = getSelectedShippingType();
		CarrierEnum carrier = null;
		final String selectedOptimizedShippingMethod = getSelectedOptimizedShippingMethod();
		OptimizedShippingMethodModel selectedOptimizedShippingMethodModel = null;
		if (StringUtils.isNotBlank(selectedShippingType)
				&& BooleanUtils.isFalse(selectedShippingType.equals(BlintegrationConstants.DEFAULT_SHIPPING_CODE)))
		{
			for (final Map.Entry<String, OptimizedShippingMethodModel> entry : optimizedMethodList.entrySet())
			{
				if (entry.getKey().equals(selectedOptimizedShippingMethod))
				{
					selectedOptimizedShippingMethodModel = entry.getValue();
					break;
				}
			}
			carrier = selectedShippingType.equals(CarrierEnum.UPS.getCode()) ? CarrierEnum.UPS : CarrierEnum.FEDEX;
		}
		
		final boolean isOptimizedShippingMethodChanged = StringUtils.isNotBlank(selectedShippingType)
				&& BooleanUtils.isFalse(selectedShippingType.equals(BlintegrationConstants.DEFAULT_SHIPPING_CODE))
				&& Objects.nonNull(carrier) && Objects.nonNull(selectedOptimizedShippingMethodModel);
		final boolean isSignatureRequired = getSignatureRequired();
		final List<String> errorPackages = Lists.newArrayList();
		if (carrier.getCode().equals(CarrierEnum.FEDEX) && isSignatureRequired)
		{
			Messagebox.show("Signature cannot be selected for FEDEX : ", BlCoreConstants.ERROR_TITLE, Messagebox.OK, Messagebox.ERROR);
		}
		for (final PackagingInfoModel packagingInfoModel : packages)
		{
			processLabelCreation(packageCount, sequenceNumber, carrier, selectedOptimizedShippingMethodModel,
					isOptimizedShippingMethodChanged, errorPackages, packagingInfoModel, isSignatureRequired);
		}
		if (CollectionUtils.isNotEmpty(errorPackages))
		{
			Messagebox.show("Error while generating outbound label for packages with ID : " + String.join(", ", errorPackages),
					BlCoreConstants.ERROR_TITLE, Messagebox.OK, Messagebox.ERROR);
		}
		else
		{
			Messagebox.show("Outbound Label Generated Successfully", BlintegrationConstants.POPUP_TEXT, Messagebox.OK, "icon");
		}
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	private void processLabelCreation(final int packageCount, final Map<String, Integer> sequenceNumber, final CarrierEnum carrier,
			final OptimizedShippingMethodModel selectedOptimizedShippingMethodModel, final boolean isOptimizedShippingMethodChanged,
			final List<String> errorPackages, final PackagingInfoModel packagingInfoModel, final boolean isSignatureRequired)
	{
		try
		{
			if (isOptimizedShippingMethodChanged)
			{
				final boolean isSuccess = getBlCreateShipmentFacade().createBlShipmentPackages(packagingInfoModel, packageCount,
						sequenceNumber, carrier, selectedOptimizedShippingMethodModel, isSignatureRequired);
				if (BooleanUtils.isFalse(isSuccess))
				{
					errorPackages.add(packagingInfoModel.getPackageId());
				}
			}
			else
			{
				final boolean isSuccess = getBlCreateShipmentFacade().createBlShipmentPackages(packagingInfoModel, packageCount,
						sequenceNumber, isSignatureRequired);
				if (BooleanUtils.isFalse(isSuccess))
				{
					errorPackages.add(packagingInfoModel.getPackageId());
				}
			}
		}
		catch (final ParseException | IOException exception)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Exception occurred while generating shipment label", exception);
		}
	}

	private String getSelectedOptimizedShippingMethod()
	{
		return Objects.nonNull(this.optimizedShippingMethodComboBox.getSelectedItem())
				? this.optimizedShippingMethodComboBox.getSelectedItem().getValue()
				: StringUtils.EMPTY;
	}

	private boolean getSignatureRequired()
	{
		return this.signatureSelection.isChecked();
	}

	private String getSelectedShippingType()
	{
		return Objects.nonNull(this.shippingTypeComboBox.getSelectedItem()) ? this.shippingTypeComboBox.getSelectedItem().getValue()
				: StringUtils.EMPTY;
	}

	/**
	 * method will be called when any value change for shipping type combobox
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SHIPPING_TYPE_COMBOBOX, eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeOptimizedShippingType()
	{
		final String selectedShippingType = this.shippingTypeComboBox.getSelectedItem().getValue();
		shippingTypeList.addToSelection(selectedShippingType);
		setOptimizedShippingMethodComboBox(selectedShippingType);
		if (selectedShippingType.equalsIgnoreCase("FEDEX"))
		{
			signatureSelection.setChecked(Boolean.FALSE);
			signatureSelection.setDisabled(Boolean.TRUE);
		}
		else
		{
			setSignatureRequiredForUPS();
		}
	}

	private void setOptimizedShippingMethodComboBox(final String selectedShippingType)
	{
		final List<String> carrierBasedOptimizedShippingMethodList = Lists.newArrayList();
		final List<OptimizedShippingMethodModel> allOptimizedShippingMethodList = getBlOptimizedShippingMethodGenericDao().find();
		allOptimizedShippingMethodList.forEach(
				optimizedShippingMethod -> optimizedMethodList.put(optimizedShippingMethod.getName(), optimizedShippingMethod));
		if (selectedShippingType.equalsIgnoreCase(CarrierEnum.UPS.getCode()))
		{
			optimizedShippingMethodList.clearSelection();
			for (final OptimizedShippingMethodModel optimizedShippingMethod : allOptimizedShippingMethodList)
			{
				if (!optimizedShippingMethod.getCode().toLowerCase().contains(CarrierEnum.FEDEX.getCode().toLowerCase()))
				{
					carrierBasedOptimizedShippingMethodList.add(optimizedShippingMethod.getName());
				}

			}
		}
		else if (selectedShippingType.equalsIgnoreCase(CarrierEnum.FEDEX.getCode()))
		{
			optimizedShippingMethodList.clearSelection();
			for (final OptimizedShippingMethodModel optimizedShippingMethod : allOptimizedShippingMethodList)
			{
				if (optimizedShippingMethod.getCode().toLowerCase().contains(CarrierEnum.FEDEX.getCode().toLowerCase()))
				{
					carrierBasedOptimizedShippingMethodList.add(optimizedShippingMethod.getName());
				}

			}
		}
		else
		{
			carrierBasedOptimizedShippingMethodList.clear();
			optimizedShippingMethodList.clearSelection();
		}
		optimizedShippingMethodList = new ListModelList<>(carrierBasedOptimizedShippingMethodList);
		optimizedShippingMethodComboBox.setModel(optimizedShippingMethodList);

	}

	@ViewEvent(componentID = "optimizedShippingMethodComboBox", eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeOptimizedShippingMethod()
	{
		final String selectedOptimizedShippingMethod = this.optimizedShippingMethodComboBox.getSelectedItem().getValue();
		optimizedShippingMethodList.addToSelection(selectedOptimizedShippingMethod);
	}

	/**
	 * this method will be used to get the value which we will display on warehouse combobox
	 *
	 * @return valueList
	 */
	private List<String> getShippingTypeList()
	{
		final List<String> shippingTypesList = Lists.newArrayList();
		//shippingTypesList.add(BlintegrationConstants.DEFAULT_SHIPPING_CODE);
		shippingTypesList.add(CarrierEnum.UPS.getCode());
		shippingTypesList.add(CarrierEnum.FEDEX.getCode());
		return shippingTypesList;
	}

	/**
	 * This method is used to check the Signature Required checkbox for UPS on outbound label generator depending on the field at shipment level.
	 */
	public void setSignatureRequiredForUPS() {
		if (selectedConsignment.isSignatureRequired() == Boolean.TRUE)
		{
			signatureSelection.setChecked(Boolean.TRUE);
			signatureSelection.setDisabled(Boolean.TRUE);
		}
		else
		{
			signatureSelection.setDisabled(Boolean.FALSE);
		}
	}


	/**
	 * @return the blCreateShipmentFacade
	 */
	public BlCreateShipmentFacade getBlCreateShipmentFacade()
	{
		return blCreateShipmentFacade;
	}


	/**
	 * @param blCreateShipmentFacade
	 *           the blCreateShipmentFacade to set
	 */
	public void setBlCreateShipmentFacade(final BlCreateShipmentFacade blCreateShipmentFacade)
	{
		this.blCreateShipmentFacade = blCreateShipmentFacade;
	}

	public DefaultBLShipmentCreationService getBlShipmentCreationService()
	{
		return blShipmentCreationService;
	}

	public void setBlShipmentCreationService(final DefaultBLShipmentCreationService blShipmentCreationService)
	{
		this.blShipmentCreationService = blShipmentCreationService;
	}

	/**
	 * @return the blOptimizedShippingMethodGenericDao
	 */
	public GenericDao<OptimizedShippingMethodModel> getBlOptimizedShippingMethodGenericDao()
	{
		return blOptimizedShippingMethodGenericDao;
	}

	/**
	 * @param blOptimizedShippingMethodGenericDao
	 *           the blOptimizedShippingMethodGenericDao to set
	 */
	public void setBlOptimizedShippingMethodGenericDao(
			final GenericDao<OptimizedShippingMethodModel> blOptimizedShippingMethodGenericDao)
	{
		this.blOptimizedShippingMethodGenericDao = blOptimizedShippingMethodGenericDao;
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
	 * @return the deliveryDate
	 */
	public Textbox getDeliveryDate()
	{
		return deliveryDate;
	}

	/**
	 * @param deliveryDate
	 *           the deliveryDate to set
	 */
	public void setDeliveryDate(final Textbox deliveryDate)
	{
		this.deliveryDate = deliveryDate;
	}

	/**
	 * @return the blOrderService
	 */
	public BlOrderService getBlOrderService()
	{
		return blOrderService;
	}

	/**
	 * @param blOrderService the blOrderService to set
	 */
	public void setBlOrderService(final BlOrderService blOrderService)
	{
		this.blOrderService = blOrderService;
	}

	/**
	 * This Method converts rental startDate and rental endDate to String
	 */
	private String convertDateToString(final Date deliveryDate , final String dateFormat, final TimeZone timeZone) {
		final SimpleDateFormat sd = new SimpleDateFormat(dateFormat);
		sd.setTimeZone(timeZone);
		final String formattedDeliveryDate = sd.format(deliveryDate);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Delivery Date - {}", formattedDeliveryDate);
		return formattedDeliveryDate;
	}

	/**
	 * @return the destinationState
	 */
	public Textbox getDestinationState()
	{
		return destinationState;
	}

	/**
	 * @param destinationState the destinationState to set
	 */
	public void setDestinationState(final Textbox destinationState)
	{
		this.destinationState = destinationState;
	}

	public Checkbox getSignatureSelection()
	{
		return signatureSelection;
	}

	/**
	 * @param signatureSelection the signatureSelection to set
	 */
	public void setSignatureSelection(final Checkbox signatureSelection)
	{
		this.signatureSelection = signatureSelection;
	}

}

