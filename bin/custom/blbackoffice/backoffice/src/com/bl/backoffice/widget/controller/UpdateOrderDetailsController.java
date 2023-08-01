package com.bl.backoffice.widget.controller;

import de.hybris.platform.commercefacades.i18n.I18NFacade;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.commerceservices.delivery.DeliveryService;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.Config;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.util.Clients;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlRushDeliveryModeModel;
import com.bl.facades.fexEx.data.SameDayCityReqData;
import com.bl.facades.fexEx.data.SameDayCityResData;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.integration.services.BlFedExSameDayService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


/**
 * ############## BL-630 : CS Cockpit - Shipping Address and Shipping Method related features Orders ############## This
 * class is created to update shipping address and shipping method for order
 *
 * @author Aditi Sharma
 */
public class UpdateOrderDetailsController extends DefaultWidgetController
{
	private static final Logger LOG = Logger.getLogger(UpdateOrderDetailsController.class);

	private static final int DURATION = 2000;

	private static final String SF_ZIPCODE = Config.getString("shipping.sf.zip.code", "94070");
	private static final String NYC_ZIPCODE = Config.getString("shipping.nyc.zip.code", "10004");

	@Wire
	private Textbox firstName;

	@Wire
	private Textbox lastName;

	@Wire
	private Textbox line1;

	@Wire
	private Textbox line2;

	@Wire
	private Textbox postalCode;

	@Wire
	private Textbox town;

	@Wire
	private Textbox countryCode;

	@Wire
	private Textbox contactNo;

	@Wire
	private Combobox regionCombobox;

	@Wire
	private Combobox deliveryModeCombobox;

	@Wire
	private Combobox isPickStoreAddress;

	@Wire
	private Combobox isUPSStoreAddress;

	@Wire
	private Textbox pickupPersonFName;

	@Wire
	private Textbox pickupPersonLName;

	@Wire
	private Textbox pickUpPersonEmail;

	@Wire
	private Textbox pickUpPersonPhone;

	@Wire
	private Combobox isSignatureRequired;

	private OrderModel orderModel;

	private transient ModelService modelService;

	private static ApplicationContext context;

	@Autowired
	private static I18NFacade i18NFacade;

	@Autowired
	private transient CommonI18NService commonI18NService;

	@Resource(name = "checkoutFacade")
	private BlCheckoutFacade checkoutFacade;

	@Autowired
	private DeliveryService deliveryService;

	@Autowired
	private BlFedExSameDayService fedExSameDayServiceImpl;

	@Autowired
	CalculationService calculationService;

	private transient Converter<RegionModel, RegionData> regionConverter;

	private ListModelList<RegionData> listModelList = new ListModelList<>();

	private ListModelList<DeliveryModeModel> deliveryList = new ListModelList<>();

	private ListModelList isPickupStore = new ListModelList();
	private ListModelList isUpsStore = new ListModelList();

	private ListModelList signatureRequired = new ListModelList();

	private final List<RegionData> blRegionCode = i18NFacade.getRegionsForCountryIso(BlInventoryScanLoggingConstants.COUNTRY_CODE);

	boolean isDeliveryModeChange = false;

	static
	{
		try
		{
			context = Registry.getApplicationContext();

			i18NFacade = (I18NFacade) context.getBean("i18NFacade");
		}
		catch (final BeansException ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, ex.getMessage());
		}
	}


	/**
	 * This method is used to show the default values of shipping and address
	 *
	 * @param inputObject
	 */
	@SocketEvent(socketId = "inputObject")
	public void initCustomerAddressForm(final OrderModel inputObject)
	{
		this.setOrderModel(inputObject);
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.updateshipping.confirm.title"))
						.concat(BlInventoryScanLoggingConstants.EMPTY_SPACE).concat(this.getOrderModel().getCode()));
		final AddressModel deliveryAddress = getOrderModel().getDeliveryAddress();
		if (deliveryAddress.getFirstname() != null)
		{
			this.firstName.setValue(deliveryAddress.getFirstname());
		}
		if (deliveryAddress.getLastname() != null)
		{
			this.lastName.setValue(deliveryAddress.getLastname());
		}
		this.line1.setValue(deliveryAddress.getLine1());
		this.line2.setValue(deliveryAddress.getLine2());
		this.postalCode.setValue(deliveryAddress.getPostalcode());
		this.town.setValue(deliveryAddress.getTown());
		this.contactNo.setValue(deliveryAddress.getPhone1());
		this.countryCode.setValue(deliveryAddress.getCountry().getName().concat(BlInventoryScanLoggingConstants.EMPTY_SPACE)
				.concat(BlInventoryScanLoggingConstants.OPEN_BRACKET).concat(deliveryAddress.getCountry().getIsocode())
				.concat(BlInventoryScanLoggingConstants.CLOSE_BRACKET));

		isPickupStore();

		isUpsStore();
		isSignatureRequired();

		getBlRegionData(blRegionCode);

		getBlDeliveryModes();

	}

	/**
	 * This method is called when we will change the value of region
	 */
	@ViewEvent(componentID = "regionCombobox", eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeCountry()
	{
		final List<RegionData> regionForIso = i18NFacade.getRegionsForCountryIso(BlInventoryScanLoggingConstants.COUNTRY_CODE);

		final String regionCode = StringUtils.substringBetween(regionCombobox.getValue(),
				BlInventoryScanLoggingConstants.OPEN_BRACKET, BlInventoryScanLoggingConstants.CLOSE_BRACKET);

		for (final RegionData isoCodeList : regionForIso)
		{
			if (StringUtils.isNotBlank(regionCode))
			{
				if (regionCode.trim().equals(isoCodeList.getIsocode()))
				{
					final RegionData regionData = i18NFacade.getRegion(BlInventoryScanLoggingConstants.COUNTRY_CODE,
							regionCode.trim());
					listModelList.addToSelection(regionData);
					regionCombobox.setModel(listModelList);
					showNotify("Changed to: " + regionCode, regionCombobox);
					break;
				}
			}
			else
			{
				throw new WrongValueException(regionCombobox, this.getLabel("blbackoffice.updateshipping.inValid.regionCode"));
			}
		}

	}

	/**
	 * This method is called when we will change the value of deliveryModeCombobox
	 */
	@ViewEvent(componentID = "deliveryModeCombobox", eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeDeliveryMode()
	{
		final SameDayCityReqData sameDayCityReqData = new SameDayCityReqData();
		validateZipCodeForDelivery(sameDayCityReqData);
		isPickupStore();
		isUpsStore();
		isSignatureRequired();
	}

	/**
	 * This method is called when we will change the value of postalCode
	 */
	@ViewEvent(componentID = "postalCode", eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeZipCode()
	{
		final SameDayCityReqData sameDayCityReqData = new SameDayCityReqData();
		validateZipCodeForDelivery(sameDayCityReqData);
	}

	/**
	 * This method is called when we will change the value of isPickStoreAddress
	 */
	@ViewEvent(componentID = "isPickStoreAddress", eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void isPickUpStoreAddress()
	{
		isPickupStore.addToSelection(this.isPickStoreAddress.getValue());
	}

	/**
	 * This method is called when we will change the value of isUPSStoreAddress
	 */
	@ViewEvent(componentID = "isUPSStoreAddress", eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void isUpsStoreAddress()
	{
		isUpsStore.addToSelection(this.isUPSStoreAddress.getValue());
	}

	/**
	 * This method is used to validate the zip code for delivery
	 *
	 * @param sameDayCityReqData
	 * @param deliveryAddressZipCode
	 * @param warehouseZipCode
	 */
	private void validateZipCodeForDelivery(final SameDayCityReqData sameDayCityReqData)
	{
		AddressModel deliveryAddress = getOrderModel().getDeliveryAddress();
		final ZoneDeliveryModeModel blZoneDeliveryMode = this.deliveryModeCombobox.getSelectedItem().getValue();
		if (blZoneDeliveryMode instanceof BlPickUpZoneDeliveryModeModel)
		{
			isDeliveryModeChange = true;
			final BlPickUpZoneDeliveryModeModel zonedeliveryMode = (BlPickUpZoneDeliveryModeModel) blZoneDeliveryMode;

			if (BlDeliveryModeLoggingConstants.BL_PARTNER_PICKUP.equals(zonedeliveryMode.getShippingGroup().getCode()))
			{
				deliveryAddress.setPickStoreAddress(true);
				deliveryAddress.setUpsStoreAddress(false);
				if (zonedeliveryMode.getInternalStoreAddress() != null)
				{
					final AddressModel pickUpStoreAddress = zonedeliveryMode.getInternalStoreAddress();
					final AddressModel pickupDeliveryAddress = modelService.clone(pickUpStoreAddress);
					pickupDeliveryAddress.setShippingAddress(true);
					deliveryAddress = pickupDeliveryAddress;
				}
			}

			if (BlDeliveryModeLoggingConstants.SHIP_HOLD_UPS_OFFICE.equals(zonedeliveryMode.getShippingGroup().getCode()))
			{
				deliveryAddress.setPickStoreAddress(false);
				deliveryAddress.setUpsStoreAddress(true);
			}
		}
		if (!(blZoneDeliveryMode instanceof BlPickUpZoneDeliveryModeModel))
		{
			isDeliveryModeChange = true;
			deliveryAddress.setPickStoreAddress(false);
			deliveryAddress.setUpsStoreAddress(false);
		}
		getBlRegionDataOnPopup(blRegionCode, deliveryAddress.getRegion());
		getSameDayCityResponse(sameDayCityReqData, blZoneDeliveryMode);
	}

	/**
	 * This method is used to get same day city response
	 *
	 * @param sameDayCityReqData
	 * @param blZoneDeliveryMode
	 */
	private void getSameDayCityResponse(final SameDayCityReqData sameDayCityReqData,
			final ZoneDeliveryModeModel blZoneDeliveryMode)
	{
		this.postalCode.clearErrorMessage();
		if (blZoneDeliveryMode instanceof BlRushDeliveryModeModel)
		{
			final BlRushDeliveryModeModel zonedeliveryMode = (BlRushDeliveryModeModel) blZoneDeliveryMode;

			if (zonedeliveryMode.getShippingGroup().getCode().equals(BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY))
			{
				sameDayCityReqData.setWarehouseZipCode(SF_ZIPCODE);
			}
			else if (zonedeliveryMode.getShippingGroup().getCode().equals(BlDeliveryModeLoggingConstants.NEXT_DAY_RUSH_DELIVERY))
			{
				sameDayCityReqData.setWarehouseZipCode(NYC_ZIPCODE);
			}

			sameDayCityReqData.setDeliveryAddressZipCode(this.postalCode.getValue());

			try
			{
				final SameDayCityResData resData = getFedExSameDayServiceImpl().getAvailability(sameDayCityReqData);

				if (BooleanUtils.isTrue(resData.getServiceApplicable()))
				{
					deliveryList.addToSelection(blZoneDeliveryMode);
					isDeliveryModeChange = true;
				}
				else
				{
					Messagebox.show(BlInventoryScanLoggingConstants.SHIPPING_METHOD_ERROR_MESSAGE);
					throw new WrongValueException(this.postalCode, this.getLabel("blbackoffice.updateshipping.inValid.zipCode"));
				}
			}
			catch (final URISyntaxException ex)
			{
				BlLogger.logMessage(LOG, Level.ERROR, ex.getMessage());
			}
		}
	}

	/**
	 * This method will be used to reset the popup values
	 */
	@ViewEvent(componentID = "undochanges", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void reset()
	{
		this.initCustomerAddressForm(this.getOrderModel());
	}

	/**
	 * This method will be used to confirm/ Save the modified values for shipping
	 */
	@ViewEvent(componentID = "confirmAddress", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void confirmOrder()
	{
		this.validateRequest();
		final AddressModel addressModel = getOrderModel().getDeliveryAddress();
		addressModel.setFirstname(this.firstName.getValue());
		addressModel.setLastname(this.lastName.getValue());
		addressModel.setLine1(this.line1.getValue());
		addressModel.setLine2(this.line2.getValue());
		addressModel.setPostalcode(this.postalCode.getValue());
		addressModel.setTown(this.town.getValue());
		addressModel.setPhone1(this.contactNo.getValue());
		final String regionCode = StringUtils.substringBetween(this.regionCombobox.getValue().trim(),
				BlInventoryScanLoggingConstants.OPEN_BRACKET, BlInventoryScanLoggingConstants.CLOSE_BRACKET);
		try
		{
			final RegionModel regionModel = getCommonI18NService()
					.getRegion(getCommonI18NService().getCountry(addressModel.getCountry().getIsocode()), regionCode);
			addressModel.setRegion(regionModel);
		}
		catch (final UnknownIdentifierException e)
		{
			throw new ConversionException("No region with the code " + regionCode + " found.", e);
		}

		final String deliveryModeToSet = this.deliveryModeCombobox.getValue();

		final DeliveryModeModel deliveryModeModel = getDeliveryService().getDeliveryModeForCode(deliveryModeToSet.trim());
		getOrderModel().setDeliveryMode(deliveryModeModel);

		getOrderModel().getDeliveryAddress().setPickStoreAddress(
				"true".equalsIgnoreCase(this.isPickStoreAddress.getValue().trim()) ? Boolean.TRUE : Boolean.FALSE);

		getOrderModel().getDeliveryAddress().setPickStoreAddress(
				"true".equalsIgnoreCase(this.isUPSStoreAddress.getValue().trim()) ? Boolean.TRUE : Boolean.FALSE);

		getOrderModel().getDeliveryAddress().setSignatureRequired(
				"true".equalsIgnoreCase(this.isSignatureRequired.getValue().trim()) ? Boolean.TRUE : Boolean.FALSE);

		modelService.save(addressModel);
		modelService.refresh(addressModel);
		orderModel.setOrderModifiedDate(new Date());
		modelService.save(orderModel);
		modelService.refresh(orderModel);
		if (BooleanUtils.isTrue(isDeliveryModeChange))
		{
			try
			{
				getOrderModel().setCalculated(false);
				modelService.save(orderModel);
				modelService.refresh(orderModel);
				getCalculationService().calculate(getOrderModel());
				isDeliveryModeChange = false;
			}
			catch (final CalculationException ex)
			{
				BlLogger.logMessage(LOG, Level.ERROR, ex.getMessage());
			}
		}
		this.showMessageBox();
	}

	/**
	 * This method is used to check isUpsStore or not
	 */
	private void isUpsStore()
	{
		isUpsStore = new ListModelList<>(getBooleanValueList());

		final boolean isUpsStoreAddSelected = getOrderModel().getDeliveryAddress().getUpsStoreAddress()
				? isUpsStore.addToSelection(Boolean.TRUE)
				: isUpsStore.addToSelection(Boolean.FALSE);

		isUPSStoreAddress.setModel(isUpsStore);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "isUPSAddessSelected {}", isUpsStoreAddSelected);
	}

	/**
	 * This method is used to check isPickupStore or not
	 */
	private void isPickupStore()
	{
		isPickupStore = new ListModelList<>(getBooleanValueList());

		final boolean isPickupAddSelected = getOrderModel().getDeliveryAddress().getPickStoreAddress()
				? isPickupStore.addToSelection(Boolean.TRUE)
				: isPickupStore.addToSelection(Boolean.FALSE);
		isPickStoreAddress.setModel(isPickupStore);

		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "isPickUpAddessSelected {}", isPickupAddSelected);
	}

	/**
	 * This method will be used to validate modified data for shipping
	 */
	protected void validateRequest()
	{

		if (StringUtils.isEmpty(this.line1.getValue()))
		{
			throw new WrongValueException(this.line1, this.getLabel("blbackoffice.updateshipping.missing.line1"));
		}

		if (StringUtils.isEmpty(this.town.getValue()))
		{
			throw new WrongValueException(this.town, this.getLabel("blbackoffice.updateshipping.missing.town"));
		}

		if (StringUtils.isEmpty(this.regionCombobox.getValue()))
		{
			throw new WrongValueException(this.regionCombobox, this.getLabel("blbackoffice.updateshipping.missing.region"));
		}

		if (StringUtils.isEmpty(this.deliveryModeCombobox.getValue()))
		{
			throw new WrongValueException(this.deliveryModeCombobox,
					this.getLabel("blbackoffice.updateshipping.missing.deliveryMode"));
		}

		if (StringUtils.isEmpty(this.postalCode.getValue()))
		{
			throw new WrongValueException(this.postalCode, this.getLabel("blbackoffice.updateshipping.missing.postalCode"));
		}

		if (StringUtils.isEmpty(this.contactNo.getValue()))
		{
			throw new WrongValueException(this.contactNo, this.getLabel("blbackoffice.updateshipping.missing.contactNo"));
		}

	}


	/**
	 * This method is used to get boolean value for ups store
	 *
	 * @return valueList
	 */
	private List<Boolean> getBooleanValueList()
	{
		final List<Boolean> valueList = new ArrayList<>();
		valueList.add(Boolean.TRUE);
		valueList.add(Boolean.FALSE);
		return valueList;
	}

	/**
	 * This method is used to get the delivery modes
	 */
	private void getBlDeliveryModes()
	{
		final Collection<ZoneDeliveryModeModel> deliveyModeList = getCheckoutFacade().getAllBlDeliveryModes();
		deliveryList = new ListModelList<>(deliveyModeList);
		deliveryList.addToSelection(getOrderModel().getDeliveryMode());
		deliveryModeCombobox.setModel(deliveryList);
	}

	/**
	 * This method is used to get BlRegionData
	 *
	 * @param regionCodeList
	 * @param blRegionCode
	 */
	private void getBlRegionData(final List<RegionData> blRegionCode)
	{
		final List<RegionData> regionCodeList = new ArrayList<>();
		regionCodeList.addAll(blRegionCode);
		listModelList = new ListModelList<>(regionCodeList);

		final RegionData selectedRegion = getRegionConverter().convert(getOrderModel().getDeliveryAddress().getRegion());

		for (final RegionData regionData : listModelList)
		{
			if (regionData.getIsocode().equals(selectedRegion.getIsocode()))
			{
				listModelList.addToSelection(regionData);
			}
		}

		regionCombobox.setModel(listModelList);
	}


	/**
	 * This method is called when we will change the value of isSignatureRequired
	 */
	@ViewEvent(componentID = "isSignatureRequired", eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void signatureRequired()
	{
		signatureRequired.addToSelection(this.isSignatureRequired.getValue());
	}

	/**
	 * This method is used to check isSignatureRequired or not
	 */
	private void isSignatureRequired()
	{
		signatureRequired = new ListModelList<>(getBooleanValueList());

		final boolean isSignatureRequiredSelected = getOrderModel().getDeliveryAddress().isSignatureRequired()
				? signatureRequired.addToSelection(Boolean.TRUE)
				: signatureRequired.addToSelection(Boolean.FALSE);
		isSignatureRequired.setModel(signatureRequired);

		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "isSignatureRequiredSelected {}", isSignatureRequiredSelected);
	}

	/**
	 * This method is used to get region date on popup
	 *
	 * @param regionCodeList
	 * @param blRegionCode
	 */
	private void getBlRegionDataOnPopup(final List<RegionData> blRegionCode, final RegionModel regionModel)
	{
		final List<RegionData> regionCodeList = new ArrayList<>();
		regionCodeList.addAll(blRegionCode);
		listModelList = new ListModelList<>(regionCodeList);

		final RegionData selectedRegion = getRegionConverter().convert(regionModel);

		for (final RegionData regionData : listModelList)
		{
			if (regionData.getIsocode().equals(selectedRegion.getIsocode()))
			{
				listModelList.addToSelection(regionData);
			}
		}

		regionCombobox.setModel(listModelList);
	}


	/**
	 * This method will be used to show success message
	 */
	protected void showMessageBox()
	{
		Messagebox.show(BlInventoryScanLoggingConstants.SHIPPING_DETAILS_UPDATE_MESSGAE);

	}

	/**
	 * This method will be used to show notification
	 *
	 * @param msg
	 * @param ref
	 */
	private void showNotify(final String msg, final Component ref)
	{
		Clients.showNotification(msg, "info", ref, "end_center", DURATION);
	}

	/**
	 * @return the orderModel
	 */
	public OrderModel getOrderModel()
	{
		return orderModel;
	}

	/**
	 * @param orderModel
	 *           the orderModel to set
	 */
	public void setOrderModel(final OrderModel orderModel)
	{
		this.orderModel = orderModel;
	}

	/**
	 * @return the commonI18NService
	 */
	public CommonI18NService getCommonI18NService()
	{
		return commonI18NService;
	}

	/**
	 * @param commonI18NService
	 *           the commonI18NService to set
	 */
	public void setCommonI18NService(final CommonI18NService commonI18NService)
	{
		this.commonI18NService = commonI18NService;
	}

	/**
	 * @return the deliveryService
	 */
	public DeliveryService getDeliveryService()
	{
		return deliveryService;
	}

	/**
	 * @param deliveryService
	 *           the deliveryService to set
	 */
	public void setDeliveryService(final DeliveryService deliveryService)
	{
		this.deliveryService = deliveryService;
	}

	/**
	 * @return the checkoutFacade
	 */
	public BlCheckoutFacade getCheckoutFacade()
	{
		return checkoutFacade;
	}

	/**
	 * @param checkoutFacade
	 *           the checkoutFacade to set
	 */
	public void setCheckoutFacade(final BlCheckoutFacade checkoutFacade)
	{
		this.checkoutFacade = checkoutFacade;
	}

	/**
	 * @return the fedExSameDayServiceImpl
	 */
	public BlFedExSameDayService getFedExSameDayServiceImpl()
	{
		return fedExSameDayServiceImpl;
	}

	/**
	 * @param fedExSameDayServiceImpl
	 *           the fedExSameDayServiceImpl to set
	 */
	public void setFedExSameDayServiceImpl(final BlFedExSameDayService fedExSameDayServiceImpl)
	{
		this.fedExSameDayServiceImpl = fedExSameDayServiceImpl;
	}

	/**
	 * @return the calculationService
	 */
	public CalculationService getCalculationService()
	{
		return calculationService;
	}

	/**
	 * @param calculationService
	 *           the calculationService to set
	 */
	public void setCalculationService(final CalculationService calculationService)
	{
		this.calculationService = calculationService;
	}

	/**
	 * @return the regionConverter
	 */
	public Converter<RegionModel, RegionData> getRegionConverter()
	{
		return regionConverter;
	}

	/**
	 * @param regionConverter
	 *           the regionConverter to set
	 */
	public void setRegionConverter(final Converter<RegionModel, RegionData> regionConverter)
	{
		this.regionConverter = regionConverter;
	}

}

