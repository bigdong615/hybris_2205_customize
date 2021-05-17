//
// Decompiled by Procyon v0.5.36
//

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
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.storelocator.model.PointOfServiceModel;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
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

import com.bl.core.model.BlRushDeliveryModeModel;
import com.bl.facades.fexEx.data.SameDayCityReqData;
import com.bl.facades.fexEx.data.SameDayCityResData;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.integration.services.BlFedExSameDayService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Iterables;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


public class UpdateOrderDetailsController extends DefaultWidgetController
{

	private static final Logger LOG = Logger.getLogger(UpdateOrderDetailsController.class);

	private static final String COUNTRY_CODE = "US";

	private static final int DURATION = 2000;

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
	private Textbox pickupPersonFName;

	@Wire
	private Textbox pickupPersonLName;

	@Wire
	private Textbox pickUpPersonEmail;

	@Wire
	private Textbox pickUpPersonPhone;

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

	ListModelList<RegionData> listModelList = new ListModelList<>();

	ListModelList<DeliveryModeModel> deliveryList = new ListModelList<>();


	static
	{
		try
		{
			context = Registry.getApplicationContext();

			i18NFacade = (I18NFacade) context.getBean("i18NFacade");
		}
		catch (final Exception e)
		{
			LOG.info("Bean entry not found " + e);
		}
	}


	@SocketEvent(socketId = "inputObject")
	public void initCustomerAddressForm(final OrderModel inputObject)
	{
		final List regionCodeList = new ArrayList();
		final List<RegionData> blRegionCode = i18NFacade.getRegionsForCountryIso(COUNTRY_CODE);

		this.setOrderModel(inputObject);
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.updateshipping.confirm.title")) + " "
						+ this.getOrderModel().getCode());
		final AddressModel deliveryAddress = getOrderModel().getDeliveryAddress();
		this.firstName.setValue(deliveryAddress.getFirstname());
		this.lastName.setValue(deliveryAddress.getLastname());
		this.line1.setValue(deliveryAddress.getLine1());
		this.line2.setValue(deliveryAddress.getLine2());
		this.postalCode.setValue(deliveryAddress.getPostalcode());
		this.town.setValue(deliveryAddress.getTown());
		this.contactNo.setValue(deliveryAddress.getCellphone());
		this.countryCode
				.setValue(deliveryAddress.getCountry().getName() + " " + "[" + deliveryAddress.getCountry().getIsocode() + "]");
		this.pickupPersonFName.setValue(getOrderModel().getPickUpPersonFirstName());
		this.pickupPersonLName.setValue(getOrderModel().getPickUpPersonLastName());
		this.pickUpPersonEmail.setValue(getOrderModel().getPickUpPersonEmail());
		this.pickUpPersonPhone.setValue(getOrderModel().getPickUpPersonPhone());

		getBlRegionData(regionCodeList, blRegionCode);

		getBlDeliveryModes();

	}

	/**
	 * This method will set all delivery modes to combo box
	 */
	private void getBlDeliveryModes()
	{
		final Collection<ZoneDeliveryModeModel> deliveyModeList = getCheckoutFacade().getAllDeliveryModes(false);
		deliveryList = new ListModelList<>(deliveyModeList);
		deliveryList.addToSelection(deliveryList.get(0));
		deliveryModeCombobox.setModel(deliveryList);
	}

	/**
	 * @param regionCodeList
	 * @param blRegionCode
	 */
	private void getBlRegionData(final List regionCodeList, final List<RegionData> blRegionCode)
	{
		regionCodeList.addAll(blRegionCode);
		listModelList = new ListModelList<>(regionCodeList);

		listModelList.addToSelection(listModelList.get(0));
		regionCombobox.setModel(listModelList);
	}

	@ViewEvent(componentID = "regionCombobox", eventName = "onChange")
	public void changeCountry()
	{
		final List<RegionData> regionForIso = i18NFacade.getRegionsForCountryIso(COUNTRY_CODE);

		final String regionCode = StringUtils.substringBetween(regionCombobox.getValue(), "[", "]");

		for (final RegionData isoCodeList : regionForIso)
		{
			if (regionCode.trim().equals(isoCodeList.getIsocode()))
			{
				final RegionData regionData = i18NFacade.getRegion(COUNTRY_CODE, regionCode.trim());

				listModelList.addToSelection(regionData);
				regionCombobox.setModel(listModelList);
				showNotify("Changed to: " + regionCode, regionCombobox);
				break;
			}
		}

	}

	@ViewEvent(componentID = "deliveryModeCombobox", eventName = "onChange")
	public void changeDeliveryMode()
	{
		final SameDayCityReqData sameDayCityReqData = new SameDayCityReqData();

		final String deliveryAddressZipCode = getOrderModel().getDeliveryAddress().getPostalcode();
		String warehouseZipCode = "";
		final ZoneDeliveryModeModel blZoneDeliveryMode = this.deliveryModeCombobox.getSelectedItem().getValue();
		if (blZoneDeliveryMode instanceof BlRushDeliveryModeModel)
		{
			final BlRushDeliveryModeModel zonedeliveryMode = (BlRushDeliveryModeModel) blZoneDeliveryMode;

			if ("SAME_DAY_DELIVERY".equals(zonedeliveryMode.getShippingGroup().getCode())
					|| "NEXT_DAY_RUSH_DELIVERY".equals(zonedeliveryMode.getShippingGroup().getCode()))
			{
				PointOfServiceModel pointOfServiceModel = new PointOfServiceModel();
				if (zonedeliveryMode.getWarehouse() != null && zonedeliveryMode.getWarehouse().getPointsOfService() != null)
				{
					pointOfServiceModel = Iterables.get(zonedeliveryMode.getWarehouse().getPointsOfService(), 0);
				}
				warehouseZipCode = pointOfServiceModel.getAddress().getPostalcode();
			}

			sameDayCityReqData.setWarehouseZipCode(warehouseZipCode);
			sameDayCityReqData.setDeliveryAddressZipCode(deliveryAddressZipCode);

			try
			{
				final SameDayCityResData resData = getFedExSameDayServiceImpl().getAvailability(sameDayCityReqData);
				if (BooleanUtils.isTrue(resData.getServiceApplicable()))
				{
					deliveryList.addToSelection(blZoneDeliveryMode);
				}
				else
				{
					deliveryList.addToSelection(getOrderModel().getDeliveryMode());
					Messagebox.show("blbackoffice.updateshipping.inValid.zipCode");
					throw new WrongValueException(this.deliveryModeCombobox, this.getLabel("Error Here"));

				}
			}
			catch (final URISyntaxException ex)
			{
				BlLogger.logMessage(LOG, Level.ERROR, ex.getMessage());
			}

		}
	}

	@ViewEvent(componentID = "undochanges", eventName = "onClick")
	public void reset()
	{
		this.initCustomerAddressForm(this.getOrderModel());
	}

	@ViewEvent(componentID = "confirmAddress", eventName = "onClick")
	public void confirmCancellation()
	{
		this.validateRequest();
		final AddressModel addressModel = getOrderModel().getDeliveryAddress();
		addressModel.setFirstname(this.firstName.getValue());
		addressModel.setLastname(this.lastName.getValue());
		addressModel.setLine1(this.line1.getValue());
		addressModel.setLine2(this.line2.getValue());
		addressModel.setPostalcode(this.postalCode.getValue());
		addressModel.setTown(this.town.getValue());
		addressModel.setCellphone(this.contactNo.getValue());
		final String regionCode = StringUtils.substringBetween(this.regionCombobox.getValue().trim(), "[", "]");
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
		modelService.save(addressModel);
		modelService.refresh(addressModel);
		modelService.save(orderModel);
		modelService.refresh(orderModel);
		this.showMessageBox();

	}

	protected void validateRequest()
	{
		if (StringUtils.isEmpty(this.firstName.getValue()))
		{
			throw new WrongValueException(this.firstName, this.getLabel("blbackoffice.updateshipping.missing.firstname"));
		}

		if (StringUtils.isEmpty(this.line1.getValue()))
		{
			throw new WrongValueException(this.line1, this.getLabel("blbackoffice.updateshipping.missing.line1"));
		}

		if (StringUtils.isEmpty(this.town.getValue()))
		{
			throw new WrongValueException(this.line1, this.getLabel("blbackoffice.updateshipping.missing.town"));
		}

	}

	protected void showMessageBox()
	{
		Messagebox.show("Address Updated Successfully");

	}

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

}

