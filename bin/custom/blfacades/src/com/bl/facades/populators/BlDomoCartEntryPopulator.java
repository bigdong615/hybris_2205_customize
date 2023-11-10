/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.commercefacades.cart.data.CartEntryData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlOptionsModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;
import com.bl.logging.BlLogger;


/**
 * @author kumar
 *
 */
public class BlDomoCartEntryPopulator implements Populator<CartEntryModel, CartEntryData>
{
	private static final Logger LOG = Logger.getLogger(BlDomoCartEntryPopulator.class);
	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final CartEntryModel source, final CartEntryData target) throws ConversionException
	{
		try
		{
			target.setAvalaralinetax(source.getAvalaraLineTax());
			target.setUpdatedTime(source.getUpdatedTime());
			target.setSplitConsignmentQuantity(source.getSplitConsignmentQuantity());
			target.setBundleMainEntry(source.isBundleMainEntry());
			target.setBundleProductCode(source.getBundleProductCode());
			target.setBundleEntry(source.isBundleEntry());
			target.setEntryCreated(source.isEntryCreated());
			target.setUnAllocatedQuantity(source.getUnAllocatedQuantity());
			target.setIsModifiedOrder(source.isIsModifiedOrder());
			target.setModifiedSerialProductList(source.getModifiedSerialProductList().stream().map(BlSerialProductModel::getCode)
					.collect(Collectors.joining(",")));
			if (source.getWarehouse() != null)
			{
				target.setWarehouse(source.getWarehouse().getCode());
			}
			target.setRecipientEmail(source.getRecipientEmail());
			target.setRecipientName(source.getRecipientName());
			target.setRecipientMessage(source.getRecipientMessage());
			target.setSerialProducts(
					source.getSerialProducts().stream().map(BlProductModel::getCode).collect(Collectors.joining(", ")));
			target.setRentalReturnDate(source.getRentalReturnDate());
			target.setRentalStartDate(source.getRentalStartDate());
			//target.setEntryNumber(source.getEntryNumber());
			if (source.getEurope1PriceFactory_PPG() != null)
			{
				target.setEurope1pricefactory_pdg(source.getEurope1PriceFactory_PDG().getCode());
			}
			if (source.getEurope1PriceFactory_PPG() != null)
			{
				target.setEurope1pricefactory_ppg(source.getEurope1PriceFactory_PPG().getCode());
			}
			if (source.getEurope1PriceFactory_PTG() != null)
			{
				target.setEurope1pricefactory_ptg(source.getEurope1PriceFactory_PTG().getCode());
			}
			if (source.getChosenVendor() != null)
			{
				target.setChosenvendor(source.getChosenVendor().getCode());
			}
			if (source.getDeliveryMode() != null)
			{
				target.setDeliveryModeCode(source.getDeliveryMode().getCode());
			}
			if (source.getDeliveryPointOfService() != null)
			{
				target.setDeliverypointofserviceCode(source.getDeliveryPointOfService().getName());
			}
			target.setDiscountvaluesinternal(source.getDiscountValuesInternal());
			target.setCalculated(source.getCalculated());
			if (source.getOrder() != null)
			{
				target.setOrderCode(source.getOrder().getCode());
			}
			if (source.getUnit() != null)
			{
				target.setUnit(source.getUnit().getCode());
			}
			target.setTaxvaluesinternal(source.getTaxValuesInternal());
			target.setGiveaway(source.getGiveAway());
			target.setRejected(source.getRejected());
			target.setEntryGroupNumberCodes(
					source.getEntryGroupNumbers().stream().map(String::valueOf).collect(Collectors.joining(",")));
			if (source.getQuantityStatus() != null)
			{
				target.setQuantitystatus(source.getQuantityStatus().getCode());
			}
			if (source.getProduct() != null)
			{
				target.setProductCode(source.getProduct().getCode());
			}

			target.setOptions(source.getOptions().stream().map(BlOptionsModel::getCode).collect(Collectors.joining(", ")));
			target.setCreatedTS(source.getCreationtime());
			target.setModifiedTS(source.getModifiedtime());
			target.setInfo(source.getInfo());
			if (source.getDeliveryAddress() != null)
			{
				target.setDeliveryaddress(source.getDeliveryAddress().getAddressID());
			}
			target.setPrimaryKey(source.getPk().toString());
			target.setTotalPrice(source.getTotalPrice());
			if (source.getQuantity() != null)
			{
				target.setQuantity(source.getQuantity().toString());
			}
			target.setEntryNumber(source.getEntryNumber());
			target.setGearGuardProFullWaiverPrice(source.getGearGuardProFullWaiverPrice());
			target.setGearGuardWaiverPrice(source.getGearGuardWaiverPrice());
		}
		catch (final Exception exception)
		{
			BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting cartentry", exception);
			exception.printStackTrace();
			getDefaultBlDomoFailureNotificationService().send(exception.toString(), source.getPk().toString(), "CartEntry api");

		}
	}

	/**
	 * @return the defaultBlDomoFailureNotificationService
	 */
	public DefaultBlDomoFailureNotificationService getDefaultBlDomoFailureNotificationService()
	{
		return defaultBlDomoFailureNotificationService;
	}

	/**
	 * @param defaultBlDomoFailureNotificationService
	 *           the defaultBlDomoFailureNotificationService to set
	 */
	public void setDefaultBlDomoFailureNotificationService(
			final DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService)
	{
		this.defaultBlDomoFailureNotificationService = defaultBlDomoFailureNotificationService;
	}
}
