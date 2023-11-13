/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlProductModel;
import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;
import com.bl.logging.BlLogger;


public class BlPackagingInfoPopulator implements Populator<PackagingInfoModel, PackagingInfoData>
{
	private static final Logger LOG = Logger.getLogger(BlPackagingInfoPopulator.class);
	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final PackagingInfoModel source, final PackagingInfoData target) throws ConversionException
	{
		try
		{
			target.setDimension(source.getDimensionUnit());
			target.setDimensionUnit(source.getDimensionUnit());
			target.setHeight(source.getHeight());
			target.setInBoundGraphicImage(source.getInBoundGraphicImage());
			target.setInBoundShippingLabel(source.getInBoundShippingLabel());
			target.setInsuredValue(source.getInsuredValue());
			target.setOutBoundGraphicImage(source.getOutBoundGraphicImage());
			target.setOutBoundShippingLabel(source.getOutBoundShippingLabel());
			target.setDelayeddate(source.getDelayedDate());
			target.setCreatedTS(source.getCreationtime());
			target.setModifiedTS(source.getModifiedtime());
			target.setWidth(source.getWidth());
			target.setLength(source.getLength());
			target.setGrossWeight(source.getGrossWeight());
			target.setDimensionUnit(source.getDimensionUnit());
			target.setWeightUnit(source.getWeightUnit());
			target.setInsuredValue(source.getInsuredValue());
			if (source.getConsignment() != null)
			{
				target.setConsignmentpos(source.getConsignment().getCode());
			}
			target.setLabelURL(source.getLabelURL());
			target.setTotalShippingPrice(source.getTotalShippingPrice());
			target.setShipmentIdentificationNumber(source.getShipmentIdentificationNumber());
			target.setOutBoundTrackingNumber(source.getOutBoundTrackingNumber());
			target.setInBoundTrackingNumber(source.getInBoundTrackingNumber());
			//target.setGraphicimage(source.getG);
			target.setHTMLImage(source.getHTMLImage());
			target.setSerialProducts(
					source.getSerialProducts().stream().map(BlProductModel::getCode).collect(Collectors.joining(", ")));
			if (source.getPackagingInfoStatus() != null)
			{
				target.setPackagingInfoStatus(source.getPackagingInfoStatus().getCode());
			}
			target.setLatePackageDate(source.getLatePackageDate());
			target.setNumberOfRepetitions(source.getNumberOfRepetitions());
			//target.setPackageReturnedToWarehouse(source.getPac);
			target.setIsScrapeScanCompleted(source.isIsScrapeScanCompleted());
			target.setDelayedDate(source.getDelayedDate());
			target.setReturningdate(source.getReturningDate());
			target.setOrdercode(source.getOrderCode());
			target.setPackageId(source.getPackageId());
			if (source.getPackageType() != null)
			{
				target.setPackagetype(source.getPackageType().getCode());
			}
			target.setSubpartName(source.getSubpartName());
			if (source.getInBoundShippingMedia() != null)
			{
				target.setInBoundShippingMedia(source.getInBoundShippingMedia().getDownloadurl());
			}
			if (source.getOutBoundShippingMedia() != null)
			{
				target.setOutBoundShippingMedia(source.getOutBoundShippingMedia().getDownloadURL());
			}
			target.setPrimaryKey(source.getPk().toString());
		}
		catch (final Exception exception)
		{
			getDefaultBlDomoFailureNotificationService().send(exception.toString(), source.getPk().toString(), "PackagingInfo api");
			LOG.info("Error while getting PackagingInfo for PK " + source.getPk().toString());
			BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting PackagingInfo", exception);
			exception.printStackTrace();

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
