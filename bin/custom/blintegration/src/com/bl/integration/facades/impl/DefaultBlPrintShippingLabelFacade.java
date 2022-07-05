/**
 *
 */
package com.bl.integration.facades.impl;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.integration.dao.impl.DefaultBlPrintingShippingLabelDao;
import com.bl.integration.facades.BlPrintShippingLabelFacade;
import com.bl.logging.BlLogger;


/**
 * @author Aditi
 *
 */
public class DefaultBlPrintShippingLabelFacade implements BlPrintShippingLabelFacade
{
	private static final Logger LOG = Logger.getLogger(DefaultBlPrintShippingLabelFacade.class);
	private DefaultBlPrintingShippingLabelDao blPrintingShippingLabelDao;
	private Converter<PackagingInfoModel, PackagingInfoData> defaultBlPrintShippingLabelConverter;

	@Override
	public List<PackagingInfoData> getConsignmentByPk(final String code)
	{
		final ConsignmentModel consignmentByPk = getBlPrintingShippingLabelDao().getConsignmentByPk(code);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Printing started for consignment {}:", consignmentByPk.getCode());
		final List<PackagingInfoModel> packagingInfoModel = consignmentByPk.getPackaginginfos();
		final List<PackagingInfoData> packagingInfoData = new ArrayList<>();
		for (final PackagingInfoModel packageInfo : packagingInfoModel)
		{
			final PackagingInfoData packagingInfo = getDefaultBlPrintShippingLabelConverter().convert(packageInfo);
			packagingInfoData.add(packagingInfo);
		}
		return packagingInfoData;
	}

	/**
	 * @return the blPrintingShippingLabelDao
	 */
	public DefaultBlPrintingShippingLabelDao getBlPrintingShippingLabelDao()
	{
		return blPrintingShippingLabelDao;
	}

	/**
	 * @param blPrintingShippingLabelDao
	 *           the blPrintingShippingLabelDao to set
	 */
	public void setBlPrintingShippingLabelDao(final DefaultBlPrintingShippingLabelDao blPrintingShippingLabelDao)
	{
		this.blPrintingShippingLabelDao = blPrintingShippingLabelDao;
	}

	/**
	 * @return the defaultBlPrintShippingLabelConverter
	 */
	public Converter<PackagingInfoModel, PackagingInfoData> getDefaultBlPrintShippingLabelConverter()
	{
		return defaultBlPrintShippingLabelConverter;
	}

	/**
	 * @param defaultBlPrintShippingLabelConverter
	 *           the defaultBlPrintShippingLabelConverter to set
	 */
	public void setDefaultBlPrintShippingLabelConverter(
			final Converter<PackagingInfoModel, PackagingInfoData> defaultBlPrintShippingLabelConverter)
	{
		this.defaultBlPrintShippingLabelConverter = defaultBlPrintShippingLabelConverter;
	}

}
