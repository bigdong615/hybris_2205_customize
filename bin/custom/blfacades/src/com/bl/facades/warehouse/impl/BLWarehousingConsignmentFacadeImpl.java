/**
 *
 */
package com.bl.facades.warehouse.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;
import de.hybris.platform.warehousingfacades.order.impl.DefaultWarehousingConsignmentFacade;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BoxSizesModel;
import com.bl.core.services.box.BoxDimensionService;
import com.bl.facades.warehouse.BLWarehousingConsignmentFacade;
import com.bl.logging.BlLogger;


/**
 * @author Keyur
 *
 */
public class BLWarehousingConsignmentFacadeImpl extends DefaultWarehousingConsignmentFacade
		implements BLWarehousingConsignmentFacade
{
	private static final Logger LOGGER = Logger.getLogger(BLWarehousingConsignmentFacadeImpl.class);

	private BoxDimensionService boxDimensionService;

	@Override
	public PackagingInfoModel createPackagingInformationOnConsignment(final String code, final PackagingInfoData packagingInfoData)
	{
		try
		{
			validateParameterNotNullStandardMessage("code", code);
			validateParameterNotNullStandardMessage("packagingInfoData", packagingInfoData);

			final ConsignmentModel consignmentModel = getConsignmentModelForCode(code);
			final PackagingInfoModel newPackagingInfo = getModelService().create(PackagingInfoModel.class);
			getReversePackagingInfoConverter().convert(packagingInfoData, newPackagingInfo);
			newPackagingInfo.setConsignment(consignmentModel);
			newPackagingInfo.setPackageId(UUID.randomUUID().toString());
			consignmentModel.setPackagingInfo(newPackagingInfo);

			getModelService().save(consignmentModel);
			getModelService().refresh(consignmentModel);
			getModelService().save(newPackagingInfo);
			getModelService().refresh(newPackagingInfo);
			return newPackagingInfo;
		}
		catch (final ModelSavingException e)
		{
			BlLogger.logMessage(LOGGER, Level.ERROR, "Unable to create package", e);
			return null;
		}
	}

	@Override
	public List<PackagingInfoData> getAllPackagingDimensions()
	{
		final List<BoxSizesModel> dimensions = getBoxDimensionService().getBoxDimestions();
		final List<PackagingInfoData> PackagingInfoDataList = new ArrayList<PackagingInfoData>();
		if (CollectionUtils.isNotEmpty(dimensions))
		{
			for (final BoxSizesModel boxDimension : dimensions)
			{
				final PackagingInfoData packagingInfoData = new PackagingInfoData();
				packagingInfoData.setHeight(boxDimension.getHeight());
				packagingInfoData.setWidth(boxDimension.getWidth());
				packagingInfoData.setLength(boxDimension.getLength());
				packagingInfoData.setGrossWeight(boxDimension.getWeight());
				packagingInfoData.setDimensionUnit(boxDimension.getDimensionUnit());
				packagingInfoData.setWeightUnit(boxDimension.getWeightUnit());
				packagingInfoData.setDimension(boxDimension.getLength() + "*" + boxDimension.getWidth() + "*"
						+ boxDimension.getHeight() + "-" + boxDimension.getWeight());
				PackagingInfoDataList.add(packagingInfoData);

			}
		}
		return PackagingInfoDataList;
	}

	/**
	 * @return the boxDimensionService
	 */
	public BoxDimensionService getBoxDimensionService()
	{
		return boxDimensionService;
	}

	/**
	 * @param boxDimensionService
	 *           the boxDimensionService to set
	 */
	public void setBoxDimensionService(final BoxDimensionService boxDimensionService)
	{
		this.boxDimensionService = boxDimensionService;
	}



}
