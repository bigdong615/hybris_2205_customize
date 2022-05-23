/**
 *
 */
package com.bl.facades.warehouse.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import com.bl.core.model.BoxSizesModel;
import com.bl.core.services.box.BoxDimensionService;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.warehouse.BLWarehousingConsignmentFacade;
import com.bl.logging.BlLogger;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;
import de.hybris.platform.warehousingfacades.order.impl.DefaultWarehousingConsignmentFacade;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


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
		List<BoxSizesModel> modifiableDimensions = new ArrayList<>(dimensions);
		Comparator<BoxSizesModel> lengthComparator = (dimensions1,dimensions2) -> Integer.valueOf(dimensions1.getLength())
				.compareTo(Integer.valueOf(dimensions2.getLength()));
		Comparator<BoxSizesModel> widthComparator = (dimensions1,dimensions2) -> Integer.valueOf(dimensions1.getWidth())
				.compareTo(Integer.valueOf(dimensions2.getWidth()));
		Comparator<BoxSizesModel> heightComparator = (dimensions1,dimensions2) -> Integer.valueOf(dimensions1.getHeight())
				.compareTo(Integer.valueOf(dimensions2.getHeight()));
		List<BoxSizesModel> sortedBoxSizes = modifiableDimensions
				.stream()
				.sorted(
						lengthComparator
								.thenComparing(widthComparator)
								.thenComparing(heightComparator)).collect(Collectors.toList());

		final List<PackagingInfoData> packagingInfoDataList = new ArrayList<>();
		if (CollectionUtils.isNotEmpty(sortedBoxSizes))
		{
			for (final BoxSizesModel boxDimension : sortedBoxSizes)
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
				putDefaultSizesOnTop(packagingInfoDataList, packagingInfoData);
			}
		}
		PackagingInfoData packagingInfoData = packagingInfoDataList.get(BlFacadesConstants.INT_SEVEN);
		packagingInfoDataList.remove(BlFacadesConstants.INT_SEVEN);
		packagingInfoDataList.add(BlFacadesConstants.FOUR,packagingInfoData);
		return packagingInfoDataList;
	}

	/**
	 * It puts the most used sizes on top of the list
	 * @param packagingInfoDataList the packagingInfoDataList
	 * @param packagingInfoData the packagingInfoData
	 */
	private void putDefaultSizesOnTop(final List<PackagingInfoData> packagingInfoDataList,
			final PackagingInfoData packagingInfoData) {
		if (packagingInfoData.getLength().equals(BlFacadesConstants.TWELVE) && packagingInfoData
				.getWidth().equals(BlFacadesConstants.EIGHT) && packagingInfoData.getHeight().equals(BlFacadesConstants.SIX)) {
			packagingInfoDataList.add(BlFacadesConstants.ZERO, packagingInfoData);
		} else if (packagingInfoData.getLength().equals(BlFacadesConstants.TWELVE) && packagingInfoData
				.getWidth().equals(BlFacadesConstants.TWELVE) && packagingInfoData.getHeight().equals(BlFacadesConstants.SEVEN)) {
			packagingInfoDataList.add(BlFacadesConstants.ONE, packagingInfoData);
		} else if (packagingInfoData.getLength().equals(BlFacadesConstants.TWELVE) && packagingInfoData
				.getWidth().equals(BlFacadesConstants.TWELVE) && packagingInfoData.getHeight().equals(BlFacadesConstants.TWELVE)) {
			packagingInfoDataList.add(BlFacadesConstants.TWO, packagingInfoData);
		} else {
			sortRemainingList(packagingInfoDataList, packagingInfoData);
		}
	}

	/**
	 * It puts the most used sizes on top of the list
	 * @param packagingInfoDataList the packagingInfoDataList
	 * @param packagingInfoData the packagingInfoData
	 */
	private void sortRemainingList(final List<PackagingInfoData> packagingInfoDataList, final PackagingInfoData packagingInfoData) {
		if (packagingInfoData.getLength().equals(BlFacadesConstants.TWENTY_FOUR) && packagingInfoData
				.getWidth().equals(BlFacadesConstants.TWENTY_FOUR) && packagingInfoData.getHeight().equals(BlFacadesConstants.SIXTEEN)) {
			packagingInfoDataList.add(BlFacadesConstants.THREE, packagingInfoData);
		} else if (packagingInfoData.getLength().equals(BlFacadesConstants.EIGHT) && packagingInfoData
				.getWidth().equals(BlFacadesConstants.EIGHT) && packagingInfoData.getHeight().equals(BlFacadesConstants.FORTY_EIGHT)) {
			packagingInfoDataList.add(BlFacadesConstants.THREE, packagingInfoData);
		} else if (packagingInfoData.getLength().equals(BlFacadesConstants.SIX) && packagingInfoData
				.getWidth().equals(BlFacadesConstants.SIX) && packagingInfoData.getHeight().equals(BlFacadesConstants.SIX)) {
			packagingInfoDataList.add(BlFacadesConstants.ZERO, packagingInfoData);
		} else {
			sortTheRest(packagingInfoDataList, packagingInfoData);
		}
	}

	/**
	 * It puts the most used sizes on top of the list
	 * @param packagingInfoDataList the packagingInfoDataList
	 * @param packagingInfoData the packagingInfoData
	 */
	private void sortTheRest(List<PackagingInfoData> packagingInfoDataList, PackagingInfoData packagingInfoData) {
		if (packagingInfoData.getLength().equals(BlFacadesConstants.SIX) && packagingInfoData
				.getWidth().equals(BlFacadesConstants.SIX) && packagingInfoData.getHeight().equals(BlFacadesConstants.TWENTY_FOUR)) {
			packagingInfoDataList.add(BlFacadesConstants.ONE, packagingInfoData);
		} else if (packagingInfoData.getLength().equals(BlFacadesConstants.EIGHT) && packagingInfoData
				.getWidth().equals(BlFacadesConstants.EIGHT) && packagingInfoData.getHeight().equals(BlFacadesConstants.THIRTY_SIX)) {
			packagingInfoDataList.add(BlFacadesConstants.TWO, packagingInfoData);
		} else {
			packagingInfoDataList.add(packagingInfoData);
		}
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
