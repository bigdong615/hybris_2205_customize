package com.bl.facades.populators;

import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commerceservices.search.resultdata.SearchResultValueData;
import de.hybris.platform.converters.Populator;

import java.math.BigDecimal;
import java.util.Date;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.logging.BlLogger;


public class BlSearchResultDomoProductPopulator implements Populator<SearchResultValueData, ProductData>
{

	private static final Logger LOG = Logger.getLogger(BlSearchResultDomoProductPopulator.class);

	@Override
	public void populate(final SearchResultValueData source, final ProductData target)
	{
		populateAdditionalAttributesForDomo(source, target);
	}

	private void populateAdditionalAttributesForDomo(final SearchResultValueData source, final ProductData target)
	{
		try
		{
			target.setCreatedTS(this.<Date> getValue(source, "creationtime"));
			target.setModifiedTS(this.<Date> getValue(source, "modifiedtime"));
			target.setOnlineDate(this.<Date> getValue(source, "onlineDate"));
			target.setOfflineDate(this.<Date> getValue(source, "offlineDate"));
			target.setCreatedDate(this.<Date> getValue(source, "createdDate"));
			target.setDateFirstActive(this.<Date> getValue(source, "dateFirstActive"));
			target.setInvoiceDate(this.<Date> getValue(source, "invoiceDate"));
			target.setDateOfSale(this.<Date> getValue(source, "dateOfSale"));
			target.setLastUnboxedOcLocationDate(this.<Date> getValue(source, "lastUnboxedOcLocationDate"));
			target.setSupplierAlternativeAID(this.<String> getValue(source, "supplierAlternativeAID"));
			target.setErpGroupBuyer(this.<String> getValue(source, "erpGroupBuyer"));
			target.setErpGroupSupplier(this.<String> getValue(source, "erpGroupSupplier"));
			if (this.<Double> getValue(source, "deliveryTime") != null)
			{
				target.setDeliveryTime(this.<Double> getValue(source, "deliveryTime"));
			}
			if (this.<Double> getValue(source, "priceQuantity") != null)
			{
				target.setPriceQuantity(this.<Double> getValue(source, "priceQuantity"));
			}
			if (this.<Double> getValue(source, "bufferedInventoryPercentage") != null)
			{
				target.setBufferedInventoryPercentage(this.<Double> getValue(source, "bufferedInventoryPercentage"));
			}
			if (this.<Double> getValue(source, "numberContentUnits") != null)
			{
				target.setNumberContentUnits(this.<Double> getValue(source, "numberContentUnits"));
			}
			if (this.<Double> getValue(source, "reviewRating") != null)
			{
				target.setReviewRating(this.<Double> getValue(source, "reviewRating"));
			}
			if (this.<Double> getValue(source, "retailPrice") != null)
			{
				target.setRetailPrice(this.<Double> getValue(source, "retailPrice"));
			}
			if (this.<Long> getValue(source, "sequenceId") != null)
			{
				target.setSequenceId(this.<Long> getValue(source, "sequenceId"));
			}
			if (this.<Long> getValue(source, "noDaysRented") != null)
			{
				target.setNoDaysRented(this.<Long> getValue(source, "noDaysRented"));
			}
			if (this.<Long> getValue(source, "noDaysNotInService") != null)
			{
				target.setNoDaysNotInService(this.<Long> getValue(source, "noDaysNotInService"));
			}
			if (this.<Long> getValue(source, "noDaysRented") != null)
			{
				target.setNoDaysRented(this.<Long> getValue(source, "noDaysRented"));
			}
			target.setMinOrderQuantity(this.<Integer> getValue(source, "minOrderQuantity"));
			target.setMaxOrderQuantity(this.<Integer> getValue(source, "maxOrderQuantity"));
			target.setOrderQuantityInterval(this.<Integer> getValue(source, "orderQuantityInterval"));
			target.setStartLineNumber(this.<Integer> getValue(source, "startLineNumber"));
			target.setEndLineNumber(this.<Integer> getValue(source, "endLineNumber"));
			target.setReviewCount(this.<Integer> getValue(source, "reviewCount"));
			target.setForSaleDiscount(this.<Integer> getValue(source, "forSaleDiscount"));
			if (this.<String> getValue(source, "forSaleBasePrice") != null)
			{
				target.setForSaleBasePrice(new BigDecimal(this.<String> getValue(source, "forSaleBasePrice")));
			}
			if (this.<String> getValue(source, "forRetailGearPrice") != null)
			{
				target.setForRetailGearPrice(new BigDecimal(this.<String> getValue(source, "forRetailGearPrice")));
			}
			if (this.<String> getValue(source, "forFinalSalePrice") != null)
			{
				target.setForFinalSalePrice(new BigDecimal(this.<String> getValue(source, "finalSalePrice")));
			}
			if (this.<String> getValue(source, "incentivizedPrice") != null)
			{
				target.setIncentivizedPrice(new BigDecimal(this.<String> getValue(source, "incentivizedPrice")));
			}
			if (this.<String> getValue(source, "procurementCost") != null)
			{
				target.setProcurementCost(new BigDecimal(this.<String> getValue(source, "procurementCost")));
			}
			if (this.<String> getValue(source, "weight") != null)
			{
				target.setWeight(new BigDecimal(this.<String> getValue(source, "weight")));
			}
			target.setHeight(this.<Integer> getValue(source, "height"));
			target.setWidth(this.<Integer> getValue(source, "width"));
			target.setLength(this.<Integer> getValue(source, "length"));
			target.setReserveRatio(this.<Integer> getValue(source, "reserveRatio"));
			if (this.<Boolean> getValue(source, "isDiscontinued") != null)
			{
				target.setDiscontinued(this.<Boolean> getValue(source, "isDiscontinued"));
			}
			if (this.<Boolean> getValue(source, "discounted") != null)
			{
				target.setDiscounted(this.<Boolean> getValue(source, "discounted"));
			}
			if (this.<Boolean> getValue(source, "visibleOnStoreFront") != null)
			{
				target.setVisibleOnStoreFront(this.<Boolean> getValue(source, "visibleOnStoreFront"));
			}
			if (this.<Boolean> getValue(source, "isSealed") != null)
			{
				target.setIsSealed(this.<Boolean> getValue(source, "isSealed"));
			}
			if (this.<Boolean> getValue(source, "level1Required") != null)
			{
				target.setLevel1Required(this.<Boolean> getValue(source, "level1Required"));
			}
			if (this.<Boolean> getValue(source, "level2Required") != null)
			{
				target.setLevel2Required(this.<Boolean> getValue(source, "level2Required"));
			}
			if (this.<Boolean> getValue(source, "mostPopular") != null)
			{
				target.setMostPopular(this.<Boolean> getValue(source, "mostPopular"));
			}
			if (this.<Boolean> getValue(source, "scheduled") != null)
			{
				target.setScheduled(this.<Boolean> getValue(source, "scheduled"));
			}
			if (this.<Boolean> getValue(source, "staffPick") != null)
			{
				target.setStaffPick(this.<Boolean> getValue(source, "staffPick"));
			}
			if (this.<Boolean> getValue(source, "bufferInvPercChangedManually") != null)
			{
				target.setBufferInvPercChangedManually(this.<Boolean> getValue(source, "bufferInvPercChangedManually"));
			}
			if (this.<Boolean> getValue(source, "retailGearInStock") != null)
			{
				target.setRetailGearInStock(this.<Boolean> getValue(source, "retailGearInStock"));
			}
			if (this.<Boolean> getValue(source, "bundleProduct") != null)
			{
				target.setBundleProduct(this.<Boolean> getValue(source, "bundleProduct"));
			}
			if (this.<Boolean> getValue(source, "constrained") != null)
			{
				target.setConstrained(this.<Boolean> getValue(source, "constrained"));
			}
			if (this.<Boolean> getValue(source, "forRent") != null)
			{
				target.setForRent(this.<Boolean> getValue(source, "forRent"));
			}
			if (this.<Boolean> getValue(source, "forSale") != null)
			{
				target.setForSale(this.<Boolean> getValue(source, "forSale"));
			}
			if (this.<Boolean> getValue(source, "greatValue") != null)
			{
				target.setGreatValue(this.<Boolean> getValue(source, "greatValue"));
			}
			if (this.<Boolean> getValue(source, "isAccounting") != null)
			{
				target.setIsAccounting(this.<Boolean> getValue(source, "isAccounting"));
			}
			if (this.<Boolean> getValue(source, "isNew") != null)
			{
				target.setIsNew(this.<Boolean> getValue(source, "isNew"));
			}
			if (this.<Boolean> getValue(source, "isVideo") != null)
			{
				target.setIsVideo(this.<Boolean> getValue(source, "isVideo"));
			}
			if (this.<Boolean> getValue(source, "hardAssigned") != null)
			{
				target.setHardAssigned(this.<Boolean> getValue(source, "hardAssigned"));
			}
			if (this.<Boolean> getValue(source, "isBufferedInventory") != null)
			{
				target.setIsBufferedInventory(this.<Boolean> getValue(source, "isBufferedInventory"));
			}
			if (this.<Boolean> getValue(source, "isSyncRequired") != null)
			{
				target.setIsSyncRequired(this.<Boolean> getValue(source, "isSyncRequired"));
			}
			if (this.<Boolean> getValue(source, "softAssigned") != null)
			{
				target.setSoftAssigned(this.<Boolean> getValue(source, "softAssigned"));
			}
			if (this.<Boolean> getValue(source, "dirtyPriorityStatus") != null)
			{
				target.setDirtyPriorityStatus(this.<Boolean> getValue(source, "dirtyPriorityStatus"));
			}
			if (this.<Boolean> getValue(source, "gearRated") != null)
			{
				target.setGearRated(this.<Boolean> getValue(source, "gearRated"));
			}
			if (this.<Boolean> getValue(source, "soldIndividually") != null)
			{
				target.setSoldIndividually(this.<Boolean> getValue(source, "soldIndividually"));
			}
			if (this.<Boolean> getValue(source, "softAssigned") != null)
			{
				target.setSoftAssigned(this.<Boolean> getValue(source, "softAssigned"));
			}
			target.setAssetNumber(this.<String> getValue(source, "assetNumber"));
			target.setAssetstatus(this.<String> getValue(source, "assetstatus"));
			target.setBarcode(this.<String> getValue(source, "barcode"));
			target.setConditions(this.<String> getValue(source, "conditions"));
			target.setInventoryLocationID(this.<String> getValue(source, "inventoryLocationID"));
			target.setInvoiceNumber(this.<String> getValue(source, "invoiceNumber"));
			target.setInvoiceNumberRecord(this.<String> getValue(source, "invoiceNumberRecord"));
			target.setLastLocationScanParent(this.<String> getValue(source, "lastLocationScanParent"));
			target.setNotes(this.<String> getValue(source, "notes"));
			target.setOcLocation(this.<String> getValue(source, "ocLocation"));
			target.setOrderNumberRecord(this.<String> getValue(source, "orderNumberRecord"));
			target.setOtherRepairsReason(this.<String> getValue(source, "otherRepairsReason"));
			target.setOwnedBy(this.<String> getValue(source, "ownedBy"));
			target.setSerialNumber(this.<String> getValue(source, "serialNumber"));
			target.setSkuFirmwareVersion(this.<String> getValue(source, "skuFirmwareVersion"));
			target.setUserChangedConditionRating(this.<String> getValue(source, "userChangedConditionRating"));
			target.setWithOrderID(this.<String> getValue(source, "withOrderID"));
			target.setAlternativeProduct(this.<String> getValue(source, "alternativeProduct"));
			target.setDisplayNotes(this.<String> getValue(source, "displayNotes"));
			target.setFirmwareVersion(this.<String> getValue(source, "firmwareVersion"));
			target.setLrLink(this.<String> getValue(source, "lrLink"));
			target.setMpn(this.<String> getValue(source, "mpn"));
			target.setPrimaryCategoryID(this.<String> getValue(source, "primaryCategoryID"));
			target.setProcurementID(this.<String> getValue(source, "procurementID"));
			target.setProcurementSource(this.<String> getValue(source, "procurementSource"));
			target.setProductId(this.<String> getValue(source, "productId"));
			target.setProductStatus(this.<String> getValue(source, "productStatus"));
			target.setPurchaseNotes(this.<String> getValue(source, "purchaseNotes"));
			target.setRentalIncludes(this.<String> getValue(source, "rentalIncludes"));
			target.setShortDescription(this.<String> getValue(source, "shortDescription"));
			target.setSpecifications(this.<String> getValue(source, "specifications"));
			target.setTechEngNotes(this.<String> getValue(source, "techEngNotes"));
			target.setTitleTag(this.<String> getValue(source, "titleTag"));
			target.setUpc(this.<String> getValue(source, "upc"));
			target.setUsedDescription(this.<String> getValue(source, "usedDescription"));
			target.setUsedIncludes(this.<String> getValue(source, "usedIncludes"));
			target.setArticleStatus(this.<String> getValue(source, "articleStatus"));
			target.setEan(this.<String> getValue(source, "ean"));
			target.setManufacturerAID(this.<String> getValue(source, "manufacturerAID"));
			target.setManufacturerTypeDescription(this.<String> getValue(source, "manufacturerTypeDescription"));
			target.setRemarks(this.<String> getValue(source, "remarks"));
			target.setSegment(this.<String> getValue(source, "segment"));
			target.setSpecialTreatmentClasses(this.<String> getValue(source, "specialTreatmentClasses"));
			target.setXmlcontent(this.<String> getValue(source, "xmlcontent"));
			target.setManufacturerName(this.<String> getValue(source, "manufacturerName"));
			target.setVariantType(this.<String> getValue(source, "variantType"));
			target.setTestingStatus(this.<String> getValue(source, "testingStatus"));
			target.setRepairLogType(this.<String> getValue(source, "repairLogType"));
			target.setRepairReasons(this.<String> getValue(source, "repairReasons"));
			target.setCosmeticRatings(this.<String> getValue(source, "cosmeticRating"));
			target.setFunctionalRatings(this.<String> getValue(source, "functionalRating"));
			target.setApprovalStatus(this.<String> getValue(source, "approvalStatus"));
			target.setBatteryMaterial(this.<String> getValue(source, "batteryMaterial"));
			target.setNumberSystem(this.<String> getValue(source, "numberSystem"));
			target.setSerialStatusses(this.<String> getValue(source, "serialStatus"));
			target.setVariantType(this.<String> getValue(source, "variantType"));
			target.setOrder(this.<Integer> getValue(source, "order"));
			target.setUnit(this.<String> getValue(source, "unit"));
			target.setCatalogVersion(this.<String> getValue(source, "catalogVersion"));
			target.setContentUnit(this.<String> getValue(source, "contentUnit"));
			target.setProductOrderLimit(this.<String> getValue(source, "productOrderLimit"));
			target.setRentalVideosLinks(this.<String> getValue(source, "rentalVideosLinks"));
			target.setUsedGearVideosLinks(this.<String> getValue(source, "usedGearVideosLinks"));
			target.setTrackingNumber(this.<String> getValue(source, "trackingNumber"));
			target.setWarehouseLocation(this.<String> getValue(source, "warehouseLocation"));
			target.setSerialHomeLocation(this.<String> getValue(source, "serialHomeLocation"));
			target.setOcLocationDetails(this.<String> getValue(source, "ocLocationDetails"));
			target.setAssociatedConsignment(this.<String> getValue(source, "associatedConsignment"));
			target.setAssociatedOrder(this.<String> getValue(source, "associatedOrder"));
			target.setAssociatedShippedConsignment(this.<String> getValue(source, "associatedShippedConsignment"));
			target.setConsignmentEntry(this.<String> getValue(source, "consignmentEntry"));
			target.setLastUnboxedOcLocationHistory(this.<String> getValue(source, "lastUnboxedOcLocationHistory"));
			target.setBlProduct(this.<String> getValue(source, "blProduct"));
			target.setAssociatedUsedGearConsignment(this.<String> getValue(source, "associatedUsedGearConsignment"));
			target.setAssociatedUsedGearOrder(this.<String> getValue(source, "associatedUsedGearOrder"));
			target.setEurope1PriceFactory_PDG(this.<String> getValue(source, "europe1PriceFactory_PDG"));
			target.setEurope1PriceFactory_PPG(this.<String> getValue(source, "europe1PriceFactory_PPG"));
			target.setEurope1PriceFactory_PTG(this.<String> getValue(source, "europe1PriceFactory_PTG"));
			target.setPrimaryKey(this.<Long> getValue(source, "pk").toString());
		}
		catch (final Exception e)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Error while creating Additional Attributes For Domo", e.getMessage());
			e.printStackTrace();
		}
	}

	protected <T> T getValue(final SearchResultValueData source, final String propertyName)
	{
		if (source.getValues() == null)
		{
			return null;
		}
		return (T) source.getValues().get(propertyName);
	}

}
