/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.catalog.model.CatalogUnawareMediaModel;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.NotesModel;
import com.bl.core.model.VendorRepairLogModel;
import com.bl.facades.vendorRepairLog.data.VendorRepairLogData;
import com.bl.logging.BlLogger;


/**
 * @author ravi
 *
 */
public class BlDomoVendorRepairLogPopulator implements Populator<VendorRepairLogModel, VendorRepairLogData>
{
	private static final Logger LOG = Logger.getLogger(BlDomoVendorRepairLogPopulator.class);

	@Override
	public void populate(final VendorRepairLogModel source, final VendorRepairLogData target) throws ConversionException
	{
		try
		{
		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		target.setSerialCode(source.getSerialCode());
		target.setItemBarcode(source.getItemBarcode());
		if (source.getRepairTechsForClaim() != null)
		{
			target.setRepairTechsForClaim(source.getRepairTechsForClaim().getCode());
		}
		if (source.getOrder() != null)
		{
			target.setOrder(source.getOrder().getCode());
		}
		if (source.getSelectedGearGaurd() != null)
		{
			target.setSelectedGearGaurd(source.getSelectedGearGaurd().getCode());
		}
		target.setRecommendToRepair(source.isRecommendToRepair());
		if (source.getCustomerResponsible() != null)
		{
			target.setCustomerResponsible(source.getCustomerResponsible().getCode());
		}
		target.setLastRepairReturnDay(source.getLastRepairReturnDay());
		target.setCostForParts(source.getCostForParts());
		target.setLabourHours(source.getLabourHours());
		target.setLabourRate(source.getLabourRate());
		target.setCostForLabor(source.getCostForLabor());
		target.setTotalForPartsAndLaborCost(source.getTotalForPartsAndLaborCost());
		if (source.getCustomerCharged() != null)
		{
			target.setCustomerCharged(source.getCustomerCharged().getCode());
		}
		if (source.getCustomerCollectionStatus() != null)
		{
			target.setCustomerCollectionStatus(source.getCustomerCollectionStatus().getCode());
		}
		target.setOtherDocument(
				source.getOtherDocument().stream().map(CatalogUnawareMediaModel::getCode).collect(Collectors.joining(",")));
		if (source.getRepairReasons() != null)
		{
			target.setRepairReasons(source.getRepairReasons().getCode());
		}
		target.setOtherRepairsReason(source.getOtherRepairsReason());
		if (source.getFrequency() != null)
		{
			target.setFrequency(source.getFrequency().getCode());
		}
		target.setCause(source.getCause());
		if (source.getBlame() != null)
		{
			target.setBlame(source.getBlame().getCode());
		}
		target.setIssueLocation(source.getIssueLocation());
		if (source.getAssociatedConsignment() != null)
		{
			target.setAssociatedConsignment(source.getAssociatedConsignment().getCode());
		}
		//target.setConsignmentEntry(source.getConsignmentEntry().getConsignmentEntry);
		target.setUserId(source.getUserId());
		target.setLastUserChangedConditionRating(source.getLastUserChangedConditionRating());
		target.setAssessmentNotes(source.getAssessmentNotes().stream().map(NotesModel::getNote).collect(Collectors.joining(",")));
		target.setDamageWaiverPaid(source.getDamageWaiverPaid());
		target.setRepairLogId(source.getRepairLogId());
		if (source.getWarehouse() != null)
		{
			target.setWarehouse(source.getWarehouse().getCode());
		}
		target.setOcLocation(source.getOcLocation());
		if (source.getRepairVendor() != null)
		{
			target.setRepairVendor(source.getRepairVendor().getCode());
		}
		target.setDateSent(source.getDateSent());
		target.setEstimateCostApproved(source.getEstimateCostApproved());
		target.setEstimateCostDeclined(source.getEstimateCostDeclined());
		target.setEstimateNote(source.getEstimateNote());
		target.setDateReturned(source.getDateReturned());
		target.setTrackingNumber(source.getTrackingNumber());
		target.setPrimaryKey(source.getPk().toString());
	}
	catch (final Exception exception)
	{
		LOG.info("Error while getting BlDomoVendorRepairLog for PK " + source.getPk().toString());
		BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting BlDomoVendorRepairLog", exception);
		exception.printStackTrace();

	}

	}

}
