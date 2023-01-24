/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.catalog.model.CatalogUnawareMediaModel;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.util.stream.Collectors;

import com.bl.core.model.InHouseRepairLogModel;
import com.bl.core.model.NotesModel;
import com.bl.facades.inHouseRepairLog.data.InHouseRepairLogData;


/**
 * @author ravi
 *
 */
public class BlDomoInHouseRepairLogPopulator implements Populator<InHouseRepairLogModel, InHouseRepairLogData>
{

	@Override
	public void populate(final InHouseRepairLogModel source, final InHouseRepairLogData target) throws ConversionException
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
	}

}
