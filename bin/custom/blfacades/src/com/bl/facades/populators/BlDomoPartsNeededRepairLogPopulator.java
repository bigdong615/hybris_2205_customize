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
import com.bl.core.model.PartsNeededRepairLogModel;
import com.bl.facades.partsNeededRepairLog.data.PartsNeededRepairLogData;
import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;
import com.bl.logging.BlLogger;


/**
 * @author Kumar
 *
 */
public class BlDomoPartsNeededRepairLogPopulator implements Populator<PartsNeededRepairLogModel, PartsNeededRepairLogData>
{

	private static final Logger LOG = Logger.getLogger(BlDomoPartsNeededRepairLogPopulator.class);
	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final PartsNeededRepairLogModel source, final PartsNeededRepairLogData target) throws ConversionException
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
			target.setAssessmentNotes(
					source.getAssessmentNotes().stream().map(NotesModel::getNote).collect(Collectors.joining(",")));
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
			if (source.getPartsStatus() != null)
			{
				target.setPartsStatus(source.getPartsStatus().getCode());
			}
			target.setPartCost(source.getPartCost());
			target.setPrimaryKey(source.getPk().toString());
		}
		catch (final Exception exception)
		{
			getDefaultBlDomoFailureNotificationService().send(exception.toString(), source.getPk().toString(),
					"PartsNeededRepairLog api");
			LOG.error("Error while getting PartsNeededRepairLog for PK " + source.getPk().toString());
			BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting PartsNeededRepairLog", exception);
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
