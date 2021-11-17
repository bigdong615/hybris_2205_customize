package com.bl.core.serial.model.populator;

import de.hybris.platform.converters.Populator;

import java.util.Objects;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;


/**
 * This Populator is used to populate online serial data to Staged serial if any changes is done in Online version of
 * serial
 *
 * @author Ravikumar
 *
 */
public class BlSerialOnlineToStagedDataPopulator implements Populator<BlSerialProductModel, BlSerialProductModel>
{
	private static final Logger LOG = Logger.getLogger(BlSerialOnlineToStagedDataPopulator.class);

	@Override
	public void populate(final BlSerialProductModel onlineSerial, final BlSerialProductModel stagedSerial)
	{
		if (Objects.nonNull(onlineSerial) && Objects.nonNull(stagedSerial))
		{
			stagedSerial.setSerialStatus(onlineSerial.getSerialStatus());
			stagedSerial.setGearRated(onlineSerial.isGearRated());
			stagedSerial.setRepairReasons(onlineSerial.getRepairReasons());
			stagedSerial.setOtherRepairReasons(onlineSerial.getOtherRepairReasons());
			stagedSerial.setFirmwareVersion(onlineSerial.getFirmwareVersion());
			stagedSerial.setCosmeticRating(onlineSerial.getCosmeticRating());
			stagedSerial.setFunctionalRating(onlineSerial.getFunctionalRating());
			stagedSerial.setAssociatedConsignment(onlineSerial.getAssociatedConsignment());
			stagedSerial.setConsignmentEntry(onlineSerial.getConsignmentEntry());
			stagedSerial.setAssociatedOrder(onlineSerial.getAssociatedOrder());
			stagedSerial.setTestingStatus(onlineSerial.getTestingStatus());
			stagedSerial.setDirtyPriorityStatus(onlineSerial.isDirtyPriorityStatus());
			stagedSerial.setHardAssigned(onlineSerial.getHardAssigned());
			stagedSerial.setSoftAssigned(onlineSerial.getSoftAssigned());
			stagedSerial.setLastLocationScanParent(onlineSerial.getLastLocationScanParent());
			stagedSerial.setLastUnboxedOcLocationDate(onlineSerial.getLastUnboxedOcLocationDate());
			stagedSerial.setLastUnboxedOcLocationHistory(onlineSerial.getLastUnboxedOcLocationHistory());
			stagedSerial.setNoDaysInTransit(onlineSerial.getNoDaysInTransit());
			stagedSerial.setNoDaysNotInService(onlineSerial.getNoDaysNotInService());
			stagedSerial.setNoDaysRented(onlineSerial.getNoDaysRented());
			stagedSerial.setOcLocation(onlineSerial.getOcLocation());
			stagedSerial.setUserChangedConditionRating(onlineSerial.getUserChangedConditionRating());
			stagedSerial.setIsBufferedInventory(onlineSerial.getIsBufferedInventory());

			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"Populated Staged Serial Data with code : {} and pk : {} with Online Serial Data with code : {} and pk : {}",
					stagedSerial.getCode(), stagedSerial.getPk().toString(), onlineSerial.getCode(), onlineSerial.getPk().toString());
		}
	}

}
