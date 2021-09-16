package com.bl.core.utils;

import java.util.ArrayList;
import java.util.List;

import com.bl.constants.BlInventoryScanLoggingConstants;


/**
 *
 * Utility class for OMS flow
 *
 * @author Ravikumar
 *
 */
public final class BlInventoryScanUtility
{

	private BlInventoryScanUtility()
	{
		//empty to avoid instantiating utils class
	}

	/**
	 * Gets all the tech eng location Initials.
	 *
	 * @return the list of default tech eng location
	 */
	public static List<String> getDefaultTechEngLocation()
	{
		final List<String> defaultLocations = new ArrayList<>();

		defaultLocations.add(BlInventoryScanLoggingConstants.MAW);
		defaultLocations.add(BlInventoryScanLoggingConstants.CAW);
		defaultLocations.add(BlInventoryScanLoggingConstants.MAR);
		defaultLocations.add(BlInventoryScanLoggingConstants.MA);
		defaultLocations.add(BlInventoryScanLoggingConstants.MAM);
		defaultLocations.add(BlInventoryScanLoggingConstants.CAM);
		defaultLocations.add(BlInventoryScanLoggingConstants.CAR);
		defaultLocations.add(BlInventoryScanLoggingConstants.BIN);

		return defaultLocations;
	}

	/**
	 * Gets all the tech eng allowed locations.
	 *
	 * @return the tech eng allowed locations
	 */
	public static List<String> getTechEngAllowedLocations()
	{
		final List<String> defaultLocations = new ArrayList<>();
		defaultLocations.addAll(getTechEngWorkStationLocations());
		defaultLocations.addAll(getTechEngCleanCartLocations());
		defaultLocations.addAll(getTechEngCleanPriorityCartLocations());
		defaultLocations.addAll(getTechEngRepairLocations());

		return defaultLocations;
	}

	/**
	 * Gets the tech eng work station locations.
	 *
	 * @return the tech eng work station locations
	 */
	public static List<String> getTechEngWorkStationLocations()
	{
		final List<String> workStationLocations = new ArrayList<>();
		workStationLocations.add(BlInventoryScanLoggingConstants.TECH_ENGINEERING_WORKSTATION);
		workStationLocations.add(BlInventoryScanLoggingConstants.VIP_WORKSTATION_TECH_ENG);
		return workStationLocations;
	}

	/**
	 * Gets the tech eng clean cart locations.
	 *
	 * @return the tech eng clean cart locations
	 */
	public static List<String> getTechEngCleanCartLocations()
	{
		final List<String> cleanCartLocations = new ArrayList<>();
		cleanCartLocations.add(BlInventoryScanLoggingConstants.CLEAN_FRONT_DESK_CART);
		cleanCartLocations.add(BlInventoryScanLoggingConstants.CLEAN_GEAR_AISLE_IN_CAGE);
		cleanCartLocations.add(BlInventoryScanLoggingConstants.CLEAN_GEAR_CAGE);
		cleanCartLocations.add(BlInventoryScanLoggingConstants.CLEAN_GEAR_MOBILE_CART);
		cleanCartLocations.add(BlInventoryScanLoggingConstants.CLEAN_GEAR_REQUEST_PICKUP_MOBILE_CART);
		cleanCartLocations.add(BlInventoryScanLoggingConstants.CLEAN_GEAR_SHIPPING_MOBILE_CART);
		cleanCartLocations.add(BlInventoryScanLoggingConstants.CLEAN_MOBILE_LAUNDRY_BIN);
		return cleanCartLocations;
	}

	/**
	 * Gets the tech eng clean priority cart locations.
	 *
	 * @return the tech eng clean priority cart locations
	 */
	public static List<String> getTechEngCleanPriorityCartLocations()
	{
		final List<String> cleanPriorityCartLocations = new ArrayList<>();
		cleanPriorityCartLocations.add(BlInventoryScanLoggingConstants.CLEAN_PRIORITY_GEAR_CART);
		cleanPriorityCartLocations.add(BlInventoryScanLoggingConstants.CLEAN_PRIORITY_MOBILE_CART);
		cleanPriorityCartLocations.add(BlInventoryScanLoggingConstants.VIP_CLEAN_PRIORITY_GEAR);
		return cleanPriorityCartLocations;
	}

	/**
	 * Gets the tech eng repair locations.
	 *
	 * @return the tech eng repair locations
	 */
	public static List<String> getTechEngRepairLocations()
	{
		final List<String> repairLocations = new ArrayList<>();
		repairLocations.add(BlInventoryScanLoggingConstants.REPAIR_SHELF);
		repairLocations.add(BlInventoryScanLoggingConstants.REPAIR_SHELF);
		repairLocations.add(BlInventoryScanLoggingConstants.REPAIR_CLEAN_MOBILE_CART);
		repairLocations.add(BlInventoryScanLoggingConstants.REPAIR);
		repairLocations.add(BlInventoryScanLoggingConstants.REPAIR_CABINET);
		repairLocations.add(BlInventoryScanLoggingConstants.REPAIR_MOBILE_SHELF);
		repairLocations.add(BlInventoryScanLoggingConstants.REPAIR_MOBILE_LAUNDRY_BIN);
		return repairLocations;
	}

	/**
	 * Gets the shipping work station locations.
	 *
	 * @return defaultLocations for shipping
	 */
	public static List<String> getShippingWorkstationInitial()
	{
		final List<String> defaultLocations = new ArrayList<>();

		defaultLocations.add(BlInventoryScanLoggingConstants.MAW);
		defaultLocations.add(BlInventoryScanLoggingConstants.CAW);
		defaultLocations.add(BlInventoryScanLoggingConstants.BOXING_FEDEX);
		defaultLocations.add(BlInventoryScanLoggingConstants.UPS);
		return defaultLocations;
	}

	/**
	 * Gets the front desk shelf locations.
	 *
	 * @return defaultLocations for front desk
	 */

	public static List<String> getFrontDeskShelfInitial()
	{
		final List<String> defaultLocations = new ArrayList<>();

		defaultLocations.add(BlInventoryScanLoggingConstants.FD_MOBILE_SHELF);
		return defaultLocations;
	}

	/**
	 * This method will return list of location that will be applicable for DirtyCart and DirtyPriorityCart location
	 *
	 * @return List<String> for default locations for DC and DPC
	 */
	public static List<String> getDefaultInventoryLocationForDPCAndDC()
	{
		final List<String> defaultLocations = new ArrayList<>();
		defaultLocations.add(BlInventoryScanLoggingConstants.BIN);
		defaultLocations.add(BlInventoryScanLoggingConstants.CAM);
		defaultLocations.add(BlInventoryScanLoggingConstants.MAW);
		defaultLocations.add(BlInventoryScanLoggingConstants.CAM);
		return defaultLocations;
	}

	/**
	 * Gets the unboxing allowed locations.
	 *
	 * @return the unboxing allowed locations
	 */
	public static List<String> getUnboxingAllowedLocations()
	{
		final List<String> defaultLocations = new ArrayList<>();
		defaultLocations.addAll(getUnBoxingWorkStationLocations());
		defaultLocations.addAll(getDirtyCartLocations());
		defaultLocations.addAll(getDirtyPriorityCartLocations());

		return defaultLocations;
	}

	/**
	 * javadoc
	 *
	 * @return List<String> for default locations
	 */
	public static List<String> getDefaultBinLocation()
	{
		final List<String> defaultBinLocation = new ArrayList<>();
		defaultBinLocation.add(BlInventoryScanLoggingConstants.BIN);
		return defaultBinLocation;
	}

	/**
	 * Gets the tech eng repair locations.
	 *
	 * @return the tech eng repair locations
	 */
	public static List<String> getShippingAllowedLocations()
	{
		final List<String> shippingAllowedLocation = new ArrayList<>();
		shippingAllowedLocation.add(BlInventoryScanLoggingConstants.SHIPPER_WORKSTATION);
		shippingAllowedLocation.add(BlInventoryScanLoggingConstants.BIN);
		shippingAllowedLocation.add(BlInventoryScanLoggingConstants.CARRIER);
		shippingAllowedLocation.add(BlInventoryScanLoggingConstants.FRONT_DESK_SHIPPING_BIN);
		return shippingAllowedLocation;
	}

	/**
	 * Gets the front desk allowed working station locations.
	 *
	 * @return fdAllowedLocation
	 */
	public static List<String> getFrontDeskAllowedWorkStationLocations()
	{
		final List<String> fdAllowedLocation = new ArrayList<>();
		fdAllowedLocation.add(BlInventoryScanLoggingConstants.SHIPPER_WORKSTATION);
		fdAllowedLocation.add(BlInventoryScanLoggingConstants.FRONT_DESK_SHIPPER_WORKSTATION);
		fdAllowedLocation.add(BlInventoryScanLoggingConstants.FRONT_DESK_SHIPPING_BIN);
		fdAllowedLocation.add(BlInventoryScanLoggingConstants.FRONT_DESK_SHIPPED_MOBILE_CART);
		return fdAllowedLocation;
	}

	/**
	 * Gets the un boxing work station locations.
	 *
	 * @return the un boxing work station locations
	 */
	public static List<String> getUnBoxingWorkStationLocations()
	{
		final List<String> workStationLocations = new ArrayList<>();
		workStationLocations.add(BlInventoryScanLoggingConstants.UNBOXING_WORKSTATION);
		workStationLocations.add(BlInventoryScanLoggingConstants.UNBOXING_WORKSTATION_LOCATION);
		return workStationLocations;
	}

	/**
	 * Gets the dirty cart locations.
	 *
	 * @return the dirty cart locations
	 */
	public static List<String> getDirtyCartLocations()
	{
		final List<String> dirtyCartLocations = new ArrayList<>();
		dirtyCartLocations.add(BlInventoryScanLoggingConstants.DIRTY_GEAR_MOBILE_CART);
		dirtyCartLocations.add(BlInventoryScanLoggingConstants.DIRTY_MOBILE_LAUNDRY_BIN);
		return dirtyCartLocations;
	}


	/**
	 * Gets the dirty priority cart locations.
	 *
	 * @return the dirty priority cart locations
	 */
	public static List<String> getDirtyPriorityCartLocations()
	{
		final List<String> dirtyPriorityCartLocations = new ArrayList<>();
		dirtyPriorityCartLocations.add(BlInventoryScanLoggingConstants.DIRTY_PRIORITY_GEAR);
		dirtyPriorityCartLocations.add(BlInventoryScanLoggingConstants.DIRTY_PRIORITY_MOBILE_CART);
		return dirtyPriorityCartLocations;
	}
}
