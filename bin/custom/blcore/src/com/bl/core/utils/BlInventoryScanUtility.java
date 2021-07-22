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
		return repairLocations;
	}
}
