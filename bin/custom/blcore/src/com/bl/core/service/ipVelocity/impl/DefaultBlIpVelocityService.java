/**
 *
 */
package com.bl.core.service.ipVelocity.impl;

import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.dao.ipVelocity.BlIpVelocityDao;
import com.bl.core.model.IpVelocityFilterModel;
import com.bl.core.service.ipVelocity.BlIpVelocityService;
import com.bl.logging.BlLogger;


/**
 * @author Admin
 *
 */
public class DefaultBlIpVelocityService implements BlIpVelocityService
{

	private static final Logger LOG = Logger.getLogger(DefaultBlIpVelocityService.class);
	private static final int MILLI_TO_HOUR = 1000 * 60 * 60;
	public static final String IP_ADDRESS_RESTRICT_DURATION = "bl.ipaddress.restrict.duration";

	private BlIpVelocityDao blIpVelocityDao;
	private ModelService modelService;
	private ConfigurationService configurationService;
	private BaseStoreService baseStoreService;

	@Override
	public IpVelocityFilterModel getUserData(final String ipAddress, final String userId)
	{
		return getBlIpVelocityDao().getUserData(ipAddress, userId);
	}

	@Override
	public void createNewEntry(final String userIp, final String principal)
	{
		try
		{
			final IpVelocityFilterModel newModel = getModelService().create(IpVelocityFilterModel.class);
			newModel.setUserIp(userIp);
			newModel.setUserId(principal);
			newModel.setSuccessfulLogin(1);
			newModel.setUnSuccessfulLogin(0);
			newModel.setLoginCounter(1);
			newModel.setLastLoginAttemptedTimeStamp(new Date());
			getModelService().save(newModel);
		}
		catch (final ModelSavingException exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while creating the Ip Velocity user  : {}", principal);
		}

	}

	@Override
	public void updateDetails(final IpVelocityFilterModel velocityFilterModel, final boolean isSuccess)
	{

		final int ipAddressRestrictDuration = Integer
				.parseInt(getConfigurationService().getConfiguration().getString(IP_ADDRESS_RESTRICT_DURATION));
		if (isSuccess)
		{
			if ((new Date().getTime() - velocityFilterModel.getLastLoginAttemptedTimeStamp().getTime())
					/ MILLI_TO_HOUR > ipAddressRestrictDuration)
			{
				velocityFilterModel.setLoginCounter(1);
				velocityFilterModel.setSuccessfulLogin(1);
				velocityFilterModel.setUnSuccessfulLogin(0);
				velocityFilterModel.setLastLoginAttemptedTimeStamp(new Date());
			}
			else
			{
				velocityFilterModel.setSuccessfulLogin(velocityFilterModel.getSuccessfulLogin() + 1);
				velocityFilterModel.setLoginCounter(velocityFilterModel.getLoginCounter() + 1);
				velocityFilterModel.setLastLoginAttemptedTimeStamp(new Date());

			}
		}
		else
		{
			velocityFilterModel.setUnSuccessfulLogin(velocityFilterModel.getUnSuccessfulLogin() + 1);
			velocityFilterModel.setLoginCounter(velocityFilterModel.getLoginCounter() + 1);
			velocityFilterModel.setLastLoginAttemptedTimeStamp(new Date());
		}
		try
		{
			getModelService().save(velocityFilterModel);
		}
		catch (final ModelSavingException exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while updating the Ip Velocity user details  : {}", velocityFilterModel.getUserId());
		}
	}


	@Override
	public void checkIfIpNeedsToBlock(final String userIp)
	{
		final List<IpVelocityFilterModel> ips = getBlIpVelocityDao().getAll();
		//if one ip

		final Map<String, Map<Integer, Integer>> ipCounterMap = new HashMap();

		for (final IpVelocityFilterModel ipFilterModel : ips)
		{
			if (!ipCounterMap.containsKey(ipFilterModel.getUserIp()))
			{
				final Map<Integer, Integer> countSuccessMap = new HashMap();
				countSuccessMap.put(ipFilterModel.getLoginCounter(), ipFilterModel.getSuccessfulLogin());
				ipCounterMap.put(ipFilterModel.getUserIp(), countSuccessMap);
			}
			else
			{
				final Map<Integer, Integer> countSuccessMap = new HashMap();
				for (final Entry<String, Map<Integer, Integer>> entry : ipCounterMap.entrySet())
				{
					for (final Entry<Integer, Integer> entry1 : entry.getValue().entrySet())
					{
						countSuccessMap.put(entry1.getKey() + ipFilterModel.getLoginCounter(),
								entry1.getValue() + ipFilterModel.getSuccessfulLogin());
					}
				}
				ipCounterMap.put(ipFilterModel.getUserIp(), countSuccessMap);
			}
		}

		//iterating over map to check if successful login is less than 50% over login counter
		for (final Entry<String, Map<Integer, Integer>> ipCounts : ipCounterMap.entrySet())
		{
			for (final Entry<Integer, Integer> ipCountsinside : ipCounts.getValue().entrySet())
			{
				if (ipCounts.getKey().equals(userIp))
				{
					final float percent = (float) ipCountsinside.getValue() / ipCountsinside.getKey() * 100;
					if (percent < 50 && null != getBaseStoreService().getCurrentBaseStore())
					{
						final List<String> ipList = new ArrayList<>(getBaseStoreService().getCurrentBaseStore().getBlockedIpList());
						ipList.add(userIp);
						final BaseStoreModel store = getBaseStoreService().getCurrentBaseStore();
						store.setBlockedIpList(ipList);
						getModelService().save(store);

					}

				}
			}
		}

	}


	/**
	 * @return the blIpVelocityDao
	 */
	public BlIpVelocityDao getBlIpVelocityDao()
	{
		return blIpVelocityDao;
	}

	/**
	 * @param blIpVelocityDao
	 *           the blIpVelocityDao to set
	 */
	public void setBlIpVelocityDao(final BlIpVelocityDao blIpVelocityDao)
	{
		this.blIpVelocityDao = blIpVelocityDao;
	}

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	public ConfigurationService getConfigurationService()
	{
		return configurationService;
	}

	public void setConfigurationService(final ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}

	/**
	 * @return the baseStoreService
	 */
	protected BaseStoreService getBaseStoreService()
	{
		return baseStoreService;
	}

	/**
	 * @param baseStoreService
	 *           the baseStoreService to set
	 */
	public void setBaseStoreService(final BaseStoreService baseStoreService)
	{
		this.baseStoreService = baseStoreService;
	}




}
