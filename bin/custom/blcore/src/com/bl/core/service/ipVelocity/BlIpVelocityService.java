/**
 *
 */
package com.bl.core.service.ipVelocity;

import com.bl.core.model.IpVelocityFilterModel;


/**
 * @author Admin
 *
 */
public interface BlIpVelocityService
{
	IpVelocityFilterModel getUserData(String ipAddress, String userId);

	/**
	 * @param userIp
	 * @param principal
	 */
	void createNewEntry(String userIp, String principal);

	/**
	 * @param velocityFilterModel
	 * @param b
	 */
	void updateDetails(IpVelocityFilterModel velocityFilterModel, boolean b);

	/**
	 * @param userIp
	 */
	void checkIfIpNeedsToBlock(String userIp);
}
