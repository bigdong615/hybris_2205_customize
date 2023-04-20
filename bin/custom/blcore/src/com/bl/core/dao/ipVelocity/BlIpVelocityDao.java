/**
 *
 */
package com.bl.core.dao.ipVelocity;

import java.util.List;

import com.bl.core.model.IpVelocityFilterModel;


/**
 * @author Admin
 *
 */



public interface BlIpVelocityDao
{


	IpVelocityFilterModel getUserData(String ipAddress, String userId);

	/**
	 * * @return +
	 */


	List<IpVelocityFilterModel> getAll();
}
