/**
 *
 */
package com.bl.integration.populators;

import org.springframework.beans.factory.annotation.Value;

import com.bl.integration.ups.upss.v1.pojo.UPSSecurity;


/**
 * @author Dell
 *
 */
public class BLUPSSecurityPopulator
{
	@Value("${blintegration.ups.license.number}")
	private String licenseNumber;

	@Value("${blintegration.ups.license.uid}")
	private String uid;

	@Value("${blintegration.ups.license.password}")
	private String password;

	public UPSSecurity populateUPSSecurity()
	{
		final UPSSecurity upss = new UPSSecurity();
		final UPSSecurity.ServiceAccessToken upsSvcToken = new UPSSecurity.ServiceAccessToken();
		upsSvcToken.setAccessLicenseNumber(licenseNumber);
		upss.setServiceAccessToken(upsSvcToken);
		final UPSSecurity.UsernameToken upsSecUsrnameToken = new UPSSecurity.UsernameToken();
		upsSecUsrnameToken.setUsername(uid);
		upsSecUsrnameToken.setPassword(password);
		upss.setUsernameToken(upsSecUsrnameToken);
		return upss;
	}

}
