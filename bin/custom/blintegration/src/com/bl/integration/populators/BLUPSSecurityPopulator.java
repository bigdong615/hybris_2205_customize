/**
 *
 */
package com.bl.integration.populators;

import org.springframework.beans.factory.annotation.Value;

import com.ups.xmlschema.xoltws.upss.v1.UPSSecurity;


/**
 * @author Aditi Sharma
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

	/**
	 * @return
	 */
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
