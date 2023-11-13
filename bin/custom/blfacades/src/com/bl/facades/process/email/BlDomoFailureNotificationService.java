/**
 *
 */
package com.bl.facades.process.email;

/**
 * @author Kumar
 *
 */
public interface BlDomoFailureNotificationService
{
	void send(String exception, String pk, String API);
}
