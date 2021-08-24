/**
 *
 */
package com.bl.core.services.document.dao;

import com.bl.core.model.VerificationDocumentMediaModel;


/**
 * It is used to fetch the Verification Document
 * @author Avani Patel
 *
 */
public interface BlVerificationDocumentDao
{

	/**
	 * Remove Verification Document
	 *
	 * @param code
	 * @return VerificationDocumentMedia
	 */
	VerificationDocumentMediaModel removeVerificationDocument(final String code);

}
