/**
 *
 */
package com.bl.core.services.document.dao;

import com.bl.core.model.VerificationDocumentMediaModel;


/**
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
