package com.bl.core.services.documentupload;

import com.bl.core.model.VerificationDocumentMediaModel;
import de.hybris.platform.core.model.user.CustomerModel;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author Neeraj Singh
 */
public interface BlVerificationDocumentService {

  /**
   *
   * @param customerModel
   * @param verificationDocumentMediaModel
   * @param document
   */
  void uploadVerificationDocument(final CustomerModel customerModel, final VerificationDocumentMediaModel verificationDocumentMediaModel, final MultipartFile document);

  /**
   * Remove Verification Document
   * @param code
   * @return VerificationDocumentMedia
   */
  VerificationDocumentMediaModel removeVerificationDocument(final String code);

}
