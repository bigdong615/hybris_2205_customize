package com.bl.facades.customer;

import com.bl.core.enums.DocumentType;
import com.bl.core.model.VerificationDocumentMediaModel;
import com.bl.facades.product.data.VerificationDocumentData;
import java.util.List;
import java.util.Map;

public interface BlVerificationDocumentFacade {

  /**
   *
   * @param verificationDocumentData
   */
  void uploadDocument(final VerificationDocumentData verificationDocumentData);

  /**
   * This method is used to get List Of Document From Customer
   *
   */
  Map<String, List<VerificationDocumentMediaModel>> getListOfDocumentFromCustomer();

  /**
   * This method is used to remove document
   * @param code
   */
  void removeDocument(final String code);
}
