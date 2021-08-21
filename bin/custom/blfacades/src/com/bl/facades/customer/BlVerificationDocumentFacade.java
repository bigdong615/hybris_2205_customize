package com.bl.facades.customer;

import com.bl.facades.product.data.VerificationDocumentData;

public interface BlVerificationDocumentFacade {

  /**
   *
   * @param verificationDocumentData
   */
  void uploadDocument(final VerificationDocumentData verificationDocumentData);
}
