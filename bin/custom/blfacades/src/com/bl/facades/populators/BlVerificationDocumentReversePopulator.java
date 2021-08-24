package com.bl.facades.populators;

import com.bl.core.enums.DocumentType;
import com.bl.core.model.VerificationDocumentMediaModel;
import com.bl.facades.product.data.VerificationDocumentData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

/**
 * @author Neeraj Singh
 */
public class BlVerificationDocumentReversePopulator implements Populator<VerificationDocumentData, VerificationDocumentMediaModel> {

  @Override
  public void populate(final VerificationDocumentData verificationDocumentData, final VerificationDocumentMediaModel verificationDocumentMedia) throws ConversionException {

    if(verificationDocumentData.getDocumentType() != null) {
      verificationDocumentMedia.setDocumentType(DocumentType.valueOf(verificationDocumentData.getDocumentType()));
    }
  }
}