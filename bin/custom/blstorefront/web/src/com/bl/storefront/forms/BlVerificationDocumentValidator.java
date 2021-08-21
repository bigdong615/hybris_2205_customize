package com.bl.storefront.forms;

import de.hybris.platform.util.Config;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import org.springframework.web.multipart.MultipartFile;

/**
 * class is used to validate VerificationDocumentForm
 *
 * @author Neeraj Singh
 */
@Component("blVerificationDocumentValidator")
public class BlVerificationDocumentValidator implements Validator {

  private static final String DOCUMENT_FORMAT = "bl.verification.document.format";
  private static final String DOCUMENT_SIZE = "bl.verification.document.size";

  @Override
  public boolean supports(Class<?> aClass) {
    return false;
  }

  @Override
  public void validate(final Object object, final Errors errors) {
    final VerificationDocumentForm verificationDocumentForm = (VerificationDocumentForm) object;
    final MultipartFile document = verificationDocumentForm.getDocument();

    if (document != null && !document.isEmpty()) {
      validateFileType(errors, document);
    } else {
      errors.rejectValue("file", "bl.verification.document");
    }

    if (document != null && !isFileSizeMatch(document)) {
      errors.rejectValue("file", "bl.verification.document.size");
    }
  }

  /**
   * This method converts bytes to MB and returns boolean value based on expected file size.
   *
   * @param document
   * @return boolean value
   */
  private boolean isFileSizeMatch(final MultipartFile document) {

    final double fileSize = (double) document.getSize() / (double) (1024 * 1024);
    if (fileSize <= Integer.parseInt(Config.getParameter(DOCUMENT_SIZE))) {
      return Boolean.TRUE;
    } else {
      return Boolean.FALSE;
    }
  }

  /**
   * This method checks uploaded document's file format.
   *
   * @param errors   the errors
   * @param document the document
   */
  private void validateFileType(final Errors errors, final MultipartFile document) {
    boolean isFileFormatMatch = false;
    final String[] documentFormat = Config.getParameter(DOCUMENT_FORMAT).split(",");

    for (String format : documentFormat) {

      if (format.equalsIgnoreCase(document.getContentType())) {
        isFileFormatMatch = true;
      }
    }
    if (Boolean.FALSE.equals(isFileFormatMatch)) {
      errors.rejectValue("file", "bl.verification.document.format");
    }
  }
}
