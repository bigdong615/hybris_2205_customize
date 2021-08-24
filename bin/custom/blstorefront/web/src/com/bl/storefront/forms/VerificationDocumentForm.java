package com.bl.storefront.forms;

import org.springframework.web.multipart.MultipartFile;

/**
 * This class is used for binding form data.
 * @author Neeraj Singh
 */
public class VerificationDocumentForm {

  private MultipartFile document;
  private String documentType;
  private String file;

  /**
 * @return the file
 */
public String getFile()
{
	return file;
}

/**
 * @param file the file to set
 */
public void setFile(String file)
{
	this.file = file;
}

public MultipartFile getDocument() {
    return document;
  }

  public void setDocument(MultipartFile document) {
    this.document = document;
  }

  public String getDocumentType() {
    return documentType;
  }

  public void setDocumentType(String documentType) {
    this.documentType = documentType;
  }
}
