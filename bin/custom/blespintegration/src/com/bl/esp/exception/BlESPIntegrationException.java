package com.bl.esp.exception;

/**
 * Exception for ESP integration issues.
 *
 * @author Sunil
 */
public class BlESPIntegrationException extends RuntimeException {

  private String errorCode;

  public BlESPIntegrationException(final String errorMessage) {
    super(errorMessage);
  }

  public BlESPIntegrationException(final String errorMessage, final Throwable err) {
    super(errorMessage, err);
  }

  public String getErrorCode() {
    return errorCode;
  }

  public void setErrorCode(final String errorCode) {
    this.errorCode = errorCode;
  }
}
