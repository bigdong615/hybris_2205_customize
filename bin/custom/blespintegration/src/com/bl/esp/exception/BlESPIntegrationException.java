package com.bl.esp.exception;

/**
 * Exception for ESP integration issues.
 *
 * @author Sunil
 */
public class BlESPIntegrationException extends RuntimeException {

  private String errorCode;
  private String requestString;

  public BlESPIntegrationException(final String errorMessage) {
    super(errorMessage);
  }

  public BlESPIntegrationException(final String errorMessage, final Throwable err) {
    super(errorMessage, err);
  }

  public BlESPIntegrationException(final String errorMessage, final String errorCode) {
    super(errorMessage);
    this.errorCode = errorCode;
  }

  public BlESPIntegrationException(final String errorMessage, final String errorCode,
      final String requestString) {
    super(errorMessage);
    this.errorCode = errorCode;
    this.requestString = requestString;
  }

  public BlESPIntegrationException(final String errorMessage, final String errorCode,
      final Throwable throwableError) {
    super(errorMessage, throwableError);
    this.errorCode = errorCode;
  }

  public BlESPIntegrationException(final String errorMessage, final String errorCode,
      final String requestString, final Throwable throwableError) {
    super(errorMessage, throwableError);
    this.errorCode = errorCode;
    this.requestString = requestString;
  }

    public String getErrorCode() {
    return errorCode;
  }

  public void setErrorCode(final String errorCode) {
    this.errorCode = errorCode;
  }

  public String getRequestString() {
    return requestString;
  }

  public void setRequestString(final String requestString) {
    this.requestString = requestString;
  }

}
