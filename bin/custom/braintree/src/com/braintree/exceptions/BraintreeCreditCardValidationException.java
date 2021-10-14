package com.braintree.exceptions;

/**
 * The Class BraintreeCreditCardValidationException is use to throw at runtime for CC verification falied due to error in CVV.
 * 
 * @author Ravikumar
 */
public class BraintreeCreditCardValidationException extends RuntimeException
{
  /** The cvv error code. */
  private String cvvErrorCode;

  /** The base exception. */
  private Exception baseException;

  /**
   * Instantiates a new braintree credit card validation exception.
   */
  public BraintreeCreditCardValidationException() {}

  public BraintreeCreditCardValidationException(final String message, final String cvvErrorCode) {
    super(message);
    this.cvvErrorCode = cvvErrorCode;
  }

  /**
   * Instantiates a new braintree credit card validation exception.
   *
   * @param message the message
   * @param exception the exception
   */
  public BraintreeCreditCardValidationException(final String message, final Throwable exception) {
    super(message, exception);
  }

  /**
   * Instantiates a new braintree credit card validation exception.
   *
   * @param message the message
   */
  public BraintreeCreditCardValidationException(final String message) {
    super(message);
  }

  /**
   * Instantiates a new braintree credit card validation exception.
   *
   * @param exception the exception
   */
  public BraintreeCreditCardValidationException(final Throwable exception) {
    super(exception);
  }

  /**
   * Sets the base exception.
   *
   * @param baseException the new base exception
   */
  public void setBaseException(final Exception baseException)
  {
    this.baseException = baseException;
  }

  /**
   * Gets the base exception.
   *
   * @return the base exception
   */
  public Exception getBaseException()
  {
    return this.baseException;
  }

  /**
   * Gets the cvv error code.
   *
   * @return the cvvErrorCode
   */
  public String getCvvErrorCode()
  {
    return cvvErrorCode;
  }

  /**
   * Sets the cvv error code.
   *
   * @param cvvErrorCode the cvvErrorCode to set
   */
  public void setCvvErrorCode(final String cvvErrorCode)
  {
    this.cvvErrorCode = cvvErrorCode;
  }
}
