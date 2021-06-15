package com.bl.Ordermanagement.exceptions;

/**
 * Exception for sourcing and allocation of order.
 *
 * @author Sunil
 */
public class BlSourcingException extends RuntimeException  {

  public BlSourcingException(final String errorMessage) {
    super(errorMessage);
  }

  public BlSourcingException(final String errorMessage, final Throwable err) {
    super(errorMessage, err);
  }
}
