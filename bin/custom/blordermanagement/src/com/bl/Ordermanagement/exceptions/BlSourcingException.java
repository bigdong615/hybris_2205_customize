package com.bl.Ordermanagement.exceptions;

/**
 * Exception for sourcing and allocation of order.
 *
 * @author Sunil
 */
public class BlSourcingException extends RuntimeException  {

  public BlSourcingException(String errorMessage) {
    super(errorMessage);
  }

  public BlSourcingException(String errorMessage, Throwable err) {
    super(errorMessage, err);
  }
}
