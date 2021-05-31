package com.bl.Ordermanagement.exceptions;

public class BlSourcingException extends RuntimeException  {

  public BlSourcingException(String errorMessage) {
    super(errorMessage);
  }

  public BlSourcingException(String errorMessage, Throwable err) {
    super(errorMessage, err);
  }
}
