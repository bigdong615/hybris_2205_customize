package com.bl.Ordermanagement.exceptions;

/**
 * Exception for sourcing and allocation of order.
 *
 * @author Namrata Lohar
 */
public class BlShippingOptimizationException extends RuntimeException  {

  public BlShippingOptimizationException(final String errorMessage) {
    super(errorMessage);
  }

  public BlShippingOptimizationException(final String errorMessage, final Throwable err) {
    super(errorMessage, err);
  }
}
