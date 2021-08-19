package com.bl.Ordermanagement.services;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;

/**
 * It is used to assign serial products to sourcing results in context.
 *
 * @author Sunil
 */
public interface BlAssignSerialService {

  /**
   * It assigns serial products to sourcing results in context.
   *
   * @param context, the SourcingContext
   */
  boolean assignSerialsFromLocation(final SourcingContext context,
      final SourcingLocation sourcingLocation)  throws BlSourcingException;

  /**
   * It checks whether all entries are fulfilled or not in context.
   *
   * @param context, the SourcingContext
   */
  boolean isAllQuantityFulfilled(final SourcingContext context);
}
