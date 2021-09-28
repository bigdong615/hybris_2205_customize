package com.bl.Ordermanagement.services;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

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

  /**
   * It allocated each order entry
   * @param context the context
   * @param result the result
   * @param finalSourcingLocation final sourcing location
   * @param entry the order entry
   * @param allEntrySourceComplete sourcing of all entries completed or not
   * @param quantity the quantity
   */
  void fulfillEachEntry(final SourcingContext context, final SourcingResult result,
      final SourcingLocation finalSourcingLocation, final AbstractOrderEntryModel entry,
      final List<AtomicBoolean> allEntrySourceComplete, final Long quantity);
}
