package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.services.BlSourcingLocationService;
import com.bl.Ordermanagement.services.BlSourcingService;
import com.bl.Ordermanagement.strategy.BlSourcingStrategyService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlRushDeliveryModeModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.google.common.base.Preconditions;
import com.google.common.collect.Sets;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import de.hybris.platform.warehousing.sourcing.strategy.SourcingStrategy;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * SourcingService to source the order and create sourcing results.
 *
 * @author Sunil
 */
public class DefaultBlSourcingService implements BlSourcingService {

  private static final Logger LOG = Logger.getLogger(DefaultBlSourcingService.class);
  private BlSourcingStrategyService blSourcingStrategyService;
  private BlSourcingLocationService blSourcingLocationService;
  private ModelService modelService;
  private BlDatePickerService blDatePickerService;
  private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;

  /**
   * This is to source the order
   *
   * @param order the order
   * @return SourcingResults The SourcingResults
   */
  @Override
  public SourcingResults sourceOrder(final AbstractOrderModel order) {

    try {

    ServicesUtil.validateParameterNotNullStandardMessage("order", order);
    Preconditions.checkArgument(Objects.nonNull(order), "Parameter order cannot be null.");
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Starting sourcing Order : {}",
        order.getCode());

    final SourcingContext context = new SourcingContext();
    final SourcingResults result = new SourcingResults();
    result.setResults(Sets.newHashSet());
    result.setComplete(Boolean.FALSE);
    context.setResult(result);

     final List<AbstractOrderEntryModel> entryListForAllocation=   order.getEntries().stream().filter(orderEntryModel -> !orderEntryModel.isBundleMainEntry()).collect(
          Collectors.toList());
    //context.setOrderEntries(order.getEntries()); //NOSONAR
      context.setOrderEntries(entryListForAllocation);

    blSourcingLocationService.createSourcingLocation(context, blDeliveryStateSourcingLocationFilter.applyFilter(order));

    final List<SourcingStrategy> strategies = this.blSourcingStrategyService.getDefaultStrategies();

    final Iterator<SourcingStrategy> strategyItr = strategies.iterator();

      while (strategyItr.hasNext()) {
        final SourcingStrategy strategy = strategyItr.next();
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Apply sourcing strategy: {}",
            strategy.getClass().getSimpleName());

        strategy.source(context);

        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "Sourcing strategy '" + strategy.getClass().getSimpleName() + "' applied" + (
                context.getResult().isComplete() ? "" : " not") + " successfully");
        if (context.getResult().isComplete()) {
          break;
        }
      }

      updateShippingDatesForInternalTransfers(order, context.getResult());

      return context.getResult();
    } catch (final BlSourcingException blSourcingException) {

      throw blSourcingException;
    } catch (final Exception exception) {

      throw exception;
    }

  }

  /**
   * This method updates the actual rental start date for internal transfer cases.
   *
   * @param order the order
   * @param result the result
   */
  public void updateShippingDatesForInternalTransfers(final AbstractOrderModel order,
      final SourcingResults result) {

    if (CollectionUtils.isNotEmpty(result.getResults())) {

      result.getResults().forEach(sourcingResult -> {

        final ZoneDeliveryModeModel deliveryModeModel = (ZoneDeliveryModeModel) order.getDeliveryMode();

        if (isDeliveryModeForInternalTransfer(deliveryModeModel) && checkIfDifferentWarehouseAllocated(
            sourcingResult, deliveryModeModel)) {

          sourcingResult.setOrderTransferConsignment(true);
          final List<Date> holidayBlackoutDates = blDatePickerService
              .getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
          final Date newStartDate = BlDateTimeUtils
              .getDateWithSubtractedDays(1, order.getActualRentalStartDate(), holidayBlackoutDates);

          order.setActualRentalStartDate(newStartDate);
          modelService.save(order);
        }
      });
    }
  }

  /**
   * This method returns true if different warehouse is allocated other than that of the delivery
   * method.
   *
   * @param sourcingResult    the sourcingResult
   * @param deliveryModeModel the deliveryModeModel
   * @return true if both are different
   */
  private boolean checkIfDifferentWarehouseAllocated(final SourcingResult sourcingResult,
      final ZoneDeliveryModeModel deliveryModeModel) {

    return !sourcingResult.getWarehouse().getCode()
        .equalsIgnoreCase(deliveryModeModel.getWarehouse().getCode());
  }

  /**
   * This method returns true if the delivery method is eligible for internal transfer pre/post date
   * calculation.
   *
   * @param deliveryModeModel the order
   * @return true if matches
   */
  private boolean isDeliveryModeForInternalTransfer(final ZoneDeliveryModeModel deliveryModeModel) {

    return deliveryModeModel instanceof BlRushDeliveryModeModel || (
        deliveryModeModel instanceof BlPickUpZoneDeliveryModeModel && Arrays
            .asList(BlCoreConstants.BL_SAN_CARLOS, BlCoreConstants.BL_WALTHAM)
            .contains(deliveryModeModel.getCode()));
  }

  public BlSourcingLocationService getBlSourcingLocationService() {
    return blSourcingLocationService;
  }

  public void setBlSourcingLocationService(
      final BlSourcingLocationService blSourcingLocationService) {
    this.blSourcingLocationService = blSourcingLocationService;
  }

  public BlSourcingStrategyService getBlSourcingStrategyService() {
    return blSourcingStrategyService;
  }

  public void setBlSourcingStrategyService(
      final BlSourcingStrategyService blSourcingStrategyService) {
    this.blSourcingStrategyService = blSourcingStrategyService;
  }

  public BlDeliveryStateSourcingLocationFilter getBlDeliveryStateSourcingLocationFilter() {
    return blDeliveryStateSourcingLocationFilter;
  }

  public void setBlDeliveryStateSourcingLocationFilter(
      final BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter) {
    this.blDeliveryStateSourcingLocationFilter = blDeliveryStateSourcingLocationFilter;
  }


  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }

  public BlDatePickerService getBlDatePickerService() {
    return blDatePickerService;
  }

  public void setBlDatePickerService(final BlDatePickerService blDatePickerService) {
    this.blDatePickerService = blDatePickerService;
  }
}
