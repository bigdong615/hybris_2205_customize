package com.bl.backoffice.widget.controller;

import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;
import com.hybris.backoffice.widgets.advancedsearch.impl.AdvancedSearchData;
import com.hybris.backoffice.widgets.advancedsearch.impl.SearchConditionData;
import com.hybris.cockpitng.annotations.SocketEvent;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created to override the OOB Search engine controller
 * @author Manikandan
 */
public class BlCustomAdvancedSearchEngineController extends com.hybris.backoffice.widgets.advancedsearch.engine.AdvancedSearchEngineController
{

  private final List<String> stringList = Arrays
      .asList(BlCoreConstants.PULLED_ORDERS_QUEUE, BlCoreConstants.AWAITING_ORDERS_QUEUE , BlCoreConstants.COMPLTED_ORDER_QUEUE);

  private static final Logger LOG = Logger.getLogger(BlCustomAdvancedSearchEngineController.class);

  /**
   * This method override to set the date if its null
   * @param searchData advance search data
   */
  @Override
  @SocketEvent(socketId = "searchData")
  public void onSearchDataInput(final AdvancedSearchData searchData) {
    try {
      if (Objects.nonNull(searchData) && Objects
          .nonNull(searchData.getConditions(BlCoreConstants.OPTIMIZEDSHIPPINGSTARTDATE))
          && stringList.contains(searchData.getTypeCode())) {
        final List<SearchConditionData> searchConditionDataList = searchData
            .getConditions(BlCoreConstants.OPTIMIZEDSHIPPINGSTARTDATE);
        if (CollectionUtils.isNotEmpty(searchConditionDataList)) {
          searchConditionDataList.stream().forEach(searchConditionData -> {
            if (Objects.isNull(searchConditionData.getValue())) {
              searchConditionData.updateValue(Calendar.getInstance().getTime());
              BlLogger
                  .logFormattedMessage(LOG, Level.INFO, "optimizedShippingStartDate for queue {} ",
                      String.valueOf(Calendar.getInstance().getTime()));
            }
          });
        }
      }
    }
    catch (final Exception e) {
      BlLogger.logMessage(LOG , Level.ERROR , "Error while executing onSearchDataInput :- BlCustomAdvancedSearchEngineController" , e);
    }
        super.onSearchDataInput(searchData);
    }

}
