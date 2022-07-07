package com.bl.core.dao.warehouse.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Lists;

/**
 * It is used to get consignments.
 *
 * @author Sunil
 */
public class DefaultBlConsignmentDao implements BlConsignmentDao {

  private static final Logger LOG = Logger.getLogger(DefaultBlConsignmentDao.class);
  private FlexibleSearchService flexibleSearchService;

  private static final String DATE_PARAM = "} BETWEEN ?startDate AND ?endDate ";
  private static final String FIND_READY_TO_SHIP_CONSIGNMENTS_FOR_DATE = "SELECT {pk} FROM {Consignment as con},{Order as o} WHERE {con:STATUS} NOT IN ({{SELECT {cs:PK} FROM {ConsignmentStatus as cs} WHERE {cs:CODE} = ?status1 OR {cs:CODE} = ?status2}})" +
			" AND {con:optimizedShippingStartDate"+ DATE_PARAM + "AND {o.pk} = {con.order} AND {o:versionID} IS NULL";
  
  private static final String CONSIGNMENT_FOR_RETURN_DATE_QUERY = "SELECT {con:" + ItemModel.PK + "} FROM {" 
		  + ConsignmentModel._TYPECODE + " as con} WHERE {con:" + ConsignmentModel.OPTIMIZEDSHIPPINGENDDATE +"} = ?returnDate";
  
  private static final String CONSIGNMENT_ENTRY_FOR_SERIAL_AND_FROM_DATE = "SELECT {ce:" + ItemModel.PK + "} from {"
			+ ConsignmentEntryModel._TYPECODE + " as ce}, {"
			+ ConsignmentModel._TYPECODE + " as con} where {ce:" + ConsignmentEntryModel.SERIALPRODUCTS
			+ "} LIKE CONCAT('%',CONCAT(?serial,'%'))" + " and {con:" + ItemModel.PK + "} = {ce:"
			+ ConsignmentEntryModel.CONSIGNMENT + "} and {con:" + ConsignmentModel.OPTIMIZEDSHIPPINGSTARTDATE + "} >= ?fromDate"; 

  /**
   * Get consignments
   *
   * @return ConsignmentModels
   */
  @Override
  public List<ConsignmentModel> getReadyToShipConsignmentsForDate(final Date shipDate) {

    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(FIND_READY_TO_SHIP_CONSIGNMENTS_FOR_DATE);

    final List<ConsignmentStatus> statusList = new ArrayList<>();
    statusList.add(ConsignmentStatus.CANCELLED);
    statusList.add(ConsignmentStatus.BL_SHIPPED);

    addQueryParameter(shipDate, statusList, fQuery);

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Flexible query for getting consignments to ship for date {} is  - {}", shipDate, fQuery.toString());

    final SearchResult<ConsignmentModel> result = getFlexibleSearchService().search(fQuery);
    final List<ConsignmentModel> consignmentModels = result.getResult();


    if (CollectionUtils.isEmpty(consignmentModels))
    {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "No Consignments available to ship for date {}", shipDate);
      return Collections.emptyList();
    }

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "No of consignments available = {} to ship for date {}", consignmentModels.size(), shipDate);

    return consignmentModels;
  }

  /**
   * It adds the parameters value into query
   * @param shipDate
   * @param statusList
   * @param fQuery
   */
  private void addQueryParameter(final Date shipDate, final List<ConsignmentStatus> statusList,
      final FlexibleSearchQuery fQuery) {

    SimpleDateFormat simpleformat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    final Calendar startDate = BlDateTimeUtils.getFormattedStartDay(shipDate);
    fQuery.addQueryParameter(BlCoreConstants.START_DATE, simpleformat.format(startDate.getTime()));

    final Calendar endDate = BlDateTimeUtils.getFormattedEndDay(shipDate);
    fQuery.addQueryParameter(BlCoreConstants.END_DATE, simpleformat.format(endDate.getTime()));

    fQuery.addQueryParameter(BlCoreConstants.STATUS1, statusList.get(0).getCode());
    fQuery.addQueryParameter(BlCoreConstants.STATUS2, statusList.get(1).getCode());

  }
  
  /**
   * {@inheritDoc}
   */
  @Override
	public List<ConsignmentModel> getConsignmentForReturnDate(final Date returnDate)
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(CONSIGNMENT_FOR_RETURN_DATE_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.RETURN_DATE, returnDate);
		final SearchResult<ConsignmentModel> search = getFlexibleSearchService().<ConsignmentModel> search(fQuery);
		if (Objects.isNull(search) || CollectionUtils.isEmpty(search.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"DefaultBlConsignmentDao : getConsignmentForReturnDate : No Consignment found for return date : {}", returnDate);
			return Lists.newArrayList();
		}
		final List<ConsignmentModel> result = search.getResult();
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment found : {} for return date: {}", result, returnDate);
		return Lists.newArrayList(result);
	}
  
	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<ConsignmentEntryModel> getConsignmentEntriesForSerialCodeAndDate(final BlSerialProductModel serial,
			final Date fromDate)
	{
		Validate.notNull(serial, "Serial Product must not be null", null);
		Validate.notNull(fromDate, "From Date must not be null", null);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(CONSIGNMENT_ENTRY_FOR_SERIAL_AND_FROM_DATE);
		fQuery.addQueryParameter(BlCoreConstants.SERIAL, serial);
		final SimpleDateFormat simpleformat = new SimpleDateFormat(BlCoreConstants.QUERY_DATE_FORMAT);
		final Calendar startDate = BlDateTimeUtils.getFormattedStartDay(fromDate);
		fQuery.addQueryParameter(BlCoreConstants.FROM_DATE, simpleformat.format(startDate.getTime()));
		final SearchResult<ConsignmentEntryModel> search = getFlexibleSearchService().<ConsignmentEntryModel> search(fQuery);
		if (Objects.isNull(search) || CollectionUtils.isEmpty(search.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"DefaultBlConsignmentDao : getConsignmentEntriesForSerialCodeAndDate : No ConsignmentEntry found for serial : {} and from date : {}",
					serial.getCode(), fromDate);
			return Lists.newArrayList();
		}
		final List<ConsignmentEntryModel> result = search.getResult();
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment Entry found : {} for serial : {} and from date: {}", result,
				serial.getCode(), fromDate);
		return Lists.newArrayList(result);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public ConsignmentModel getConsignmentForSerialCode(final String serialCode, final String orderCode)
	{
		
		Validate.notNull(serialCode, "Serial Product must not be null", null);
		Validate.notNull(orderCode, "Order must not be null", null);
		
		final String consignmentList = "select {c.pk} from {consignment as c join order as o on {c:order}={o:pk} join consignmententry as ce on {ce:consignment}={c:pk}} where {o.code} = ?orderCode AND  {ce:serialProducts} LIKE CONCAT('%',CONCAT(?serialCode,'%'))";
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(consignmentList);
		fQuery.addQueryParameter(BlCoreConstants.SERIAL_CODE, serialCode);
		fQuery.addQueryParameter(BlCoreConstants.ORDER_CODE, orderCode);
		
		
		final SearchResult<ConsignmentModel> search = getFlexibleSearchService().<ConsignmentModel> search(fQuery);
		if (Objects.isNull(search) || CollectionUtils.isEmpty(search.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"DefaultBlConsignmentDao : getConsignmentEntriesForSerialCodeAndDate : No ConsignmentEntry found for serial : {}",serialCode);
			return null;
		}
		final List<ConsignmentModel> result = search.getResult();
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment Entry found : {} for serial : {}", result,serialCode);
		return result.get(0);	
	
	}

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(
      final FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }

}
