package com.bl.core.model.handler;

import com.bl.core.order.dao.BlOrderDao;
import com.bl.logging.BlLogger;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * This class is responsible to get dynamic calculated value of rolling spend
 * @author Moumita
 */
public class BlCalculateRollingSpendHandler implements DynamicAttributeHandler<BigDecimal, CustomerModel>
{
	private static final Logger LOG = Logger.getLogger(BlCalculateCostForLaborHandler.class);
	private BlOrderDao orderDao;

	@Override
	public BigDecimal get(final CustomerModel customerModel)
	{
		final Date sameDayPastYear = getPastYearSameDay();
		final List<AbstractOrderModel> orders = getOrderDao().getOneYearOldCompletedOrders(sameDayPastYear);
		AtomicDouble totalPrice = new AtomicDouble();
		orders.forEach(order -> {
			totalPrice.addAndGet(order.getTotalPrice() - order.getTotalTax());
		});
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Rolling Spend value : {} ",
				totalPrice.get());
		return BigDecimal.valueOf(totalPrice.get());
	}

	public static Date getPastYearSameDay()
	{
		final Date currentDate = new Date();
		final Calendar calendar = Calendar.getInstance();
		calendar.setTime(currentDate);
		calendar.add(Calendar.YEAR, -1);
		return calendar.getTime();
	}

	@Override
	public void set(final CustomerModel customerModel, final BigDecimal rollingSpendAmount)
	{
		BlLogger.logMessage(LOG, Level.ERROR, "Setter for attribute CustomerModel.rollingSpend is not supported");
		throw new UnsupportedOperationException();
	}

	public BlOrderDao getOrderDao() {
		return orderDao;
	}

	public void setOrderDao(BlOrderDao orderDao) {
		this.orderDao = orderDao;
	}

}
