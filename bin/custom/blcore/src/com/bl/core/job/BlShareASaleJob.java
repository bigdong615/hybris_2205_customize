package com.bl.core.job;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.util.Config;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This job is responsible to send completed rental orders to ShareASale.
 *
 * @author Neeraj Singh
 */
public class BlShareASaleJob extends AbstractJobPerformable<CronJobModel> {

  private static final Logger LOGGER = Logger.getLogger(BlShareASaleJob.class);
  private final String merchantID = Config.getParameter("shareasale.merchantID.value");
  private final String apiToken = Config.getParameter("shareasale.token.value");
  private final String action = Config.getParameter("shareasale.action.value");
  private final String shareASalePerSaleValue = Config.getParameter("shareasale.shareASalePerSaleValue");
  private final String storeId = Config.getParameter("shareasale.storeID");
  private final String version = Config.getParameter("shareasale.version");
  private final String apiSecret = Config.getParameter("shareasale.apiSecret");
  private final String customerSecureURL = Config.getParameter("shareasale.customerSecureUrl");
  private final String sale = Config.getParameter("shareasale.pixel.value");

  private BlOrderDao orderDao;

  /**
   * It is responsible to send those completed rental orders to ShareASale, which orders are not sent to ShareASale.
   * @param cronJobModel the CronJobModel
   * @return PerformResult
   */
  @Override
  public PerformResult perform(final CronJobModel cronJobModel) {
    BlLogger.logFormatMessageInfo(LOGGER, Level.INFO, BlCoreConstants.START_SHARE_A_SALE_JOB);

    try {
      final List<AbstractOrderModel> abstractOrderModelList = orderDao
          .getCompletedRentalOrderForShareASale();
      if (CollectionUtils.isNotEmpty(abstractOrderModelList)) {
        for (final AbstractOrderModel abstractOrderModel : abstractOrderModelList) {
          StringBuilder url = new StringBuilder(BlCoreConstants.SHARE_A_SALE_URL);
          url.append(BlCoreConstants.SHARE_A_SALE_MERCHANT_ID).append(merchantID)
              .append(BlCoreConstants.SHARE_A_SALE_TOKEN).append(apiToken)
              .append(BlCoreConstants.SHARE_A_SALE_VERSION).append(version)
              .append(BlCoreConstants.SHARE_A_SALE_ACTION).append(action);
          url.append(BlCoreConstants.SHARE_A_SALE_DATE)
              .append(sasDateFormat(abstractOrderModel.getCreationtime()));
          url.append(BlCoreConstants.SHARE_A_SALE_ORDER_NUMBER)
              .append(abstractOrderModel.getCode());
          url.append(BlCoreConstants.SHARE_A_SALE_TRANS_TYPE).append(sale)
              .append(BlCoreConstants.SHARE_A_SALE_AMOUNT)
              .append(String.format(BlCoreConstants.SHARE_A_SALE_SUBTOTAL_FORMAT,
                  abstractOrderModel.getSubtotal()));
          url.append(BlCoreConstants.SHARE_A_SALE_TRACKING).append(abstractOrderModel.getCode())
              .append(BlCoreConstants.SHARE_A_SALE_TRACKING_S);
          url.append(BlCoreConstants.SHARE_A_SALE_STORE_ID).append(storeId)
              .append(BlCoreConstants.SHARE_A_SALE_PER_SALE)
              .append(shareASalePerSaleValue);
          url.append(BlCoreConstants.SHARE_A_SALE_NEW_CUSTOMER).append(
              isNewCustomer(abstractOrderModel) ? BlCoreConstants.ONE : BlCoreConstants.ZERO);
          if (StringUtils.isNotEmpty(customerSecureURL)) {
            url.append(BlCoreConstants.SHARE_A_SALE_URL_LINK)
                .append(URLEncoder.encode(customerSecureURL, BlCoreConstants.DEFAULT_ENCODING_STRING));
          }
          addCouponCodeToUrl(abstractOrderModel, url);
          BlLogger
              .logFormatMessageInfo(LOGGER, Level.DEBUG, BlCoreConstants.SHARE_A_SALE_NEW_AC_URL,
                  url);

          DateFormat dateFormat = new SimpleDateFormat(BlCoreConstants.SHARE_A_SALE_DATE_FORMAT);
          dateFormat.setTimeZone(TimeZone.getTimeZone(BlCoreConstants.SHARE_A_SALE_GMT));
          String date = dateFormat.format(new Date());

          String auth = apiToken + BlCoreConstants.RATIO + date + BlCoreConstants.RATIO + action + BlCoreConstants.RATIO + apiSecret;
          String authHash = DigestUtils.sha256Hex(auth);

          if (makeHttpConnectionToShareASale(url, date, authHash)) {
            continue;
          }

          abstractOrderModel.setShareASaleSent(Boolean.TRUE);
          modelService.save(abstractOrderModel);
          modelService.refresh(abstractOrderModel);
        }

      }
      BlLogger.logFormatMessageInfo(LOGGER, Level.INFO, BlCoreConstants.SHARE_A_SALE_JOB_FINISH);
      return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
    } catch (final Exception exception) {
      BlLogger.logFormattedMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
          exception,
          BlCoreConstants.SHARE_A_SALE_JOB_ERROR_MSG);
      return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
    }
  }

  /**
   * This method checks, if an order contains coupon code then add it to the ShareASale URL.
   *
   * @param abstractOrderModel the AbstractOrderModel
   * @param url                the URL
   * @throws UnsupportedEncodingException
   */
  private void addCouponCodeToUrl(final AbstractOrderModel abstractOrderModel,
      final StringBuilder url)
      throws UnsupportedEncodingException {
    final Collection<String> appliedCouponCodes = abstractOrderModel.getAppliedCouponCodes();
    if (CollectionUtils.isNotEmpty(appliedCouponCodes)) {
      url.append(BlCoreConstants.SHARE_A_SALE_COUPON_CODE);
      int size= appliedCouponCodes.size();
      for (final String couponCode : appliedCouponCodes) {
        url.append(URLEncoder.encode(couponCode, BlCoreConstants.DEFAULT_ENCODING_STRING));
        if(size > 1){
          size--;
          url.append(BlCoreConstants.SHARE_A_SALE_COMMA);
        }
      }
    }
  }

  /**
   * This method makes http connection to share A sale URL.
   *
   * @param url      the URL
   * @param date     the Date
   * @param authHash the Auth Hash
   * @return boolean
   * @throws MalformedURLException the MalformedURLException
   */
  private boolean makeHttpConnectionToShareASale(final StringBuilder url, final String date,
      final String authHash)
      throws MalformedURLException {
    final URL urlObject = new URL(url.toString());
    BlLogger
        .logFormatMessageInfo(LOGGER, Level.INFO, BlCoreConstants.SHARE_A_SALE_NEW_URL_SENDING_MSG,
            urlObject);
    boolean isConnectionFailure = false;
    try {
      HttpURLConnection httpURLConnection = (HttpURLConnection) urlObject.openConnection();
      httpURLConnection.setRequestProperty(BlCoreConstants.SHARE_A_SALE_AUTH_DATE, date);
      httpURLConnection.setRequestProperty(BlCoreConstants.SHARE_A_SALE_AUTH, authHash);
      httpURLConnection.connect();
      if (httpURLConnection.getResponseCode() != HttpURLConnection.HTTP_OK) {
        isConnectionFailure = true;
        throw new IOException(
            BlCoreConstants.SHARE_A_SALE_JOB_HTTP_STATUS_MSG + httpURLConnection.getResponseCode()
                + httpURLConnection.getResponseMessage());
      }
    } catch (final Exception exception) {
      BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR,
          BlCoreConstants.SHARE_A_SALE_JOB_HTTP_CONNECTION_FAILURE, exception, url);
      if (BooleanUtils.isTrue(isConnectionFailure)) {
        return true;
      }
    }
    return false;
  }

  /**
   * It is used to format order creation date.
   *
   * @param orderCreationDate is order creation date
   * @return date in String format.
   */
  private String sasDateFormat(final Date orderCreationDate) {
    return new SimpleDateFormat(BlCoreConstants.SHARE_A_SALE_ORDER_CREATION_DATE_FORMAT)
        .format(orderCreationDate);
  }

  /**
   * It is used to identify new customer based on order.
   *
   * @param abstractOrderModel the AbstractOrderModel
   * @return boolean value.
   */
  private boolean isNewCustomer(final AbstractOrderModel abstractOrderModel) {
    if (abstractOrderModel.getUser() != null) {
      Collection<OrderModel> orderModels = abstractOrderModel.getUser().getOrders();
      if (CollectionUtils.isNotEmpty(orderModels) && orderModels.size() > 1) {
        return false;
      }
    }
    return true;
  }

  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(BlOrderDao orderDao) {
    this.orderDao = orderDao;
  }
}
