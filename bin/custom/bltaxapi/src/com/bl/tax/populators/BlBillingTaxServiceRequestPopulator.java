package com.bl.tax.populators;

import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.bl.tax.Addresses;
import com.bl.tax.AddressesData;
import com.bl.tax.TaxLine;
import com.bl.tax.TaxRequestData;
import com.bl.tax.billing.BillingPojo;
import com.bl.tax.constants.BltaxapiConstants;
import com.bl.tax.utils.BlTaxAPIUtils;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.ordersplitting.impl.DefaultWarehouseService;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.util.Config;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Lists;

import java.text.ParseException;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

/**
 * This populator created for preparing avalara tax request
 * @author Jyoti Swamy
 */
public class BlBillingTaxServiceRequestPopulator implements Populator<BillingPojo, TaxRequestData> {

  private static final Logger LOG = Logger.getLogger(BlBillingTaxServiceRequestPopulator.class);
 private SessionService sessionService;
  private ConfigurationService configurationService;

  @Override
  public void populate(BillingPojo billing, TaxRequestData taxRequest) throws ConversionException {
    AbstractOrderModel abstractOrder=billing.getOrder();
    taxRequest.setCompanyCode(getCompanyCode());
    taxRequest.setCode(abstractOrder.getCode());
    taxRequest.setType(BooleanUtils.isTrue(abstractOrder.getStatus() == OrderStatus.COMPLETED) ? BltaxapiConstants.SALESINVOICE : BltaxapiConstants.SALESORDER);
    setOrderDateToRequest(taxRequest);
    taxRequest.setCustomerCode(abstractOrder.getUser().getUid());
    taxRequest.setSalesPersonCode(null);
    taxRequest.setOriginCode(BltaxapiConstants.ORIGIN);
    taxRequest.setDestinationCode(BltaxapiConstants.DESTINATION);
    try {
      setTaxCommittedToRequest(abstractOrder, taxRequest);
    } catch (ParseException e) {
      BlLogger.logMessage(LOG, Level.ERROR, "Error while setting request for tax commit ", e);
    }
    taxRequest.setAddresses(createAddressesForOrderTax(abstractOrder));
    taxRequest.setLines(createdTaxLineForRequest(billing));
    taxRequest.setCurrencyCode(abstractOrder.getCurrency().getIsocode());
    taxRequest.setIsShippingTax(false);
    taxRequest.setShippingAmount(null);

  }


  /**
   * Gets the company code.
   *
   * @return the company code
   */
  private String getCompanyCode()
  {
    final String companyCode = getConfigurationService().getConfiguration().getString(BltaxapiConstants.COMPANY_CODE);
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Avalara Company code : {}", companyCode);
    return companyCode;
  }

  /**
   *
   * @param billing
   * @return
   */
  private List<TaxLine> createdTaxLineForRequest(final BillingPojo billing) {

      final List<TaxLine> taxLines = new ArrayList<>();

      final TaxLine taxLine = new TaxLine();
      taxLine.setQuantity(1);
      taxLine.setNumber(0);
      taxLine.setItemCode(getTrimmedProductCodeFromProduct(billing.getSerialNo()));
      taxLine.setAmount(billing.getAmount());
      taxLine.setDescription(StringUtils.EMPTY);
      taxLine.setTaxCode(setPayBillTaxCode(billing.getBillingChargesReason()));
      taxLines.add(taxLine);


      return taxLines;
  }




  /**
   * this method created to prepare address for avalara request
   * @param abstractOrder abstractOrder
   * @return Addresses
   */
  private Addresses createAddressesForOrderTax(final AbstractOrderModel abstractOrder)
  {
    final Addresses addresses = new Addresses();
    final AddressModel deliveryAddressForOrder = abstractOrder.getDeliveryAddress();
    if (deliveryAddressForOrder != null)
    {
      final AddressesData shipTo = new AddressesData();
      shipTo.setFirstName(deliveryAddressForOrder.getFirstname());
      shipTo.setLastName(deliveryAddressForOrder.getLastname());
      shipTo.setLine1(deliveryAddressForOrder.getLine1());
      shipTo.setLine2(deliveryAddressForOrder.getLine2());
      shipTo.setCity(deliveryAddressForOrder.getTown());

      if(null != deliveryAddressForOrder.getRegion()) {
        shipTo.setState(deliveryAddressForOrder.getRegion().getName());
        shipTo.setRegion(deliveryAddressForOrder.getRegion().getIsocode());
      }
      if(null != deliveryAddressForOrder.getCountry() && null != deliveryAddressForOrder.getCountry().getIsocode()) {
        shipTo.setCountry(deliveryAddressForOrder.getCountry().getIsocode());
      }
      shipTo.setPostalCode(deliveryAddressForOrder.getPostalcode());
      shipTo.setPhone(deliveryAddressForOrder.getPhone1());
      shipTo.setEmail(deliveryAddressForOrder.getEmail());
      shipTo.setAddressType(BltaxapiConstants.EMPTY_STRING);
      addresses.setShipTo(shipTo);
    }
    final AddressesData shipFrom = new AddressesData();
    shipFrom.setLine1(getValuesFromProperty(BltaxapiConstants.LINE_1));
    shipFrom.setCity(getValuesFromProperty(BltaxapiConstants.CITY));
    shipFrom.setRegion(getValuesFromProperty(BltaxapiConstants.REGION));
    shipFrom.setCountry(getValuesFromProperty(BltaxapiConstants.COUNTRY));
    shipFrom.setPostalCode(getValuesFromProperty(BltaxapiConstants.POSTAL_CODE));
    addresses.setShipFrom(shipFrom);
    return addresses;
  }


  /**
   * To set product tax code as per unpaid bill charge
   */

  private String setPayBillTaxCode(final ItemBillingChargeTypeEnum billChargeType)
  {
    switch (billChargeType.getCode())
    {
      case "LATE CHARGE":
        return getValuesFromProperty(BltaxapiConstants.LATE_FEE_TAX_CODE);

      case "REPAIR CHARGE":
      case "CUSTOMER RESPONSIBLE REPAIRS":
        return getValuesFromProperty(BltaxapiConstants.REPAIR_TAX_CODE);

      case "MISSING CHARGE":
        return getValuesFromProperty(BltaxapiConstants.MISSING_TAX_CODE);

      default:
        return null;
    }
  }

  /**
   * To set orderDate to request
   */
  private void setOrderDateToRequest(final TaxRequestData taxRequest) {
    taxRequest.setDate(DateFormatUtils.format(new Date(), BltaxapiConstants.LOCAL_DATE_FORMAT));
  }


  /**
   * Validate and set tax excemption details to request
   */
  private void setTaxCommittedToRequest(final AbstractOrderModel abstractOrder , final TaxRequestData taxRequest) throws ParseException {
    if(BooleanUtils.isTrue(abstractOrder.getUser().getIsTaxExempt()) && isTaxExemptValid(abstractOrder)) {
      String addressState = BltaxapiConstants.EMPTY_STRING;
      if(null != abstractOrder.getDeliveryAddress().getRegion()) {
        addressState = abstractOrder.getDeliveryAddress().getRegion().getName();
      }
      taxRequest.setTaxExemptState(addressState.equalsIgnoreCase(abstractOrder.getUser().getTaxExemptState()) ? addressState : null);
      final Date endDay = getDateForRequest(abstractOrder);
      if (null != abstractOrder.getUser().getTaxExemptExpiry() && null != endDay && isTaxExemptDateValid(abstractOrder ,endDay) && null != taxRequest.getTaxExemptState()) {
        taxRequest.setTaxExemptExpiry(abstractOrder.getUser().getTaxExemptExpiry());
        taxRequest.setExemptionNo(StringUtils.isNotBlank(abstractOrder.getUser().getTaxExemptNumber()) ? abstractOrder.getUser().getTaxExemptNumber() : null);
        taxRequest.setCommit(abstractOrder.getUser().getIsTaxExempt());
      }
      else {
        taxRequest.setCommit(false);
      }
    }
    else {
      taxRequest.setCommit(false);
    }
  }

  private Date getDateForRequest(final AbstractOrderModel abstractOrder) throws ParseException {
    final RentalDateDto rentalDateDto = getRentalDatesFromSession();
    if (null != rentalDateDto && null != rentalDateDto.getSelectedToDate()) {
      return BlTaxAPIUtils.getDate(rentalDateDto.getSelectedToDate() , BltaxapiConstants.DATE_FORMAT);
    }

    if (abstractOrder.getEntries().stream()
            .anyMatch(abstractOrderEntryModel ->
                    abstractOrderEntryModel.getProduct() instanceof BlSerialProductModel)) {
      return abstractOrder.getDate();
    }
    return abstractOrder.getDate();
  }

  /**
   * This method check for rental end date is on or before tax certificate expiry
   */
  private boolean isTaxExemptDateValid(final AbstractOrderModel abstractOrder , final Date endDay) {
    return endDay.before(abstractOrder.getUser().getTaxExemptExpiry()) || DateUtils.isSameDay(abstractOrder.getUser().getTaxExemptExpiry() ,endDay);
  }


  /**
   * This method created to check whether the payment is captured
   */

  private boolean isTaxExemptValid(final AbstractOrderModel abstractOrderModel) {

    if(CollectionUtils.isEmpty(abstractOrderModel.getPaymentTransactions())) {
      return true;
    }
    else {
      for (final PaymentTransactionModel paymentTransactionModel : abstractOrderModel
              .getPaymentTransactions()) {
        for(final PaymentTransactionEntryModel paymentTransactionEntryModel : paymentTransactionModel.getEntries()) {
          if(paymentTransactionEntryModel.getType().getCode().equalsIgnoreCase(BltaxapiConstants.CAPTURE)){
            return false;
          }
        }

      }
    }
    return true;
  }

  /**
   * This method created to get trim the product code , if it more than 50 characters
   * @param code code
   * @return String
   */
  private String getTrimmedProductCodeFromProduct(final String code) {
    final AtomicReference<String> productCode = new AtomicReference<>(code);
    final AtomicInteger  maxCharacter = new AtomicInteger(0);
    if(Objects.nonNull(Config.getInt(BltaxapiConstants.MAX_CHARACTER , 50))) {
      maxCharacter.set(Config.getInt(BltaxapiConstants.MAX_CHARACTER, 50));
    }
    if(productCode.get().length() > maxCharacter.get()){
      productCode.set(productCode.get().substring(0 , maxCharacter.get()));
    }
    return productCode.get();
  }

  /**
   * This method created to get values from property
   * @param key key
   * @return String
   */
  private String getValuesFromProperty(final String key) {
    final AtomicReference<String> value = new AtomicReference<>(StringUtils.EMPTY);
    if(StringUtils.isNotBlank(Config.getParameter(key))) {
      value.set(Config.getParameter(key));
    }
    return value.get();
  }

  public SessionService getSessionService() {
    return sessionService;
  }

  public void setSessionService(SessionService sessionService) {
    this.sessionService = sessionService;
  }

  private RentalDateDto getRentalDatesFromSession()
  {
    final Map<String, String> rentalDate = getSessionService().getAttribute(BltaxapiConstants.SELECTED_DATE_MAP);
    final Map<String, String> selectedDuration = getSessionService().getAttribute(BltaxapiConstants.SELECTED_DURATION_MAP);
    if (null != rentalDate)
    {
      final RentalDateDto date = new RentalDateDto();
      final String startDate = rentalDate.get(BltaxapiConstants.START_DATE);
      final String endDate = rentalDate.get(BltaxapiConstants.END_DATE);
      final String selectedDurationDays = selectedDuration.get(BltaxapiConstants.SELECTED_DURATION);
      if (null != startDate && null != endDate)
      {
        date.setSelectedFromDate(startDate);
        date.setSelectedToDate(endDate);
        date.setNumberOfDays(String.valueOf(
                ChronoUnit.DAYS.between(BlTaxAPIUtils.convertStringDateToLocalDate(startDate, BltaxapiConstants.DATE_FORMAT),
                        BlTaxAPIUtils.convertStringDateToLocalDate(endDate, BltaxapiConstants.DATE_FORMAT))));
        if(org.apache.commons.lang.StringUtils.isNotBlank(selectedDurationDays)) {
          date.setSelectedDays(selectedDurationDays);
        }
        return date;
      }
    }
    return null;
  }

  /**
   * @return the configurationService
   */
  public ConfigurationService getConfigurationService()
  {
    return configurationService;
  }

  /**
   * @param configurationService the configurationService to set
   */
  public void setConfigurationService(ConfigurationService configurationService)
  {
    this.configurationService = configurationService;
  }

}
