package com.bl.tax.populators;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.bl.tax.Addresses;
import com.bl.tax.AddressesData;
import com.bl.tax.TaxLine;
import com.bl.tax.TaxRequestData;
import com.bl.tax.constants.BltaxapiConstants;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.time.DateFormatUtils;

public class BlTaxServiceRequestPopulator implements Populator<AbstractOrderModel, TaxRequestData> {

  private ProductService productService;
  private BlDatePickerService blDatePickerService;

  @Override
  public void populate(final AbstractOrderModel abstractOrder, final TaxRequestData taxRequest)
      throws ConversionException{
    taxRequest.setCompanyCode(BltaxapiConstants.COMPANY_CODE);
    taxRequest.setCode(abstractOrder.getCode());
    taxRequest.setType(BltaxapiConstants.SALESORDER); // As of now using by default SalesOrder
    setOrderDateToRequest(abstractOrder , taxRequest);
    taxRequest.setCustomerCode("bltestuser"); //customer code
    taxRequest.setSalesPersonCode(null); // sales person code optional
    taxRequest.setOriginCode(BltaxapiConstants.ORIGIN);
    taxRequest.setDestinationCode(BltaxapiConstants.DESTINATION);
    try {
      setTaxCommittedToRequest(abstractOrder, taxRequest);
    } catch (ParseException e)
    {
      //BlLogger.logMessage("");
    }
    taxRequest.setAddresses(createAddressesForOrderTax(abstractOrder));
    final List<String> taxCode = new ArrayList<>();
    if(null != abstractOrder.getDeliveryMode()) {
      if ("pickup".equalsIgnoreCase(abstractOrder.getDeliveryMode().getCode())) {
        taxCode.add(abstractOrder.getEntries().stream().findAny().get().getProduct() instanceof BlSerialProductModel ?
            BltaxapiConstants.SHIPPING_SALES_TAX_CODE :BltaxapiConstants.RENTAL_TAX_CODE);
      }
    }
    if(CollectionUtils.isNotEmpty(abstractOrder.getAppliedCouponCodes())) {
      taxCode.add(BltaxapiConstants.DISCOUNT_TAX_CODE);
    }
    taxRequest.setTaxCode(taxCode);
    taxRequest.setLines(createdTaxLineForRequest(abstractOrder, taxRequest.getIsTaxExempt()));
    taxRequest.setCurrencyCode(abstractOrder.getCurrency().getIsocode());

  }

  private List<TaxLine> createdTaxLineForRequest(final AbstractOrderModel abstractOrder , final boolean value) {

    final List<TaxLine> taxLines = new ArrayList<>();
    for (final AbstractOrderEntryModel entry : abstractOrder.getEntries())
    {
      final TaxLine taxLine = new TaxLine();
      taxLine.setQuantity(entry.getQuantity().intValue());
      taxLine.setNumber(entry.getEntryNumber().toString());
      taxLine.setItemCode(entry.getProduct().getCode());
      taxLine.setAmount(value ? 0d : entry.getTotalPrice());
      taxLine.setDescription(entry.getInfo());
      taxLine.setTaxCode(setProductTaxCode(entry));
      taxLine.setCompletedOrderCount(2);
      taxLines.add(taxLine);
    }
    return taxLines;
  }

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
      shipTo.setState(deliveryAddressForOrder.getDistrict());
      shipTo.setRegion(null != deliveryAddressForOrder.getRegion()? deliveryAddressForOrder.getRegion().getIsocode() : null);
      shipTo.setCountry(null != deliveryAddressForOrder.getCountry()? deliveryAddressForOrder.getCountry().getIsocode() : null);
      shipTo.setPostalCode(deliveryAddressForOrder.getPostalcode());
      shipTo.setPhone(deliveryAddressForOrder.getPhone1());
      shipTo.setEmail(deliveryAddressForOrder.getEmail());
      shipTo.setIsDefault(true);
      shipTo.setAddressType("");
      addresses.setShipTo(shipTo);
    }
       final AddressesData shipFrom = new AddressesData();
       shipFrom.setLine1("1664 Industrial Rd");
       shipFrom.setCity("San Carlos");
       shipFrom.setRegion("CA");
       shipFrom.setCountry("US");
       shipFrom.setPostalCode("94070");
       addresses.setShipFrom(shipFrom);
       return addresses;
  }


  private String setProductTaxCode(final AbstractOrderEntryModel entry) { // Have another way to check eg : isRentalCart
      return entry.getProduct() instanceof BlSerialProductModel ? BltaxapiConstants.SALES_TAX_CODE
          : BltaxapiConstants.RENTAL_TAX_CODE;
    }

  private void setOrderDateToRequest(final AbstractOrderModel abstractOrder , final TaxRequestData taxRequest) {
   taxRequest.setDate(DateFormatUtils.format(abstractOrder.getDate(), BltaxapiConstants.DATE_FORMAT));
  }



  private void setTaxCommittedToRequest(final AbstractOrderModel abstractOrder , final TaxRequestData taxRequest)
      throws ParseException {
        /// Needs to modify condition once able to get date for rental and expiry date
        final boolean isTaxExempt = abstractOrder.getUser().getIsTaxExempt();
        if(isTaxExempt) {
          taxRequest.setTaxExemptState(abstractOrder.getDeliveryAddress().getDistrict());
          final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
          final String endDate = rentalDateDto.getSelectedToDate();
          final Date endDay = new SimpleDateFormat(BltaxapiConstants.DATE_FORMAT).parse(endDate);
          if (null != abstractOrder.getUser().getTaxExemptExpiry() && endDay
                .before(abstractOrder.getUser().getTaxExemptExpiry())) {
              taxRequest.setTaxExemptExpiry(abstractOrder.getUser().getTaxExemptExpiry());
              taxRequest.setTaxExemptNumber("ExcemptNumber123");
              taxRequest.setIsTaxExempt(isTaxExempt);
          }
          else {
            taxRequest.setIsTaxExempt(false);
          }
        }
        else {
          taxRequest.setIsTaxExempt(false);
        }
  }

  public BlDatePickerService getBlDatePickerService() {
    return blDatePickerService;
  }

  public void setBlDatePickerService(BlDatePickerService blDatePickerService) {
    this.blDatePickerService = blDatePickerService;
  }


  public ProductService getProductService(){
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }

}
