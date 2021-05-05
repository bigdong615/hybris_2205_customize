package com.bl.tax.populators;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.model.BlSerialProductModel;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.tax.Addresses;
import com.bl.tax.AddressesData;
import com.bl.tax.TaxLine;
import com.bl.tax.TaxRequestData;
import com.bl.tax.constants.BltaxapiConstants;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.impl.DefaultWarehouseService;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;

public class BlTaxServiceRequestPopulator implements Populator<AbstractOrderModel, TaxRequestData> {

  private ProductService productService;
  private BlDatePickerService blDatePickerService;
  private DefaultWarehouseService defaultWarehouseService;

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
    e.printStackTrace();
    }
    taxRequest.setAddresses(createAddressesForOrderTax(abstractOrder));
    taxRequest.setLines(createdTaxLineForRequest(abstractOrder));
    setShippingAndDiscountLineForRequest(taxRequest , abstractOrder);
    taxRequest.setCurrencyCode(abstractOrder.getCurrency().getIsocode());

  }

  private List<TaxLine> createdTaxLineForRequest(final AbstractOrderModel abstractOrder) {

    final List<TaxLine> taxLines = new ArrayList<>();
    for (final AbstractOrderEntryModel entry : abstractOrder.getEntries())
    {
      final TaxLine taxLine = new TaxLine();
      taxLine.setQuantity(entry.getQuantity().intValue());
      taxLine.setNumber(entry.getEntryNumber());
      taxLine.setItemCode(entry.getProduct().getCode());
      Double value = 0.0;
      if(BooleanUtils.isTrue(entry.getGearGuardProFullWaiverSelected())) {
         value = entry.getGearGuardProFullWaiverPrice();
      } else if(BooleanUtils.isTrue(entry.getGearGuardWaiverSelected())) {
        value = entry.getGearGuardWaiverPrice();
      }
      taxLine.setAmount(entry.getTotalPrice() + value);
      taxLine.setDescription(entry.getInfo());
      taxLine.setTaxCode(setProductTaxCode(entry));
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
      List<WarehouseModel> warehouseModels =  getDefaultWarehouseService().getDefWarehouse();
       for(int i=0; i< warehouseModels.size() ;i++) {
         if(null != warehouseModels.get(i).getOriginAddress())
         {
           final AddressModel addressModel = warehouseModels.get(i).getOriginAddress();
           shipFrom.setLine1(addressModel.getLine1());
           shipFrom.setCity(addressModel.getTown());
           shipFrom.setRegion("CA");
           shipFrom.setCountry(addressModel.getCountry().getIsocode());
           shipFrom.setPostalCode(addressModel.getPostalcode());
           addresses.setShipFrom(shipFrom);
         }
       }

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
              taxRequest.setExemptionNo(StringUtils.isNotBlank(abstractOrder.getUser().getTaxExemptNumber()) ? abstractOrder.getUser().getTaxExemptNumber() : null);
              taxRequest.setCommit(isTaxExempt);
          }
          else {
            taxRequest.setCommit(false);
          }
        }
        else {
          taxRequest.setCommit(false);
        }
  }


  private void setShippingAndDiscountLineForRequest(final TaxRequestData taxRequest , final AbstractOrderModel abstractOrder) {
    final List<TaxLine> taxLines = taxRequest.getLines();
    final TaxLine taxLine =  new TaxLine();
    final TaxLine taxLine1 =  new TaxLine();
    if(abstractOrder.getDeliveryMode() instanceof ZoneDeliveryModeModel) {
      taxLine.setQuantity(1);
      taxLine.setNumber(taxRequest.getLines().get(taxRequest.getLines().size()-1).getNumber() + 1);
      taxLine.setItemCode("shipping");
      taxLine.setAmount(abstractOrder.getDeliveryCost());
      taxLine.setTaxCode(abstractOrder.getEntries().stream().findAny().get().getProduct() instanceof BlSerialProductModel
          ? BltaxapiConstants.SHIPPING_SALES_TAX_CODE : BltaxapiConstants.RENTAL_TAX_CODE);
      taxLines.add(taxLine);
    }
    boolean allowed = false;
    if(CollectionUtils.isNotEmpty(abstractOrder.getAppliedCouponCodes()) || allowed)
    {
      taxLine1.setQuantity(1);
      taxLine1.setNumber(null != taxLine.getNumber() ? taxLine.getNumber() + 1 : 1);
      taxLine1.setAmount(-10.00);
      taxLine1.setTaxCode(BltaxapiConstants.DISCOUNT_TAX_CODE);
      taxLines.add(taxLine1);
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

  public DefaultWarehouseService getDefaultWarehouseService() {
    return defaultWarehouseService;
  }

  public void setDefaultWarehouseService(
      DefaultWarehouseService defaultWarehouseService) {
    this.defaultWarehouseService = defaultWarehouseService;
  }

}
