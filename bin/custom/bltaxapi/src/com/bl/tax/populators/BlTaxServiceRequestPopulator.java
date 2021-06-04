package com.bl.tax.populators;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.model.BlSerialProductModel;
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
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This populator created for preparing avalara tax request
 * @author Manikandan
 */
public class BlTaxServiceRequestPopulator implements Populator<AbstractOrderModel, TaxRequestData> {

  private static final Logger LOG = Logger.getLogger(BlTaxServiceRequestPopulator.class);

  private ProductService productService;
  private BlDatePickerService blDatePickerService;
  private DefaultWarehouseService defaultWarehouseService;

  /*
   * this method created to prepare taxrequest from abstractOrderModel
   */
  @Override
  public void populate(final AbstractOrderModel abstractOrder, final TaxRequestData taxRequest)
      throws ConversionException{
    taxRequest.setCompanyCode(BltaxapiConstants.COMPANY_CODE);
    taxRequest.setCode(abstractOrder.getCode());
    taxRequest.setType(BltaxapiConstants.SALESORDER);
    setOrderDateToRequest(taxRequest);
    taxRequest.setCustomerCode(abstractOrder.getUser().getUid());
    taxRequest.setSalesPersonCode(null);
    taxRequest.setOriginCode(BltaxapiConstants.ORIGIN);
    taxRequest.setDestinationCode(BltaxapiConstants.DESTINATION);
    try {
      setTaxCommittedToRequest(abstractOrder, taxRequest);
    } catch (ParseException e) {
      BlLogger.logMessage(LOG , Level.ERROR , "Error while setting request for tax commit " , e);
    }
    taxRequest.setAddresses(createAddressesForOrderTax(abstractOrder));
    taxRequest.setLines(createdTaxLineForRequest(abstractOrder));
    setShippingAndDiscountLineForRequest(taxRequest , abstractOrder);
    taxRequest.setCurrencyCode(abstractOrder.getCurrency().getIsocode());
  }

  /**
   * this method created to preparing line item request
   * @param abstractOrder abstractOrder
   * @return  List<TaxLine>
   */
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
      shipTo.setState(deliveryAddressForOrder.getRegion().getName());
      shipTo.setRegion(null != deliveryAddressForOrder.getRegion()? deliveryAddressForOrder.getRegion().getIsocode() : null);
      shipTo.setCountry(null != deliveryAddressForOrder.getCountry()? deliveryAddressForOrder.getCountry().getIsocode() : null);
      shipTo.setPostalCode(deliveryAddressForOrder.getPostalcode());
      shipTo.setPhone(deliveryAddressForOrder.getPhone1());
      shipTo.setEmail(deliveryAddressForOrder.getEmail());
      shipTo.setAddressType(BltaxapiConstants.EMPTY_STRING);
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

  /**
   * To set product tax code as per product type
   */

  private String setProductTaxCode(final AbstractOrderEntryModel entry) {
      return entry.getProduct() instanceof BlSerialProductModel ? BltaxapiConstants.SALES_TAX_CODE
          : BltaxapiConstants.RENTAL_TAX_CODE;
    }

  /**
   * To set orderDate to request
   */
  private void setOrderDateToRequest(final TaxRequestData taxRequest) {
   taxRequest.setDate(DateFormatUtils.format(new Date(), BltaxapiConstants.DATE_FORMAT));
  }


  /**
   * Validate and set tax excemption details to request
   */
  private void setTaxCommittedToRequest(final AbstractOrderModel abstractOrder , final TaxRequestData taxRequest) throws ParseException {
        if(BooleanUtils.isTrue(abstractOrder.getUser().getIsTaxExempt())) {
          final String addressState = abstractOrder.getDeliveryAddress().getRegion().getName();
            taxRequest.setTaxExemptState(addressState.equalsIgnoreCase(abstractOrder.getUser().getTaxExemptState()) ? addressState : null);
          final Date endDay = getDateForRequest(abstractOrder);
          if (null != abstractOrder.getUser().getTaxExemptExpiry() && null != endDay && endDay
                .before(abstractOrder.getUser().getTaxExemptExpiry()) && null != taxRequest.getTaxExemptState()) {
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
    final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();

    if (null != rentalDateDto && null != rentalDateDto.getSelectedToDate()) {
        return new SimpleDateFormat(BltaxapiConstants.DATE_FORMAT).parse(rentalDateDto.getSelectedToDate());
      }

    if (abstractOrder.getEntries().stream()
        .anyMatch(abstractOrderEntryModel ->
            abstractOrderEntryModel.getProduct() instanceof BlSerialProductModel)) {
      return abstractOrder.getDate();
    }
    return abstractOrder.getDate();
  }

  /**
   * this method created to prepare shipping and discount tax for orders
   */
  private void setShippingAndDiscountLineForRequest(final TaxRequestData taxRequest , final AbstractOrderModel abstractOrder) {
    final List<TaxLine> taxLines = taxRequest.getLines();
    final TaxLine shippingTaxLine =  new TaxLine();
    final TaxLine discountTaxLine =  new TaxLine();
    if(null != abstractOrder.getDeliveryMode()) {
      shippingTaxLine.setQuantity(BltaxapiConstants.QTY);
      shippingTaxLine.setNumber(taxRequest.getLines().get(taxRequest.getLines().size() -1).getNumber() + 1);
      shippingTaxLine.setItemCode(BltaxapiConstants.SHIPPING);
      shippingTaxLine.setAmount(abstractOrder.getDeliveryCost());
      shippingTaxLine.setTaxCode(abstractOrder.getEntries().stream().findAny().get().getProduct() instanceof BlSerialProductModel
          ? BltaxapiConstants.SHIPPING_SALES_TAX_CODE : BltaxapiConstants.RENTAL_TAX_CODE);
      taxLines.add(shippingTaxLine);
    }
    if(CollectionUtils.isNotEmpty(abstractOrder.getAppliedCouponCodes()))
    {
      discountTaxLine.setQuantity(BltaxapiConstants.QTY);
      discountTaxLine.setNumber(null != shippingTaxLine.getNumber() ? shippingTaxLine.getNumber() + 1 : 1);
      Double discountAmount = abstractOrder.getGiftCardAmount() + abstractOrder.getTotalDiscounts();
      discountTaxLine.setAmount(- discountAmount);
      discountTaxLine.setTaxCode(BltaxapiConstants.DISCOUNT_TAX_CODE);
      taxLines.add(discountTaxLine);
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
