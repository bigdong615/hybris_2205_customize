package com.bl.tax.populators;

import com.bl.core.model.BlProductModel;
import com.bl.tax.AddressData;
import com.bl.tax.Addresses;
import com.bl.tax.TaxLine;
import com.bl.tax.constants.BltaxapiConstants;
import com.bl.tax.data.TaxRequestData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.BooleanUtils;

public class BlTaxServiceRequestPopulator implements Populator<AbstractOrderModel, TaxRequestData> {

  private ProductService productService;

  @Override
  public void populate(AbstractOrderModel abstractOrder, TaxRequestData taxRequest)
      throws ConversionException {
    taxRequest.setCompanyCode(BltaxapiConstants.COMPANY_CODE);
    taxRequest.setCode(abstractOrder.getCode());
    taxRequest.setType(BltaxapiConstants.SALESORDER); // As of now using by default SalesOrder
    setOrderDateToRequest(abstractOrder , taxRequest);
    taxRequest.setCustomerCode("Ecommerce"); //customer code
    taxRequest.setSalesPersonCode(null); // sales person code optional
    taxRequest.setOriginCode(BltaxapiConstants.ORIGIN);
    taxRequest.setDestinationCode(BltaxapiConstants.DESTINATION);
    setTaxCommittedToRequest(abstractOrder, taxRequest);
    taxRequest.setLines(createdTaxLineForRequest(abstractOrder));

  }

  private List<TaxLine> createdTaxLineForRequest(final AbstractOrderModel abstractOrder) {

    List<TaxLine> taxLines = new ArrayList<>();
    for (final AbstractOrderEntryModel entry : abstractOrder.getEntries())
    {
      final TaxLine taxLine = new TaxLine();
      taxLine.setQuantity(entry.getQuantity().intValue());
      taxLine.setNumber(entry.getEntryNumber().toString());
      taxLine.setItemCode(entry.getProduct().getCode());
      taxLine.setAmount(entry.getTotalPrice());
      taxLine.setDescription(entry.getInfo());
      taxLine.setAddresses(createAddressesForOrderTax(entry.getOrder()));
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
      final AddressData shipTo = new AddressData();
      shipTo.setFirstName(deliveryAddressForOrder.getFirstname());
      shipTo.setLastName(deliveryAddressForOrder.getLastname());
      shipTo.setLine1(deliveryAddressForOrder.getLine1());
      shipTo.setLine2(deliveryAddressForOrder.getLine2());
      shipTo.setCity(deliveryAddressForOrder.getTown());
      shipTo.setState(deliveryAddressForOrder.getDistrict());
      shipTo.setRegion(deliveryAddressForOrder.getCountry() != null ? deliveryAddressForOrder.getCountry().getIsocode() : null);
      shipTo.setCountry(deliveryAddressForOrder.getCountry() != null ? deliveryAddressForOrder.getCountry().getIsocode() : null);
      shipTo.setPostalCode(deliveryAddressForOrder.getPostalcode());
      shipTo.setPhone(deliveryAddressForOrder.getPhone1());
      shipTo.setEmail(deliveryAddressForOrder.getEmail());
      shipTo.setIsDefault(true);
      shipTo.setAddressType("");
      addresses.setShipTo(shipTo);
    }
       final AddressData shipFrom = new AddressData();
       shipFrom.setLine1("1664 Industrial Rd");
       shipFrom.setCity("San Carlos");
       shipFrom.setRegion("CA");
       shipFrom.setCountry("US");
       shipFrom.setPostalCode("94070");
       addresses.setShipFrom(shipFrom);
       return addresses;
  }


  private String setProductTaxCode(final AbstractOrderEntryModel entry) {
    final BlProductModel productModel = (BlProductModel) getProductService().getProductForCode(entry.getProduct().getCode());
    return productModel.getForSale() ? BltaxapiConstants.SALES_TAX_CODE: BltaxapiConstants.RENTAL_TAX_CODE;
  }

  private void setOrderDateToRequest(final AbstractOrderModel abstractOrder , final TaxRequestData taxRequest) {
      SimpleDateFormat simpleDateFormat = new SimpleDateFormat(BltaxapiConstants.DATE_FORMAT);
    try {
      String strDate = simpleDateFormat.format(abstractOrder.getDate());
      taxRequest.setDate(simpleDateFormat.parse(strDate));
    }
    catch (Exception e) {
   //
    }
  }

  private void setTaxCommittedToRequest(final AbstractOrderModel abstractOrder , final TaxRequestData taxRequest) {
        /// Needs to modify condition once able to get date for rental and expiry date
        taxRequest.setIsTaxExempt(abstractOrder.getIsTaxExempt());
        taxRequest.setTaxExemptExpiry(BooleanUtils.isTrue(abstractOrder.getIsTaxExempt()) ? abstractOrder.getExpirationTime() : null);
        taxRequest.setTaxExemptNumber("DummyNumber");
  }

  public ProductService getProductService(){
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }

}
