package com.bl.tax.populators;

import com.bl.core.model.BlProductModel;
import com.bl.tax.Addresses;
import com.bl.tax.ShipFrom;
import com.bl.tax.ShipTo;
import com.bl.tax.TaxLine;
import com.bl.tax.constants.BltaxapiConstants;
import com.bl.tax.data.TaxRequestData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.util.ArrayList;
import java.util.List;

public class BlTaxServiceRequestPopulator implements Populator<AbstractOrderModel, TaxRequestData> {

  private ProductService productService;

  @Override
  public void populate(AbstractOrderModel abstractOrder, TaxRequestData taxRequest)
      throws ConversionException {
    taxRequest.setCode(abstractOrder.getCode());
    taxRequest.setType(null != abstractOrder.getTaxType() ? abstractOrder.getTaxType().getCode() : BltaxapiConstants.SALESORDER);
    taxRequest.setDate(abstractOrder.getDate());
    taxRequest.setCustomerCode(abstractOrder.getUser().getUid()); //customer code
    taxRequest.setSalesPersonCode("TestUser"); // sales person code optional
    taxRequest.setOriginCode("Origin");
    taxRequest.setDestinationCode("Dest");
    taxRequest.setExemptionNo("12345678");
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
      taxLines.add(taxLine);
    }
    taxRequest.setLines(taxLines);
  }


  private Addresses createAddressesForOrderTax(final AbstractOrderModel abstractOrder)
  {
    final Addresses addresses = new Addresses();
    final AddressModel deliveryAddressForOrder = abstractOrder.getDeliveryAddress();
    if (deliveryAddressForOrder != null)
    {
      final ShipTo shipTo = new ShipTo();
      shipTo.setLine1(deliveryAddressForOrder.getLine1());
      shipTo.setCity(deliveryAddressForOrder.getTown());
      shipTo.setRegion(deliveryAddressForOrder.getCountry() != null ? deliveryAddressForOrder.getCountry().getIsocode() : null);
      shipTo.setCountry(deliveryAddressForOrder.getCountry() != null ? deliveryAddressForOrder.getCountry().getIsocode() : null);
      shipTo.setPostalCode(deliveryAddressForOrder.getPostalcode());
      addresses.setShipTo(shipTo);
    }
       ShipFrom shipFrom = new ShipFrom();
       shipFrom.setLine1("1664 Industrial Rd");
       shipFrom.setCity("San Carlos");
       shipFrom.setRegion("CA");
       shipFrom.setCountry("United States");
       shipFrom.setPostalCode("94070");
       addresses.setShipFrom(shipFrom);
       return addresses;
  }


  private String setProductTaxCode(final AbstractOrderEntryModel entry) {
    final BlProductModel productModel = (BlProductModel) getProductService().getProductForCode(entry.getProduct().getCode());
    return productModel.getForSale() ? BltaxapiConstants.SALES_TAX_CODE: BltaxapiConstants.RENTAL_TAX_CODE;
  }

  public ProductService getProductService() {
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }

}
