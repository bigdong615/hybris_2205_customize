package com.bl.core.service;

import de.hybris.platform.core.model.product.ProductModel;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Date;

/**
 * BlBackOfficePriceService interface is used to get product dynamic price
 *
 * @author Kalyan Kumar
 */
public interface BlBackOfficePriceService {

  /**
   * getProductPrice is used to get product dynamic price based on given dates
   *
   * @param productModel used to get product
   * @param returnDate   used to get rental return date
   * @param arrivalDate  used to get rental start date
   * @return BigDecimal
   * @throws ParseException it can throw parse exception while converting dates
   */
  BigDecimal getProductPrice(ProductModel productModel, Date arrivalDate, Date returnDate)
      throws ParseException;
}
