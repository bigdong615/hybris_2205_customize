package com.bl.facades.product;

import de.hybris.platform.commercefacades.product.data.PriceData;
import java.io.Serializable;

/*
 * This DTO created for serial product
 * @author Vijay Vishwakarma
 */
public class SerialProductData implements Comparable<SerialProductData>, Serializable {

  private static final long serialVersionUID = 1L;

  private double conditionRating;
  private PriceData serialProductPrice;
  private String serialId;

  @Override
  public int compareTo(SerialProductData o) { // NOSONAR
    if (o == null) {
      return -1;
    }
    int c = Double.valueOf(conditionRating).compareTo(o.conditionRating);
    if (c != 0) {
      return c;
    }
    return (int) o.getConditionRating();
  }

  public void setConditionRating(final double conditionRating) {
    this.conditionRating = conditionRating;
  }

  public double getConditionRating() {
    return conditionRating;
  }

  public void setSerialProductPrice(final PriceData serialProductPrice) {
    this.serialProductPrice = serialProductPrice;
  }

  public PriceData getSerialProductPrice() {
    return serialProductPrice;
  }

  public void setSerialId(final String serialId) {
    this.serialId = serialId;
  }

  public String getSerialId() {
    return serialId;
  }

}
