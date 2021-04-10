package com.bl.facades.product;

import de.hybris.platform.commercefacades.product.data.PriceData;
import java.io.Serializable;

/*
 * This DTO created to take serial product related information.
 * @author Vijay Vishwakarma
 */
public class SerialProductData implements Comparable<SerialProductData>, Serializable {

  private static final long serialVersionUID = 1L;

  private double conditionRating;
  private PriceData serialProductPrice;
  private String serialId;

  @Override
  public int compareTo(final SerialProductData serialProductData) { // NOSONAR
    if (serialProductData == null) {
      return -1;
    }
    final int rating = Double.valueOf(conditionRating).compareTo(serialProductData.conditionRating);
    if (rating != 0) {
      return rating;
    }
    return (int) serialProductData.getConditionRating();
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
