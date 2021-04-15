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
  private PriceData finalSalePrice;
  private PriceData finalIncentivizedPrice;
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

  public void setSerialId(final String serialId) {
    this.serialId = serialId;
  }

  public String getSerialId() {
    return serialId;
  }

/**
 * @return the finalSalePrice
 */
public PriceData getFinalSalePrice()
{
	return finalSalePrice;
}

/**
 * @param finalSalePrice the finalSalePrice to set
 */
public void setFinalSalePrice(PriceData finalSalePrice)
{
	this.finalSalePrice = finalSalePrice;
}

/**
 * @return the finalIncentivizedPrice
 */
public PriceData getFinalIncentivizedPrice()
{
	return finalIncentivizedPrice;
}

/**
 * @param finalIncentivizedPrice the finalIncentivizedPrice to set
 */
public void setFinalIncentivizedPrice(PriceData finalIncentivizedPrice)
{
	this.finalIncentivizedPrice = finalIncentivizedPrice;
}

}
