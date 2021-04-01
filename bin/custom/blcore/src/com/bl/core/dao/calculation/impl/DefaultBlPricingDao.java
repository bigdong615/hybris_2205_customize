package com.bl.core.dao.calculation.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.core.dao.calculation.BlPricingDao;
import com.bl.core.enums.DurationEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlPricingLogicModel;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.europe1.model.PriceRowModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.util.Collections;
import java.util.List;
/**
 * This class has implementation of methods
 * related to retrieval of pricing logic and prices
 *
 * @author Ritika
 */
public class DefaultBlPricingDao  implements BlPricingDao {
  private FlexibleSearchService flexibleSearchService;
  private static final String FIND_BL_PRICING_BY_PRODUCT_TYPE = "SELECT {" + BlPricingLogicModel.PK + "} FROM {" + BlPricingLogicModel._TYPECODE + "} WHERE {" + BlPricingLogicModel.PRODUCTTYPE+ "} =?productType";//NOSONAR
  private static final String FIND_PRICE_BY_PRODUCT = "SELECT {" + PriceRowModel.PK + "} FROM {" + PriceRowModel._TYPECODE + "} WHERE {" + PriceRowModel.DURATION+ "} =?duration" +" and {" + PriceRowModel.PRODUCT + "} =?product" ;//NOSONAR


  @Override
  public List<BlPricingLogicModel> getBlPricingByProductType(ProductTypeEnum productType) {
      validateParameterNotNull(productType, "ProductType must not be null");
      final FlexibleSearchQuery query = new FlexibleSearchQuery(FIND_BL_PRICING_BY_PRODUCT_TYPE);
      query.addQueryParameter("productType",productType);
      final SearchResult<BlPricingLogicModel> result = getFlexibleSearchService().search(query);
      return result.getCount() > 0 ? result.getResult() : Collections.emptyList();
  }

  @Override
  public PriceRowModel getPriceRowByDuration(DurationEnum duration, BlProductModel blProduct){
      validateParameterNotNull(duration, "duration must not be null");
      validateParameterNotNull(blProduct, "BlProduct must not be null");
      final FlexibleSearchQuery query = new FlexibleSearchQuery(FIND_PRICE_BY_PRODUCT);
      query.addQueryParameter("duration",duration);
      query.addQueryParameter("product",blProduct);
      final SearchResult<PriceRowModel> result = getFlexibleSearchService().search(query);
      return result.getCount()> 0 && result.getResult() != null ? result.getResult().get(0) : null ;
  }

    /**
     * @return the flexibleSearchService
     */
    public FlexibleSearchService getFlexibleSearchService(){

      return flexibleSearchService;
    }

    /**
     * @param flexibleSearchService the flexibleSearchService to set
     */
    public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService){

      this.flexibleSearchService = flexibleSearchService;
    }

  }
