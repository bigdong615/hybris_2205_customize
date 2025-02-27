package com.bl.facades.populators;

import de.hybris.platform.acceleratorfacades.order.data.PriceRangeData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commerceservices.product.data.SolrFirstVariantCategoryEntryData;
import de.hybris.platform.commerceservices.search.resultdata.SearchResultValueData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.provider.entity.SolrPriceRange;
import de.hybris.platform.commerceservices.search.solrfacetsearch.provider.entity.SolrPriceRangeEntry;
import de.hybris.platform.commerceservices.search.solrfacetsearch.provider.impl.SolrFirstVariantCategoryManager;
import java.util.List;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Required;

public class BlSearchResultVariantProductPopulator extends BlSearchResultProductPopulator{
  public static final String MULTIDIMENSIONAL = "multidimensional";
  public static final String PRICE_RANGE = "priceRange";
  public static final String FIRST_CATEGORY_NAME_LIST = "firstCategoryNameList";

  private SolrFirstVariantCategoryManager categoryManager;

  @Override
  public void populate(final SearchResultValueData source, final ProductData target)
  {
    super.populate(source, target);
    if (source.getValues() != null)
    {
      target.setMultidimensional((Boolean) source.getValues().get(MULTIDIMENSIONAL));
      setPriceRange(source, target);
      setFirstCategoryNameList(source, target);
    }
  }

  /**
   * Set price range for the {@link ProductData}. If there is no priceRange in the source, no {@link PriceRangeData}
   * will be set in the target.
   *
   * @param source
   *           The {@link SearchResultValueData} containing the priceRange.
   * @param target
   *           The {@link ProductData} to be modified.
   */
  protected void setPriceRange(final SearchResultValueData source, final ProductData target)
  {
    final PriceRangeData priceRange = new PriceRangeData();
    final String priceRangeValue = (String) source.getValues().get(PRICE_RANGE);
    if (StringUtils.isNotEmpty(priceRangeValue))
    {
      final SolrPriceRange solrPriceRange = SolrPriceRange.buildSolrPriceRangePairFromProperty(priceRangeValue);
      if (solrPriceRange != null)
      {
        priceRange.setMinPrice(createPriceData(solrPriceRange.getLower()));
        priceRange.setMaxPrice(createPriceData(solrPriceRange.getHigher()));
      }
    }

    target.setPriceRange(priceRange);
  }

  protected PriceData createPriceData(final SolrPriceRangeEntry priceInfo)
  {
    return getPriceDataFactory().create(PriceDataType.FROM, priceInfo.getValue(), priceInfo.getCurrencyIso());
  }

  /**
   * Set list of first categories for the {@link ProductData}.
   *
   * @param source
   *           The {@link SearchResultValueData} containing the priceRange.
   * @param target
   *           The {@link ProductData} to be modified.
   */
  protected void setFirstCategoryNameList(final SearchResultValueData source, final ProductData target)
  {
    final String categoryListSolr = (String) source.getValues().get(FIRST_CATEGORY_NAME_LIST);
    if (StringUtils.isNotEmpty(categoryListSolr))
    {
      final List<SolrFirstVariantCategoryEntryData> variantCategoryList = categoryManager
          .buildFirstVariantCategoryListFromSolrProperty(categoryListSolr);
      target.setFirstCategoryNameList(variantCategoryList);
    }
  }

  @Required
  public void setCategoryManager(final SolrFirstVariantCategoryManager categoryManager)
  {
    this.categoryManager = categoryManager;
  }


}
