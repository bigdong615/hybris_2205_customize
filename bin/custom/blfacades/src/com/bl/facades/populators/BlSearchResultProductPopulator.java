package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.facades.productreference.BlProductFacade;
import com.bl.logging.BlLogger;
import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.catalog.model.classification.ClassAttributeAssignmentModel;
import de.hybris.platform.classification.features.Feature;
import de.hybris.platform.classification.features.FeatureList;
import de.hybris.platform.classification.features.FeatureValue;
import de.hybris.platform.classification.features.LocalizedFeature;
import de.hybris.platform.classification.features.UnlocalizedFeature;
import de.hybris.platform.commercefacades.product.ImageFormatMapping;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ImageDataType;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.product.data.PromotionData;
import de.hybris.platform.commercefacades.product.data.StockData;
import de.hybris.platform.commerceservices.search.resultdata.SearchResultValueData;
import de.hybris.platform.commerceservices.url.UrlResolver;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.stocknotificationfacades.StockNotificationFacade;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;


/**
 * This class is completly overriden for adding custom logics on populator
 * @author Manikandan
 */
public class BlSearchResultProductPopulator implements Populator<SearchResultValueData, ProductData> {

	private static final Logger LOG = Logger.getLogger(BlSearchResultProductPopulator.class);

  private StockNotificationFacade stockNotificationFacade;
  private ImageFormatMapping imageFormatMapping;
  private PriceDataFactory priceDataFactory;
  private UrlResolver<ProductData> productDataUrlResolver;
  private Populator<FeatureList, ProductData> productFeatureListPopulator;
  private ProductService productService;
  private BlProductFacade blProductFacade;
  private CommonI18NService commonI18NService;
  private Converter<ProductModel, StockData> stockConverter;
  private Converter<StockLevelStatus, StockData> stockLevelStatusConverter;
  private BlCommercePriceService commercePriceService;

  /**
   * this method is created for populating values from source to target
   * @param source the source object
   * @param target the target to fill
   */
  @Override
  public void populate(final SearchResultValueData source, final ProductData target) {
    Assert.notNull(source, "Parameter source cannot be null.");
    Assert.notNull(target, "Parameter target cannot be null.");
    // Pull the values directly from the SearchResult object
    target.setCode(this.<String>getValue(source, "code"));
    target.setName(this.<String>getValue(source, "name"));
    target.setManufacturer(this.<String>getValue(source, "manufacturerName"));
    target.setDescription(this.<String>getValue(source, "description"));
    target.setSummary(this.<String>getValue(source, "summary"));
    target.setAverageRating(this.<Double>getValue(source, "reviewAvgRating"));
    target.setConfigurable(this.<Boolean>getValue(source, "configurable"));
    target.setConfiguratorType(this.<String>getValue(source, "configuratorType"));
    target.setBaseProduct(this.<String>getValue(source, "baseProductCode"));
    target.setIsDiscontinued(this.<Boolean>getValue(source, "isDiscontinued"));

   // Adding this condition to segregate the rental and used gear data and to prevent unwanted calls on each PLP AND SLP.
    if(BlCoreConstants.RENTAL_GEAR.equalsIgnoreCase(source.getBlPage())) {
      addProductTag(source,target);
      setUpcomingAttributeValue(source, target, BlCoreConstants.UPCOMING);
      populatePrices(source, target);
      populateStock(target);
    }else {
      // Populates Serial Product Price Data
     if(null != this.getValue(source , "onSale")){
        setProductTagValues(source, target, "onSale", "On Sale");
      }
      populateSerialProductPrices(source, target);
      popaulatePromotionMessage(target);
    }
    // Populate product's classification features
    getProductFeatureListPopulator().populate(getFeaturesList(source), target);

    final List<ImageData> images = createImageData(source);
    if (CollectionUtils.isNotEmpty(images))
    {
      target.setImages(images);
    }

    if(CollectionUtils.isNotEmpty(source.getTags())){
      target.setTags(source.getTags());
    }

    populateUrl(source, target);
    populatePromotions(source, target);
    target.setIsWatching(getStockNotificationFacade().isWatchingProduct(target));
  }

  private void popaulatePromotionMessage(final ProductData target) {
    target.setUgPromotionMessage(getBlProductFacade().getPromotionMessageFromUsedGear(target));
  }

  protected void populatePrices(final SearchResultValueData source, final ProductData target)
  {
    // Pull the volume prices flag
    final Boolean volumePrices = this.<Boolean> getValue(source, "volumePrices");
    target.setVolumePricesFlag(volumePrices == null ? Boolean.FALSE : volumePrices);

    // Pull the price value for the current currency
    final Double priceValue = this.<Double> getValue(source, "priceValue");
    final Boolean constrained = this.<Boolean> getValue(source, "constrained");
    if (priceValue != null)
    {
   	 BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Default Price Value is {} for Product : {}", priceValue, target.getCode());
   	//Getting Dynamic Price if eligible for Renatl Products and selected rental days
		final BigDecimal dynamicPriceValue = getCommercePriceService().getDynamicPriceDataForProduct(constrained, priceValue);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Dynamic Calculated Price Value is {} for Product : {}", dynamicPriceValue, target.getCode());
		target.setPrice(getProductPriceData(dynamicPriceValue));
    }
  }
  
  /**
   * Populates serial product prices.
   *
   * @param source the source
   * @param target the target
   */
  private void populateSerialProductPrices(final SearchResultValueData source, final ProductData target)
  {
	  final Double minSerialfinalSalePrice = this.<Double> getValue(source, "minSerialfinalSalePrice");
	  if(PredicateUtils.notNullPredicate().evaluate(minSerialfinalSalePrice)) 
	  {
		  target.setSerialfinalSalePrice(getProductPriceData(BigDecimal.valueOf(minSerialfinalSalePrice)));
	  }
	  
	  final Double minSerialIncentivizedPrice = this.<Double> getValue(source, "minSerialIncentivizedPrice");
	  if(PredicateUtils.notNullPredicate().evaluate(minSerialIncentivizedPrice)) 
	  {
		  target.setSerialIncentivizedPrice(getProductPriceData(BigDecimal.valueOf(minSerialIncentivizedPrice)));
	  }
  }
  
  /**
   * Gets the product price data.
   *
   * @param priceValue the price value
   * @return the product price data
   */
  private PriceData getProductPriceData(final BigDecimal priceValue)
  {
	  return getPriceDataFactory().create(PriceDataType.BUY, priceValue, getCommonI18NService().getCurrentCurrency());
  }
  
  protected void populateUrl(final SearchResultValueData source, final ProductData target)
  {
    final String url = this.<String> getValue(source, "url");
    if (StringUtils.isEmpty(url))
    {
      // Resolve the URL and set it on the product data
      target.setUrl(getProductDataUrlResolver().resolve(target));
    }
    else
    {
      target.setUrl(url);
    }
  }

  protected void populatePromotions(final SearchResultValueData source, final ProductData target)
  {
    final String promotionCode = this.<String> getValue(source, "primaryPromotionCode");
    if (StringUtils.isNotEmpty(promotionCode))
    {
      final String primaryPromotionBannerUrl = this.<String> getValue(source, "primaryPromotionBanner");
      target.setPotentialPromotions(Collections.singletonList(createPromotionData(promotionCode, primaryPromotionBannerUrl)));
    }
  }

  /**
   * It sets the stock data to productData
   * @param target
   */
  protected void populateStock(final ProductData target)
  {
		try
		{
			// In case of low stock then make a call to the stock service to determine if in or out of stock.
			// In this case (low stock) it is ok to load the product from the DB and do the real stock check
			final BlProductModel blProductModel = (BlProductModel) getProductService().getProductForCode(target.getCode());
			if (blProductModel != null)
			{
				target.setStock(getStockConverter().convert(blProductModel));
			}
		}
		catch (final UnknownIdentifierException ex)
		{
			// If the product is no longer visible to the customergroup then this exception can be thrown

			// We can't remove the product from the results, but we can mark it as out of stock
			target.setStock(getStockLevelStatusConverter().convert(StockLevelStatus.OUTOFSTOCK));
		}
  }

  protected List<ImageData> createImageData(final SearchResultValueData source)
  {
    final List<ImageData> result = new ArrayList<>();

    addImageData(source, "thumbnail", result);
    addImageData(source, "product", result);

    return result;
  }

  protected void addImageData(final SearchResultValueData source, final String imageFormat, final List<ImageData> images)
  {
    final String mediaFormatQualifier = getImageFormatMapping().getMediaFormatQualifierForImageFormat(imageFormat);
    if (mediaFormatQualifier != null && !mediaFormatQualifier.isEmpty())
    {
      addImageData(source, imageFormat, mediaFormatQualifier, ImageDataType.PRIMARY, images);
    }
  }

  protected void addImageData(final SearchResultValueData source, final String imageFormat, final String mediaFormatQualifier,
      final ImageDataType type, final List<ImageData> images)
  {
    if(mediaFormatQualifier.equalsIgnoreCase(BlCoreConstants.MEDIA_FORMAT)) {
      final String splitter = BlCoreConstants.BL_IMAGE;
      final String multiImage = getValue(source, "img-" + mediaFormatQualifier);

      if(null != multiImage && !multiImage.isEmpty() && multiImage.contains(splitter)) {
        final String[] split = multiImage.split(splitter);
        for (int i = 0; i < split.length; i++) {
          final ImageData imageData = createImageData();
          imageData.setImageType(type);
          imageData.setFormat(imageFormat);
          imageData.setUrl(split[i]);
          images.add(imageData);
        }
      } else if (null != multiImage && !multiImage.isEmpty()) {
        final ImageData imageData = createImageData();
        imageData.setImageType(type);
        imageData.setFormat(imageFormat);
        imageData.setUrl(multiImage);
        images.add(imageData);
      }
    }
    else {
      final String imgValue = getValue(source, "img-" + mediaFormatQualifier);
      if (imgValue != null && !imgValue.isEmpty()) {
        final ImageData imageData = createImageData();
        imageData.setImageType(type);
        imageData.setFormat(imageFormat);
        imageData.setUrl(imgValue);

        images.add(imageData);
      }
    }
  }

  protected PromotionData createPromotionData(final String code, final String imageUrl)
  {
    final PromotionData promotionData = createPromotionData();
    promotionData.setCode(code);

    if (imageUrl != null && !imageUrl.isEmpty())
    {
      final ImageData productBanner = createImageData();
      productBanner.setUrl(imageUrl);
      promotionData.setProductBanner(productBanner);
    }

    return promotionData;
  }

  protected <T> T getValue(final SearchResultValueData source, final String propertyName)
  {
    if (source.getValues() == null)
    {
      return null;
    }

    // DO NOT REMOVE the cast (T) below, while it should be unnecessary it is required by the javac compiler
    return (T) source.getValues().get(propertyName);
  }

  protected FeatureList getFeaturesList(final SearchResultValueData source)
  {
    final List<Feature> featuresList = new ArrayList<>();
    final Locale currentLocale = getCommonI18NService().getLocaleForLanguage(getCommonI18NService().getCurrentLanguage());

    if (source != null && source.getFeatureValues() != null && !source.getFeatureValues().isEmpty())
    {
      // Pull the classification features
      for (final Map.Entry<ClassAttributeAssignmentModel, Object> featureEntry : source.getFeatureValues().entrySet())
      {
        final ClassAttributeAssignmentModel classAttributeAssignment = featureEntry.getKey();
        final Object value = featureEntry.getValue();

        final FeatureValue featureValue = new FeatureValue(value, null, classAttributeAssignment.getUnit());
        final Feature feature;
        if (Boolean.TRUE.equals(classAttributeAssignment.getLocalized()))
        {
          final Map<Locale, List<FeatureValue>> featureMap = new HashMap<>();
          featureMap.put(currentLocale, Collections.singletonList(featureValue));
          feature = new LocalizedFeature(classAttributeAssignment, featureMap, currentLocale);
        }
        else
        {
          feature = new UnlocalizedFeature(classAttributeAssignment, Collections.singletonList(featureValue));
        }
        featuresList.add(feature);
      }
    }
    return new FeatureList(featuresList);
  }

  /**
   *  this is method is created for adding product tags to target in case rental and used gear product
   * @param source source object
   * @param target target to fill
   */
  private void addProductTag(final SearchResultValueData source, final ProductData target) {
    setProductTagValues(source,target,BlCoreConstants.IS_NEW,BlCoreConstants.NEW);
    setProductTagValues(source,target,BlCoreConstants.MOST_POPULAR,BlCoreConstants.POPULAR);
    setProductTagValues(source,target,BlCoreConstants.GREAT_VALUE,BlCoreConstants.GREAT_VALUE_STRING);
    setProductTagValues(source, target, BlCoreConstants.STAFF_PICK, BlCoreConstants.STAFF_PICK_STRING);
  }

  /**
   * This method is created for adding product tags to target
   * @param source source onject
   * @param target target to be fill
   * @param key key to fetch value from source
   * @param value value to be set for target
   */
  private void setProductTagValues (final SearchResultValueData source, final ProductData target ,final String key , final String value) {
    if (StringUtils.isBlank(target.getProductTagValues()) && null!=this.<Boolean>getValue(source, key)  && this.<Boolean>getValue(source, key).booleanValue()) {
      target.setProductTagValues(value);
    }
  }


  /**
   * This method is greated for setting upcoming product to target
   * @param source source objet
   * @param target target to be fill
   * @param key key to get value from source
   */
  private void setUpcomingAttributeValue(final SearchResultValueData source, final ProductData target ,final String key) {
      if (null != this.<Boolean>getValue(source, key) && this.<Boolean>getValue(source, key)) {
        target.setIsUpcoming(this.<Boolean>getValue(source,key));
    }
  }


  protected PromotionData createPromotionData()
  {
    return new PromotionData();
  }

  protected ImageData createImageData()
  {
    return new ImageData();
  }

  protected PriceDataFactory getPriceDataFactory()
  {
    return priceDataFactory;
  }

  public void setPriceDataFactory(final PriceDataFactory priceDataFactory)
  {
    this.priceDataFactory = priceDataFactory;
  }

  protected ImageFormatMapping getImageFormatMapping()
  {
    return imageFormatMapping;
  }

  public void setImageFormatMapping(final ImageFormatMapping imageFormatMapping)
  {
    this.imageFormatMapping = imageFormatMapping;
  }

  protected UrlResolver<ProductData> getProductDataUrlResolver()
  {
    return productDataUrlResolver;
  }

  public void setProductDataUrlResolver(final UrlResolver<ProductData> productDataUrlResolver)
  {
    this.productDataUrlResolver = productDataUrlResolver;
  }

  protected Populator<FeatureList, ProductData> getProductFeatureListPopulator()
  {
    return productFeatureListPopulator;
  }

  public void setProductFeatureListPopulator(final Populator<FeatureList, ProductData> productFeatureListPopulator)
  {
    this.productFeatureListPopulator = productFeatureListPopulator;
  }

  protected ProductService getProductService()
  {
    return productService;
  }

  public void setProductService(final ProductService productService)
  {
    this.productService = productService;
  }

  protected CommonI18NService getCommonI18NService()
  {
    return commonI18NService;
  }

  public void setCommonI18NService(final CommonI18NService commonI18NService)
  {
    this.commonI18NService = commonI18NService;
  }

  protected Converter<ProductModel, StockData> getStockConverter()
  {
    return stockConverter;
  }

  public void setStockConverter(final Converter<ProductModel, StockData> stockConverter)
  {
    this.stockConverter = stockConverter;
  }

  protected Converter<StockLevelStatus, StockData> getStockLevelStatusConverter()
  {
    return stockLevelStatusConverter;
  }

  public void setStockLevelStatusConverter(final Converter<StockLevelStatus, StockData> stockLevelStatusConverter)
  {
    this.stockLevelStatusConverter = stockLevelStatusConverter;
  }

/**
 * @return the commercePriceService
 */
public BlCommercePriceService getCommercePriceService()
{
	return commercePriceService;
}

/**
 * @param commercePriceService the commercePriceService to set
 */
public void setCommercePriceService(final BlCommercePriceService commercePriceService)
{
	this.commercePriceService = commercePriceService;
}

  public StockNotificationFacade getStockNotificationFacade() {
    return stockNotificationFacade;
  }

  public void setStockNotificationFacade(StockNotificationFacade stockNotificationFacade) {
    this.stockNotificationFacade = stockNotificationFacade;
  }


  public BlProductFacade getBlProductFacade() {
    return blProductFacade;
  }

  public void setBlProductFacade(BlProductFacade blProductFacade) {
    this.blProductFacade = blProductFacade;
  }
}
