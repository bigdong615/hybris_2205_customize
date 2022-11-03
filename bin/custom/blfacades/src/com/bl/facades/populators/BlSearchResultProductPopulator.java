package com.bl.facades.populators;

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
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.model.BlProductModel;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.product.service.BlProductService;
import com.bl.core.promotions.promotionengineservices.service.BlPromotionService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;


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
  private BaseStoreService baseStoreService;
  private BlPromotionService blPromotionService;
  private CommonI18NService commonI18NService;
  private Converter<ProductModel, StockData> stockConverter;
  private Converter<StockLevelStatus, StockData> stockLevelStatusConverter;
  private BlCommercePriceService commercePriceService;
  private BlWishlistOptionsPopulator blWishlistOptionsPopulator;
  private BlProductService blProductService;
  private BlCommerceStockService blCommerceStockService;
  private BlDatePickerService blDatePickerService;

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
    target.setDisplayName(this.<String>getValue(source, "displayName"));
    target.setManufacturer(this.<String>getValue(source, "manufacturerName"));
    target.setDescription(this.<String>getValue(source, "description"));
    target.setSummary(this.<String>getValue(source, "summary"));
    target.setAverageRating(this.<Double>getValue(source, "reviewAvgRating"));
    target.setConfigurable(this.<Boolean>getValue(source, "configurable"));
    target.setConfiguratorType(this.<String>getValue(source, "configuratorType"));
    target.setBaseProduct(this.<String>getValue(source, "baseProductCode"));
    target.setIsDiscontinued(this.<Boolean>getValue(source, "isDiscontinued"));
    target.setIsBlProductType(this.<Boolean>getValue(source, "isBlProductType"));
    if(null != this.getValue(source , BlCoreConstants.RETAILGEAR)) {
      target.setRetailGear(this.<Boolean>getValue(source, "retailGear"));
      target.setIsRetailGearInStock(this.<Boolean>getValue(source, "retailGearInStock"));
      final Double retailGearPrice = this.<Double> getValue(source, "retailGearPrice");
      if(PredicateUtils.notNullPredicate().evaluate(retailGearPrice))
      {
        target.setRetailGearPrice(getProductPriceData(BigDecimal.valueOf(retailGearPrice)));
      }
    }
    if(null != this.getValue(source , BlCoreConstants.BUNDLEPRODUCT)) {
   	 target.setIsBundle(this.<Boolean>getValue(source, BlCoreConstants.BUNDLEPRODUCT));
    }
   // Adding this condition to segregate the rental and used gear data and to prevent unwanted calls on each PLP AND SLP.
    if(BlCoreConstants.RENTAL_GEAR.equalsIgnoreCase(source.getBlPage())) {
      addProductTag(source,target);
      setUpcomingAttributeValue(source, target, BlCoreConstants.UPCOMING);
      populatePrices(source, target);
      populateStock(target);
      populateBookMarks(target);
    }else {
     if(null != this.getValue(source , BlFacadesConstants.ON_SALE)){
        setProductTagValues(source, target, BlFacadesConstants.ON_SALE, BlFacadesConstants.ON_SALE_TAG_VALUE);
        target.setOnSale(this.<Boolean>getValue(source, BlFacadesConstants.ON_SALE));
      }
      // Populates Serial Product Price Data
      populateSerialProductPrices(source, target);
      populateSerialPromotionMessage(target);
    }
    // Populate product type
    populateProductType(target);
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
	 populateAdditionalAttributesForDomo(source, target);

  }


/**
 * @param source
 * @param target
 */
private void populateAdditionalAttributesForDomo(final SearchResultValueData source, final ProductData target)
{
	try
	{
		//target.setCreatedTS(this.<Date> getValue(source, "createdTS"));
		 //target.setModifiedTS(this.<Date> getValue(source, "modifiedTS"));
		 target.setOnlineDate(this.<Date> getValue(source, "onlineDate"));
		 target.setOfflineDate(this.<Date> getValue(source, "offlineDate"));
		 target.setCreatedDate(this.<Date> getValue(source, "createdDate"));
		 target.setDateFirstActive(this.<Date> getValue(source, "dateFirstActive"));
		 target.setInvoiceDate(this.<Date> getValue(source, "invoiceDate"));
		 target.setDateOfSale(this.<Date> getValue(source, "dateOfSale"));
		 target.setLastUnboxedOcLocationDate(this.<Date> getValue(source, "lastUnboxedOcLocationDate"));
		 target.setSupplierAlternativeAID(this.<String> getValue(source, "supplierAlternativeAID"));
		 target.setErpGroupBuyer(this.<String> getValue(source, "erpGroupBuyer"));
		 target.setErpGroupSupplier(this.<String> getValue(source, "erpGroupSupplier"));
		 if (this.<Double> getValue(source, "deliveryTime") != null)
		 {
			 target.setDeliveryTime(this.<Double> getValue(source, "deliveryTime"));
		 }
		 if (this.<Double> getValue(source, "priceQuantity") != null)
		 {
			 target.setPriceQuantity(this.<Double> getValue(source, "priceQuantity"));
		 }
		 if (this.<Double> getValue(source, "bufferedInventoryPercentage") != null)
		 {
			 target.setBufferedInventoryPercentage(this.<Double> getValue(source, "bufferedInventoryPercentage"));
		 }
		 target.setMinOrderQuantity(this.<Integer> getValue(source, "minOrderQuantity"));
		 target.setMaxOrderQuantity(this.<Integer> getValue(source, "maxOrderQuantity"));
		 target.setOrderQuantityInterval(this.<Integer> getValue(source, "orderQuantityInterval"));
		 target.setStartLineNumber(this.<Integer> getValue(source, "startLineNumber"));
		 target.setEndLineNumber(this.<Integer> getValue(source, "endLineNumber"));
		 target.setReviewCount(this.<Integer> getValue(source, "reviewCount"));
		 //target.setWeight(this.<BigDecimal> getValue(source, "weight"));
		 //target.setHeight(this.<BigDecimal> getValue(source, "height"));
		 //target.setWidth(this.<BigDecimal> getValue(source, "width"));
		 //target.setLength(this.<BigDecimal> getValue(source, "length"));
		 target.setDiscontinued(this.<Boolean> getValue(source, "isDiscontinued"));
		 target.setLevel1Required(this.<Boolean> getValue(source, "level1Required"));
		 target.setLevel2Required(this.<Boolean> getValue(source, "level2Required"));
		 target.setMostPopular(this.<Boolean> getValue(source, "mostPopular"));
		 target.setScheduled(this.<Boolean> getValue(source, "scheduled"));
		 target.setStaffPick(this.<Boolean> getValue(source, "staffPick"));
		 target.setBufferInvPercChangedManually(this.<Boolean> getValue(source, "bufferInvPercChangedManually"));
		 target.setRetailGearInStock(this.<Boolean> getValue(source, "retailGearInStock"));
		 target.setBundleProduct(this.<Boolean> getValue(source, "bundleProduct"));
		 target.setConstrained(this.<Boolean> getValue(source, "constrained"));
		 target.setForRent(this.<Boolean> getValue(source, "forRent"));
		 target.setForSale(this.<Boolean> getValue(source, "forSale"));
		 target.setGreatValue(this.<Boolean> getValue(source, "greatValue"));
		 target.setIsAccounting(this.<Boolean> getValue(source, "isAccounting"));
		 if (this.<Double> getValue(source, "isNew") != null)
		 {
			 target.setIsNew(this.<Boolean> getValue(source, "isNew"));
		 }
		 if (this.<Double> getValue(source, "isVideo") != null)
		 {
			 target.setIsVideo(this.<Boolean> getValue(source, "isVideo"));
		 }
		 if (this.<Double> getValue(source, "hardAssigned") != null)
		 {
			 target.setHardAssigned(this.<Boolean> getValue(source, "hardAssigned"));
		 }
		 if (this.<Double> getValue(source, "isBufferedInventory") != null)
		 {
			 target.setIsBufferedInventory(this.<Boolean> getValue(source, "isBufferedInventory"));
		 }
		 if (this.<Double> getValue(source, "isSyncRequired") != null)
		 {
			 target.setIsSyncRequired(this.<Boolean> getValue(source, "isSyncRequired"));
		 }
		 if (this.<Double> getValue(source, "softAssigned") != null)
		 {
			 target.setSoftAssigned(this.<Boolean> getValue(source, "softAssigned"));
		 }
		 if (this.<Double> getValue(source, "dirtyPriorityStatus") != null)
		 {
			 target.setDirtyPriorityStatus(this.<Boolean> getValue(source, "dirtyPriorityStatus"));
		 }
		 if (this.<Double> getValue(source, "gearRated") != null)
		 {
			 target.setGearRated(this.<Boolean> getValue(source, "gearRated"));
		 }
		 if (this.<Double> getValue(source, "soldIndividually") != null)
		 {
			 target.setSoldIndividually(this.<Boolean> getValue(source, "soldIndividually"));
		 }
		 if (this.<Double> getValue(source, "softAssigned") != null)
		 {
			 target.setSoftAssigned(this.<Boolean> getValue(source, "softAssigned"));
		 }
	}
	catch (final Exception e)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Error while creating Additional Attributes For Domo", e.getMessage());
		e.printStackTrace();
	}
}


  /**
   * To Populate the Product data for bookmark
   * @param target
   */
  private void populateBookMarks(final ProductData target) {
    final BlProductModel blProductModel = (BlProductModel) getProductService().getProductForCode(target.getCode());
    if (blProductModel != null)
    {
      getBlWishlistOptionsPopulator().populate(blProductModel,target);
    }

  }
  /**
   * To Populate the Product data for product type
   * @param target
   */
  private void populateProductType(final ProductData target) {
    final BlProductModel blProductModel = (BlProductModel) getProductService().getProductForCode(target.getCode());
    if (blProductModel != null)
    {
      target.setProductType(blProductModel.getProductType().getCode());
    }

  }

  /**
   * Populating message on BlProduct when any Serial has active promotion
   * @param target
   */
  private void populateSerialPromotionMessage(final ProductData target) {
    final BaseStoreModel baseStore = getBaseStoreService().getCurrentBaseStore();
    if(baseStore != null && StringUtils.isNotBlank(baseStore.getUsedGearPromotionMessage()) && getBlPromotionService().isUsedGearCategoryPromotionActive()) {
      target.setUgPromotionMessage(baseStore.getUsedGearPromotionMessage());
    }
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

   else if(target.isIsBundle())
    {
      final BlProductModel blProductModel = (BlProductModel) getProductService().getProductForCode(target.getCode());
      final BigDecimal dynamicPriceValue = getCommercePriceService().getDynamicPriceDataForBundleProduct(constrained, blProductModel);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Dynamic Calculated Price Value is {} for Product : {}", dynamicPriceValue, target.getCode());
       target.setPrice(Objects.nonNull(dynamicPriceValue) ? getProductPriceData(dynamicPriceValue) : (getProductPriceData(BigDecimal.valueOf(0.0d))));
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
	  final Double minSerialPromoPrice = this.<Double> getValue(source, "minSerialPromoPrice");
	  if(PredicateUtils.notNullPredicate().evaluate(minSerialPromoPrice))
	  {
		  target.setSerialPromotionPrice(getProductPriceData(BigDecimal.valueOf(minSerialPromoPrice)));
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
      if (blProductModel != null && !blProductService
          .isAquatechProduct(getProductService().getProductForCode(target.getCode()))) {

        target.setStock(getStockConverter().convert(blProductModel));
      }

      final RentalDateDto rentalDatesFromSession = getBlDatePickerService()
          .getRentalDatesFromSession();
      if (Objects.nonNull(rentalDatesFromSession) && Objects.nonNull(blProductModel)) {
        final String nextAvailableDate = getBlCommerceStockService()
            .getNextAvailabilityDateInPDP(blProductModel.getCode(), rentalDatesFromSession);
        target.setNextAvailableDate(nextAvailableDate);
        if (StringUtils.isNotBlank(nextAvailableDate)) {
          final RentalDateDto rentalDuration =  BlRentalDateUtils.getRentalsDuration();
          if (Objects.nonNull(rentalDuration) && StringUtils
              .isNotBlank(rentalDuration.getSelectedFromDate())
              && !nextAvailableDate.equalsIgnoreCase(rentalDuration.getSelectedFromDate())) {
            target.setDisableButton(Boolean.TRUE);
          }
        }
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
  private void setUpcomingAttributeValue(final SearchResultValueData source,
      final ProductData target, final String key) {

    if (null != this.<Boolean>getValue(source, key) && this.<Boolean>getValue(source, key)
        && !blProductService
        .isAquatechProduct(getProductService().getProductForCode(target.getCode()))) {

      target.setIsUpcoming(this.<Boolean>getValue(source, key));
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

  public void setStockNotificationFacade(final StockNotificationFacade stockNotificationFacade) {
    this.stockNotificationFacade = stockNotificationFacade;
  }

  public BlWishlistOptionsPopulator getBlWishlistOptionsPopulator() {
    return blWishlistOptionsPopulator;
  }

  public void setBlWishlistOptionsPopulator(
      final BlWishlistOptionsPopulator blWishlistOptionsPopulator) {
    this.blWishlistOptionsPopulator = blWishlistOptionsPopulator;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(final BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }

  public BlPromotionService getBlPromotionService() {
    return blPromotionService;
  }

  public void setBlPromotionService(
      final BlPromotionService blPromotionService) {
    this.blPromotionService = blPromotionService;
  }

  public BlProductService getBlProductService() {
    return blProductService;
  }

  public void setBlProductService(final BlProductService blProductService) {
    this.blProductService = blProductService;
  }

  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(final BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }

  public BlDatePickerService getBlDatePickerService() {
    return blDatePickerService;
  }

  public void setBlDatePickerService(final BlDatePickerService blDatePickerService) {
    this.blDatePickerService = blDatePickerService;
  }


}
