package com.bl.integration.utils;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.catalog.daos.CatalogVersionDao;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.Registry;
import de.hybris.platform.product.daos.ProductDao;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.session.SessionService;
import java.util.Collection;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;

/**
 * This Util class created for updating serial product status on staged version.
 * @author Manikandan
 */
public class BlUpdateStagedProductUtils {

  private BlUpdateStagedProductUtils() {
    //empty to avoid instantiating utils class
  }

  public static final String PRODUCT_CATALOG = "catalog";
  public static final String VERSION = "version";
  public static final String STAGED = "Staged";
  public static final String CATALOG_VALUE = "blProductCatalog";

  private static SessionService sessionService;
  private static CatalogVersionDao catalogVersionDao;
  private static ModelService modelService;
  private static SearchRestrictionService searchRestrictionService;
  private static ProductDao productDao;

  /**
   *  This method created to change the serial status .
   * @param productCode product code
   * @param serialStatus status to be update
   */
  public static void changeSerialStatusInStagedVersion(final String productCode, final SerialStatusEnum serialStatus) {
    Collection<CatalogVersionModel> catalogModels =  getCatalogVersionDao().findCatalogVersions(
        CATALOG_VALUE, STAGED);
    List<BlSerialProductModel> products = getProductsOfStagedVersion(productCode, catalogModels.iterator().next());
    if(CollectionUtils.isNotEmpty(products)) {
      BlSerialProductModel product = products.get(0);
      product.setSerialStatus(serialStatus);
      getModelService().save(product);
    }
  }

  /**
   * It gets serialProductModel of staged version
   *
   * @param productCode the product code
   * @param catalogVersionModel the catalog version model
   * @return List<BlSerialProductModel> the blSerialProducts
   */
  private static  List<BlSerialProductModel> getProductsOfStagedVersion(final String productCode,
      final CatalogVersionModel catalogVersionModel) {
    return getSessionService().executeInLocalView(new SessionExecutionBody()
    {
      @Override
      public Object execute()
      {
        try
        {
          getSearchRestrictionService().disableSearchRestrictions();
          return getProductDao().findProductsByCode(catalogVersionModel,
              productCode);
        }
        finally
        {
          getSearchRestrictionService().enableSearchRestrictions();
        }
      }
    });
  }


  public static SessionService getSessionService() {
    return null == sessionService ? (SessionService) Registry.getApplicationContext()
        .getBean("sessionService") : sessionService;
  }

  public static void setSessionService(
      SessionService sessionService) {
    BlUpdateStagedProductUtils.sessionService = sessionService;
  }


  private static CatalogVersionDao getCatalogVersionDao() {
    return null == catalogVersionDao ? (CatalogVersionDao) Registry.getApplicationContext()
        .getBean("catalogVersionDao") : catalogVersionDao;
  }

  public static void setCatalogVersionDao(
      CatalogVersionDao catalogVersionDao) {
    BlUpdateStagedProductUtils.catalogVersionDao = catalogVersionDao;
  }

  public static ModelService getModelService() {
    return null == modelService ? (ModelService) Registry.getApplicationContext()
        .getBean("modelService") : modelService;
  }

  public static void setModelService(
      ModelService modelService) {
    BlUpdateStagedProductUtils.modelService = modelService;
  }


  public static SearchRestrictionService getSearchRestrictionService() {
    return null == searchRestrictionService ? (SearchRestrictionService) Registry.getApplicationContext()
        .getBean("searchRestrictionService") : searchRestrictionService;
  }

  public static void setSearchRestrictionService(
      SearchRestrictionService searchRestrictionService) {
    BlUpdateStagedProductUtils.searchRestrictionService = searchRestrictionService;
  }

  public static ProductDao getProductDao() {
    return null == productDao ? (ProductDao) Registry.getApplicationContext()
        .getBean("productDao") : productDao;
  }

  public static void setProductDao(ProductDao productDao) {
    BlUpdateStagedProductUtils.productDao = productDao;
  }


}
