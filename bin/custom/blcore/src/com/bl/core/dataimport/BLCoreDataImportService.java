package com.bl.core.dataimport;

import de.hybris.platform.commerceservices.dataimport.impl.CoreDataImportService;


/**
 * BLCoreDataImportService class makes possible to import custom impexes along with other required impexes while system
 * initialization or update process
 *
 * @author Ravikumar
 */
public class BLCoreDataImportService extends CoreDataImportService
{

	/**
	 * Import common data.
	 *
	 * @param extensionName
	 *           the extension name
	 */
	@Override
	protected void importCommonData(final String extensionName)
	{
		super.importCommonData(extensionName);
		//importing custom common data impexes
		getSetupImpexService().importImpexFile(
				String.format("/%s/import/coredata/common/core_custom_common_data_sequence.impex", extensionName), false);
	}

	/**
	 * Import product catalog data.
	 *
	 * @param extensionName
	 *           the extension name
	 * @param productCatalogName
	 *           the product catalog name
	 */
	@Override
	protected void importProductCatalog(final String extensionName, final String productCatalogName)
	{
		super.importProductCatalog(extensionName, productCatalogName);
		//importing custom product data impexes
		getSetupImpexService().importImpexFile(
				String.format("/%s/import/coredata/productCatalogs/core_custom_product_data_sequence.impex", extensionName), false);
	}

	/**
	 * Import content catalog data.
	 *
	 * @param extensionName
	 *           the extension name
	 * @param contentCatalogName
	 *           the content catalog name
	 */
	@Override
	protected void importContentCatalog(final String extensionName, final String contentCatalogName)
	{
		super.importContentCatalog(extensionName, contentCatalogName);
		//importing custom content data impexes
		getSetupImpexService().importImpexFile(
				String.format("/%s/import/coredata/contentCatalogs/core_custom_content_data_sequence.impex", extensionName), false);
	}

	/**
	 * Import stores data.
	 *
	 * @param extensionName
	 *           the extension name
	 * @param storeName
	 *           the store name
	 * @param productCatalogName
	 *           the product catalog name
	 */
	@Override
	protected void importStore(final String extensionName, final String storeName, final String productCatalogName)
	{
		super.importStore(extensionName, storeName, productCatalogName);
		//importing custom stores data impexes
		getSetupImpexService().importImpexFile(
				String.format("/%s/import/coredata/stores/core_custom_stores_data_sequence.impex", extensionName), false);
	}

}
