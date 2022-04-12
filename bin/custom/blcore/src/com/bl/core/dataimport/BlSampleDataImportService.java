/**
 *
 */
package com.bl.core.dataimport;

import de.hybris.platform.commerceservices.dataimport.impl.SampleDataImportService;
import de.hybris.platform.util.Config;


/**
 * This class is used to import common data, product catalog, content catalog, store from sampledata while performing
 * system initialization or update process.
 *
 * @author Ravikumar
 */

public class BlSampleDataImportService extends SampleDataImportService
{

	/**
	 * Import product catalog impexes from sampledata .
	 *
	 * @param extensionName
	 *           the extension name
	 * @param productCatalogName
	 *           the product catalog name
	 */
	@Override
	protected void importProductCatalog(final String extensionName, final String productCatalogName)
	{
		//super.importProductCatalog(extensionName, productCatalogName);
		//importing custom product data impexes
		if (Config.getBoolean("test.data", false))
		{
			getSetupImpexService().importImpexFile(String.format(
					"/%s/import/sampledata/productCatalogs/sampledata_custom_product_data_sequence.impex", extensionName), false);
		}
		else
		{
			getSetupImpexService().importImpexFile(
					String.format("/%s/import/sampledata/productCatalogs/proddata_custom_product_data_sequence.impex", extensionName),
					false);
		}
	}

	/**
	 * Import content catalog impexes from sampledata.
	 *
	 * @param extensionName
	 *           the extension name
	 * @param contentCatalogName
	 *           the content catalog name
	 */
	@Override
	protected void importContentCatalog(final String extensionName, final String contentCatalogName)
	{
		//super.importContentCatalog(extensionName, contentCatalogName);
		//importing custom content data impexes
		getSetupImpexService().importImpexFile(
				String.format("/%s/import/sampledata/contentCatalogs/sampledata_custom_content_data_sequence.impex", extensionName),
				false);
	}

	/**
	 * Import stores impexes from sampledata.
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
		//super.importStore(extensionName, storeName, productCatalogName);
		//importing custom stores data impexes
		getSetupImpexService().importImpexFile(
				String.format("/%s/import/sampledata/stores/sampledata_custom_stores_data_sequence.impex", extensionName), false);
	}

}
