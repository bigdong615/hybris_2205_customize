package com.bl.core.after.save.listener;

import de.hybris.platform.catalog.daos.CatalogVersionDao;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.GenericSearchConstants.LOG;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.exceptions.ModelLoadingException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.solrfacetsearch.enums.IndexerOperationValues;
import de.hybris.platform.solrfacetsearch.indexer.cron.SolrIndexerHotUpdateJob;
import de.hybris.platform.solrfacetsearch.model.config.SolrFacetSearchConfigModel;
import de.hybris.platform.solrfacetsearch.model.indexer.cron.SolrIndexerHotUpdateCronJobModel;
import de.hybris.platform.tx.AfterSaveEvent;
import de.hybris.platform.tx.AfterSaveListener;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;


/**
 *
 * The class BlSerialProductAfterSaveListener will perform to set attribute values of Serial Product from online version
 * to staged version
 *
 * @author Ravikumar
 *
 */
public class BlSerialProductAfterSaveListener implements AfterSaveListener
{
	private static final Logger LOG = Logger.getLogger(BlSerialProductAfterSaveListener.class);
	private ModelService modelService;
	private BlProductService productService;
	private CatalogVersionDao catalogVersionDao;
	private Populator<BlSerialProductModel, BlSerialProductModel> populator;
	private SessionService sessionService;
	private BlStockLevelDao blStockLevelDao;
	private SolrIndexerHotUpdateJob solrIndexerHotUpdateJob;

	@Override
	public void afterSave(final Collection<AfterSaveEvent> afterSaveEventCollection)
	{
		final boolean isSyncActive = BooleanUtils
				.toBoolean((Boolean) getSessionService().getCurrentSession().getAttribute("catalog.sync.active"));
		if (!isSyncActive && CollectionUtils.isNotEmpty(afterSaveEventCollection))
		{
			afterSaveEventCollection.forEach(event -> {
				if (event.getType() == AfterSaveEvent.UPDATE)
				{
					final PK pk = event.getPk();
					final Object object = getObjectFromPK(pk);
					if (object instanceof BlSerialProductModel)
					{
						performUpdateStagedSerial(object);
						final BlSerialProductModel serial = (BlSerialProductModel) object;
						if (serial.getIsSyncRequired())
						{
							updateIndexForProduct(serial);
						}
					}
				}
			});
		}
	}

	/**
	 * Gets the object from PK.
	 *
	 * @param pk the pk
	 * @return the object from PK
	 */
	private Object getObjectFromPK(final PK pk)
	{
		if(Objects.nonNull(pk))
		{
			try
			{
				return getModelService().get(pk);
			}
			catch(final ModelLoadingException exception)
			{
				BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception, "No item found for given pk: {}", pk);
			}
		}
		return null;
	}

	/**
	 * Perform updating staged serial from online serial.
	 *
	 * @param object
	 *           the object
	 */
	private void performUpdateStagedSerial(final Object object)
	{
		final BlSerialProductModel onlineSerial = ((BlSerialProductModel) object);
		if (onlineSerial.getCatalogVersion().getVersion().equals(BlCoreConstants.ONLINE))
		{
			final Collection<CatalogVersionModel> catalogModels = getCatalogVersionDao()
					.findCatalogVersions(BlCoreConstants.CATALOG_VALUE, BlCoreConstants.STAGED);
			if (CollectionUtils.isNotEmpty(catalogModels))
			{
				final CatalogVersionModel stagedCatalogVersion = catalogModels.iterator().next();
				final List<BlSerialProductModel> productsOfStagedVersion = getProductService()
						.getProductsOfStagedVersion(onlineSerial.getCode(), stagedCatalogVersion);
				final BlSerialProductModel stagedSerial = CollectionUtils.isNotEmpty(productsOfStagedVersion)
						? productsOfStagedVersion.iterator().next()
						: null;
				if (Objects.nonNull(stagedSerial))
				{
					getPopulator().populate(onlineSerial, stagedSerial);
					getModelService().save(stagedSerial);
					getModelService().refresh(stagedSerial);
					BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Data Updated on Staged serial with code : {} and pk : {}",
							stagedSerial.getCode(), stagedSerial.getPk().toString());
				}
			}
		}
	}

	private void updateIndexForProduct(final BlSerialProductModel serial)
	{
		try
		{
			final SolrIndexerHotUpdateCronJobModel cronJob = new SolrIndexerHotUpdateCronJobModel();
			final List<ItemModel> products = new ArrayList<ItemModel>();
			products.add(serial.getBlProduct());
			cronJob.setItems(products);
			final SolrFacetSearchConfigModel facetConfig = getBlStockLevelDao().getFacetConfigModel();
			cronJob.setFacetSearchConfig(facetConfig);
			cronJob.setIndexerOperation(IndexerOperationValues.UPDATE);
			cronJob.setIndexTypeName("BlProduct");
			getSolrIndexerHotUpdateJob().perform(cronJob);
		}
		catch (final Exception ex)
		{
			LOG.info("Error during running hot update index for product " + serial.getBlProduct().getCode() + "and exception is ",
					ex);
		}
	}

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * @return the populator
	 */
	public Populator<BlSerialProductModel, BlSerialProductModel> getPopulator()
	{
		return populator;
	}

	/**
	 * @param populator
	 *           the populator to set
	 */
	public void setPopulator(final Populator<BlSerialProductModel, BlSerialProductModel> populator)
	{
		this.populator = populator;
	}

	/**
	 * @return the productService
	 */
	public BlProductService getProductService()
	{
		return productService;
	}

	/**
	 * @param productService
	 *           the productService to set
	 */
	public void setProductService(final BlProductService productService)
	{
		this.productService = productService;
	}

	/**
	 * @return the catalogVersionDao
	 */
	public CatalogVersionDao getCatalogVersionDao()
	{
		return catalogVersionDao;
	}

	/**
	 * @param catalogVersionDao
	 *           the catalogVersionDao to set
	 */
	public void setCatalogVersionDao(final CatalogVersionDao catalogVersionDao)
	{
		this.catalogVersionDao = catalogVersionDao;
	}

	/**
	 * @return the sessionService
	 */
	public SessionService getSessionService()
	{
		return sessionService;
	}

	/**
	 * @param sessionService
	 *           the sessionService to set
	 */
	public void setSessionService(final SessionService sessionService)
	{
		this.sessionService = sessionService;
	}

	/**
	 * @return the blStockLevelDao
	 */
	public BlStockLevelDao getBlStockLevelDao()
	{
		return blStockLevelDao;
	}

	/**
	 * @param blStockLevelDao
	 *           the blStockLevelDao to set
	 */
	public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao)
	{
		this.blStockLevelDao = blStockLevelDao;
	}

	/**
	 * @return the solrIndexerHotUpdateJob
	 */
	public SolrIndexerHotUpdateJob getSolrIndexerHotUpdateJob()
	{
		return solrIndexerHotUpdateJob;
	}

	/**
	 * @param solrIndexerHotUpdateJob
	 *           the solrIndexerHotUpdateJob to set
	 */
	public void setSolrIndexerHotUpdateJob(final SolrIndexerHotUpdateJob solrIndexerHotUpdateJob)
	{
		this.solrIndexerHotUpdateJob = solrIndexerHotUpdateJob;
	}

}
