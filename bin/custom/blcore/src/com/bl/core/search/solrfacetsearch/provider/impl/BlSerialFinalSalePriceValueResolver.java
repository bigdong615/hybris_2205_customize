package com.bl.core.search.solrfacetsearch.provider.impl;

import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;

import java.util.Comparator;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;


/**
 * This class is responsible to send calculated value to SOLR Document from Hybris for finalSalePrice attribute of
 * BlSerialProduct
 *
 * @author Ravikumar
 *
 */
public class BlSerialFinalSalePriceValueResolver extends AbstractValueResolver<BlProductModel, Object, Object>
{
	private static final Logger LOG = Logger.getLogger(BlSerialFinalSalePriceValueResolver.class);

	/**
	 * Adds the field values by getting the minimum final sale price value from the list of BlSerialProducts on SKU
	 *
	 * @param inputDocument
	 *           the input document
	 * @param indexerBatchContext
	 *           the indexer batch context
	 * @param indexedProperty
	 *           the indexed property
	 * @param blProductModel
	 *           the bl product model
	 * @param valueResolverContext
	 *           the value resolver context
	 * @throws FieldValueProviderException
	 *            the field value provider exception
	 */
	@Override
	protected void addFieldValues(final InputDocument inputDocument, final IndexerBatchContext indexerBatchContext,
			final IndexedProperty indexedProperty, final BlProductModel blProductModel,
			final ValueResolverContext<Object, Object> valueResolverContext) throws FieldValueProviderException
	{
		try
		{
			if (PredicateUtils.notNullPredicate().evaluate(blProductModel)
					&& CollectionUtils.isNotEmpty(blProductModel.getSerialProducts()))
			{
				final Optional<BlSerialProductModel> minSerialfinalSalePrice = blProductModel.getSerialProducts().stream()
						.filter(serialProductModel -> BooleanUtils.isTrue(serialProductModel.getForSale())
								&& PredicateUtils.notNullPredicate().evaluate(serialProductModel.getFinalSalePrice()))
						.collect(Collectors.minBy(Comparator.comparing(serialProduct -> serialProduct.getFinalSalePrice())));

				if (minSerialfinalSalePrice.isPresent())
				{
					inputDocument.addField(indexedProperty, minSerialfinalSalePrice.get().getFinalSalePrice().doubleValue());
				}
			}
		}
		catch (final Exception exception)
		{
			final String productCode = StringUtils.isNotBlank(blProductModel.getCode()) ? blProductModel.getCode()
					: StringUtils.EMPTY;
			BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.SOLR_INDEXING_ERROR.getCode(), exception,
					"Failed to resolve value for minSerialfinalSalePrice attribute for product with code: {}", productCode);
		}
	}

}
