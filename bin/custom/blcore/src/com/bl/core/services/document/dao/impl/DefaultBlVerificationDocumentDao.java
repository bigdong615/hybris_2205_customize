/**
 *
 */
package com.bl.core.services.document.dao.impl;

import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import com.bl.core.model.VerificationDocumentMediaModel;
import com.bl.core.services.document.dao.BlVerificationDocumentDao;
import com.bl.logging.BlLogger;
import org.apache.log4j.Logger;


/**
 * @author Avani Patel
 *
 */
public class DefaultBlVerificationDocumentDao implements BlVerificationDocumentDao {

	private static final Logger LOG = Logger.getLogger(DefaultBlVerificationDocumentDao.class);
	private FlexibleSearchService flexibleSearchService;

	/**
	 * Remove Verification Document
	 *
	 * @param code
	 * @return VerificationDocumentMedia
	 */
	@Override
	public VerificationDocumentMediaModel removeVerificationDocument(final String code) {

			final String getDocumentByCodeQuery = "SELECT {vm.pk} FROM {"+ VerificationDocumentMediaModel._TYPECODE +"  AS vm} WHERE {vm:code} = ?code";
			final FlexibleSearchQuery flexibleSearchQuery = new FlexibleSearchQuery(
					getDocumentByCodeQuery);
			flexibleSearchQuery.addQueryParameter("code", code);
			final SearchResult<VerificationDocumentMediaModel> result = getFlexibleSearchService()
					.search(flexibleSearchQuery);
			final List<VerificationDocumentMediaModel> verificationDocument = result.getResult();
			if (CollectionUtils.isEmpty(verificationDocument)) {
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No document found for code: {}", code);
				return null;
			}
			return verificationDocument.get(0);
	}
	/**
	 * @return the flexibleSearchService
	 */
	public FlexibleSearchService getFlexibleSearchService() {
		return flexibleSearchService;
	}

	/**
	 * @param flexibleSearchService the flexibleSearchService to set
	 */
	public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService) {
		this.flexibleSearchService = flexibleSearchService;
	}
}