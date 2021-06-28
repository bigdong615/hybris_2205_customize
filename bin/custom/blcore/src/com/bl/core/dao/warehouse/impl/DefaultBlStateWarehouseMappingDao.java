package com.bl.core.dao.warehouse.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlStateWarehouseMappingDao;
import com.bl.core.model.BlStateWarehouseMappingModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import org.apache.commons.collections.CollectionUtils;


/**
 * This class is
 * created for State Warehouse Mapping.
 *
 * @author Sunil
 */
public class DefaultBlStateWarehouseMappingDao implements BlStateWarehouseMappingDao {

    private FlexibleSearchService flexibleSearchService;
    private static final String FIND_BL_STATE_WAREHOUSE_BY_STATE_CODE =
        "SELECT {pk} FROM {" + BlStateWarehouseMappingModel._TYPECODE + "} WHERE {"
            + BlStateWarehouseMappingModel.CODE
            + "} IN ({{SELECT {reg:PK} FROM {Region as reg} WHERE {reg:isoCodeShort} =  ?isoCodeShort }})";

    /**
     * {@inheritDoc}
     */
    @Override
    public BlStateWarehouseMappingModel getStateWarehouseForStateCode(final String isoCodeShort) {

        final FlexibleSearchQuery query = new FlexibleSearchQuery(
            FIND_BL_STATE_WAREHOUSE_BY_STATE_CODE);
        query.addQueryParameter(BlCoreConstants.ISO_CODE_SHORT, isoCodeShort);
        final SearchResult<BlStateWarehouseMappingModel> result = getFlexibleSearchService()
            .search(query);
        if (null != result && CollectionUtils.isNotEmpty(result.getResult())) {
            return result.getResult().get(0);
        }

       return null;
    }

    public FlexibleSearchService getFlexibleSearchService() {
        return flexibleSearchService;
    }

    public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService) {
        this.flexibleSearchService = flexibleSearchService;
    }

}
