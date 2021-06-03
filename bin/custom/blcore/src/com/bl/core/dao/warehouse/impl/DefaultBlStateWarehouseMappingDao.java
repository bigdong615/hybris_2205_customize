package com.bl.core.dao.warehouse.impl;

import com.bl.core.dao.warehouse.BlStateWarehouseMappingDao;
import com.bl.core.model.BlStateWarehouseMappingModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;


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
        query.addQueryParameter("isoCodeShort", isoCodeShort);
        final SearchResult result = getFlexibleSearchService().search(query);
        BlStateWarehouseMappingModel model = (BlStateWarehouseMappingModel) result.getResult()
            .get(0);
        return model;
    }

    public FlexibleSearchService getFlexibleSearchService() {
        return flexibleSearchService;
    }

    public void setFlexibleSearchService(FlexibleSearchService flexibleSearchService) {
        this.flexibleSearchService = flexibleSearchService;
    }

}
