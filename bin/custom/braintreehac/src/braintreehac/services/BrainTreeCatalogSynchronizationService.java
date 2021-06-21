package braintreehac.services;

import de.hybris.platform.catalog.CatalogTypeService;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.SyncItemJobModel;
import de.hybris.platform.catalog.synchronization.CatalogSynchronizationService;
import de.hybris.platform.catalog.synchronization.SyncConfig;
import de.hybris.platform.catalog.synchronization.SynchronizationStatusService;
import de.hybris.platform.cms2.common.service.SessionSearchRestrictionsDisabler;
import de.hybris.platform.cmsfacades.data.SyncRequestData;
import de.hybris.platform.cmsfacades.synchronization.service.impl.DefaultItemSynchronizationService;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.model.collector.RelatedItemsCollector;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.type.TypeService;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;


@Component
public class BrainTreeCatalogSynchronizationService extends DefaultItemSynchronizationService {

    private static final Logger LOG = Logger.getLogger(BrainTreeCatalogSynchronizationService.class);

    @Resource(name = "catalogSynchronizationService")
    private CatalogSynchronizationService catalogSynchronizationService;

    @Resource(name = "syncConfig")
    private SyncConfig syncConfig;

    @Resource(name = "modelService")
    private ModelService modelService;

    @Resource(name = "typeService")
    private TypeService typeService;

    @Resource(name = "catalogTypeService")
    private CatalogTypeService catalogTypeService;

    @Resource(name = "catalogVersionService")
    private CatalogVersionService catalogVersionService;

    @Resource(name = "commonI18NService")
    private CommonI18NService commonI18NService;

    @Resource(name = "synchronizationStatusService")
    private SynchronizationStatusService platformSynchronizationStatusService;

    @Resource(name = "relatedItemsCollector")
    private RelatedItemsCollector relatedItemsCollector;

    @Resource(name = "searchRestrictionService")
    private SearchRestrictionService searchRestrictionService;

    @Resource(name = "cmsSessionSearchRestrictionsDisabler")
    private SessionSearchRestrictionsDisabler sessionSearchRestrictionsDisabler;

    @Resource(name = "sessionService")
    private SessionService sessionService;

    public void synchronizeItems(List<ItemModel> itemsToSynchronize, final CatalogVersionModel source,
                                 final CatalogVersionModel target) {
        SyncRequestData syncRequestData = createSyncRequestData(source, target);
        getCatalogSynchronizationService().performSynchronization(itemsToSynchronize,
                getSyncItemJobModel(itemsToSynchronize, syncRequestData),
                getSyncConfig());
    }

    private SyncRequestData createSyncRequestData(final CatalogVersionModel source,
                                                  final CatalogVersionModel target){
        final SyncRequestData syncRequestData = new SyncRequestData();
        syncRequestData.setCatalogId(source.getCatalog().getId());
        syncRequestData.setSourceVersionId(source.getVersion());
        syncRequestData.setTargetVersionId(target.getVersion());
        return syncRequestData;
    }

    public SyncItemJobModel getSyncItemJobModel(List<ItemModel> itemsToSynchronize, SyncRequestData syncRequestData){
        ItemModel firstItem = itemsToSynchronize.stream().findFirst().get();
        final ComposedTypeModel givenComposedType = typeService.getComposedTypeForCode(firstItem.getItemtype());
        SyncItemJobModel relevantSyncItemJob = getRelevantSyncItemJob(syncRequestData, firstItem);

        List<ComposedTypeModel> oldRootTypes = relevantSyncItemJob.getRootTypes();
        if (!oldRootTypes.contains(givenComposedType)){
            List<ComposedTypeModel> rootTypes = new ArrayList<>();
            rootTypes.add(givenComposedType);
            rootTypes.addAll(oldRootTypes);
            relevantSyncItemJob.setRootTypes(rootTypes);
        }

        return relevantSyncItemJob;
    }

    public CatalogSynchronizationService getCatalogSynchronizationService() {
        return catalogSynchronizationService;
    }

    public void setCatalogSynchronizationService(CatalogSynchronizationService catalogSynchronizationService) {
        this.catalogSynchronizationService = catalogSynchronizationService;
    }

    public SyncConfig getSyncConfig() {
        return syncConfig;
    }

    public void setSyncConfig(SyncConfig syncConfig) {
        this.syncConfig = syncConfig;
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

    @Override
    public CatalogTypeService getCatalogTypeService() {
        return catalogTypeService;
    }

    @Override
    public void setCatalogTypeService(CatalogTypeService catalogTypeService) {
        this.catalogTypeService = catalogTypeService;
    }

    @Override
    public CatalogVersionService getCatalogVersionService() {
        return catalogVersionService;
    }

    @Override
    public void setCatalogVersionService(CatalogVersionService catalogVersionService) {
        this.catalogVersionService = catalogVersionService;
    }

    @Override
    public CommonI18NService getCommonI18NService() {
        return commonI18NService;
    }

    @Override
    public void setCommonI18NService(CommonI18NService commonI18NService) {
        this.commonI18NService = commonI18NService;
    }

    @Override
    public SynchronizationStatusService getPlatformSynchronizationStatusService() {
        return platformSynchronizationStatusService;
    }

    @Override
    public void setPlatformSynchronizationStatusService(SynchronizationStatusService platformSynchronizationStatusService) {
        this.platformSynchronizationStatusService = platformSynchronizationStatusService;
    }

    @Override
    public RelatedItemsCollector getRelatedItemsCollector() {
        return relatedItemsCollector;
    }

    @Override
    public void setRelatedItemsCollector(RelatedItemsCollector relatedItemsCollector) {
        this.relatedItemsCollector = relatedItemsCollector;
    }

    @Override
    public SearchRestrictionService getSearchRestrictionService() {
        return searchRestrictionService;
    }

    @Override
    public void setSearchRestrictionService(SearchRestrictionService searchRestrictionService) {
        this.searchRestrictionService = searchRestrictionService;
    }

    @Override
    public SessionSearchRestrictionsDisabler getSessionSearchRestrictionsDisabler() {
        return sessionSearchRestrictionsDisabler;
    }

    @Override
    public void setSessionSearchRestrictionsDisabler(SessionSearchRestrictionsDisabler sessionSearchRestrictionsDisabler) {
        this.sessionSearchRestrictionsDisabler = sessionSearchRestrictionsDisabler;
    }

    @Override
    public SessionService getSessionService() {
        return sessionService;
    }

    @Override
    public void setSessionService(SessionService sessionService) {
        this.sessionService = sessionService;
    }
}
