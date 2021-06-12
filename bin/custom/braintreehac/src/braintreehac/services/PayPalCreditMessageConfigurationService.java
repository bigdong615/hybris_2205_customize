package braintreehac.services;

import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.contents.components.SimpleCMSComponentModel;
import de.hybris.platform.cms2.model.site.CMSSiteModel;
import de.hybris.platform.cms2.servicelayer.services.CMSComponentService;
import de.hybris.platform.cms2.servicelayer.services.CMSSiteService;
import de.hybris.platform.cms2.servicelayer.services.admin.CMSAdminSiteService;
import de.hybris.platform.cmsfacades.cmsitems.CMSItemFacade;
import de.hybris.platform.core.model.ItemModel;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import java.util.*;

@Component
public class PayPalCreditMessageConfigurationService {

    @Resource(name = "cmsAdminSiteService")
    private CMSAdminSiteService cmsAdminSiteService;

    @Resource(name = "cmsSiteService")
    private CMSSiteService cmsSiteService;

    @Resource(name = "catalogVersionService")
    private CatalogVersionService catalogVersionService;

    @Resource(name = "cmsItemFacade")
    private CMSItemFacade cmsItemFacade;

    @Resource(name = "cmsComponentService")
	private CMSComponentService cmsComponentService;

    @Resource(name = "brainTreeCatalogSynchronizationService")
    private BrainTreeCatalogSynchronizationService brainTreeCatalogSynchronizationService;

    private List<String> color = new ArrayList<>();

    private List<String> ratio = new ArrayList<>();

    private List<String> layout = new ArrayList<>();

    private List<String> logoType = new ArrayList<>();

    private List<String> logoPosition = new ArrayList<>();

    private List<String> textColor = new ArrayList<>();


    private static final Logger LOG = Logger.getLogger(PayPalCreditMessageConfigurationService.class);

    @PostConstruct
    private void onInit(){
        Collections.addAll(color, "blue", "black", "white", "white-no-border", "monochrome", "gray", "grayscale");
        Collections.addAll(ratio, "1x1", "1x4", "8x1", "20x1");
        Collections.addAll(layout, "TEXT", "FLEX");
        Collections.addAll(logoType, "PRIMARY", "ALTERNATIVE", "INLINE", "NONE");
        Collections.addAll(logoPosition, "LEFT", "RIGHT", "TOP");
        Collections.addAll(textColor, "BLACK", "WHITE", "MONOCHROME", "GRAYSCALE");
    }

    protected static final String INCORRECT_CMS_SITE_CHANNEL_ERROR_MESSAGE = "Matched CMSSite for current URL has unsupported channel";

    public  Map<String, Object> getCreditMessageItemByUid(final SimpleCMSComponentModel componentByUid) throws Exception
    {
        setActiveCatalogVersion(componentByUid.getCatalogVersion());
        return cmsItemFacade.getCMSItemByUuid(componentByUid.getUid());
    }

    public SimpleCMSComponentModel getComponentByUid(final String uid)
    {
        try {
            return cmsComponentService.getSimpleCMSComponent(uid);
        } catch (CMSItemNotFoundException e) {
            LOG.error("Wasn't found CMS item with uid - " + uid);
        }
        return null;
    }

    public void processComponentUpdating(final String creditMessageComponentUid,  Map<String, String[]> refreshedParameters) throws Exception {
        final SimpleCMSComponentModel creditMessageComponent = getComponentByUid(creditMessageComponentUid);
        final SimpleCMSComponentModel creditMessageComponentB2B = getComponentByUid(creditMessageComponentUid + "B2B");
        List<ItemModel> itemsToSynchronize = new ArrayList<>();

        updateComponent(creditMessageComponent, refreshedParameters);

        if (creditMessageComponentB2B != null){
            updateComponent(creditMessageComponentB2B, refreshedParameters);
            itemsToSynchronize.add(creditMessageComponentB2B);
            synchronizeComponent(itemsToSynchronize);
            itemsToSynchronize.remove(0);
        }

        itemsToSynchronize.add(creditMessageComponent);
        synchronizeComponent(itemsToSynchronize);
    }

    private void synchronizeComponent(List<ItemModel> itemsToSynchronize){
        final CatalogVersionModel source = getActiveCatalogVersion();
        final CatalogVersionModel target = getTargetCatalogVersion(source);

        getBrainTreeCatalogSynchronizationService().synchronizeItems(itemsToSynchronize, source, target);
    }

    public Map<String, Object> updatePayPalComponentValues(Map<String, Object> creditMessageItem,
                                                           Map<String, String[]> refreshedParameters)
    {
        LOG.warn("Start component update process");

        Set<String> refreshedParametersKeys = refreshedParameters.keySet();
        Iterator<String> iterator = refreshedParametersKeys.iterator();

        while (iterator.hasNext()){
            String refreshedParameterKey = iterator.next();
            String refreshedParameterValue = Arrays.stream(refreshedParameters.get(refreshedParameterKey)).findFirst().get();
            if (creditMessageItem.containsKey(refreshedParameterKey)) {
                creditMessageItem.replace(refreshedParameterKey, refreshedParameterValue);
            }else {
                creditMessageItem.put(refreshedParameterKey, refreshedParameterValue);
            }
        }

        return creditMessageItem;
    }

    public void updatePayPalComponentItem(final String uuid, final Map<String, Object> updatedPayPalCreditMessageComponent) throws Exception
    {
        cmsItemFacade.updateItem(uuid, updatedPayPalCreditMessageComponent);
        LOG.warn("Updated component");
    }

    private void updateComponent(SimpleCMSComponentModel creditMessageComponent, Map<String, String[]> refreshedParameters) throws Exception {
        Map<String, Object> updatedCreditMessageValues =
                updatePayPalComponentValues(getCreditMessageItemByUid(creditMessageComponent), refreshedParameters);

        updatePayPalComponentItem(updatedCreditMessageValues.get("uuid").toString(),
                updatedCreditMessageValues);

    }

    private void setActiveCatalogVersion(final CatalogVersionModel catalogVersion){
            String catalogID = catalogVersion.getCatalog().getId();
            String versionName = "Staged";
            CatalogVersionModel version = getCatalogVersionService().getCatalogVersion(catalogID, versionName);
        getCmsAdminSiteService().setActiveCatalogVersion(version);
    }

    public CatalogVersionModel getActiveCatalogVersion(){
       return getCmsAdminSiteService().getActiveCatalogVersion();
    }

    public CatalogVersionModel getTargetCatalogVersion(final CatalogVersionModel sourceCatalogVersion){
        String catalogId = sourceCatalogVersion.getCatalog().getId();
        String versionName = "Online";
        if ("Online".equals(sourceCatalogVersion.getVersion())){
            versionName = "Staged";
        }
        return getCatalogVersionService().getCatalogVersion(catalogId, versionName);
    }

    public boolean processNormalRequest()
    {
        Collection<CatalogModel> allCatalogs = new ArrayList<>();
        Collection<CatalogVersionModel> catalogVersions = new ArrayList<>();
        for (CMSSiteModel site : getCmsSiteService().getSites()) {
            allCatalogs.addAll(getCmsSiteService().getAllCatalogs(site));
            for (CatalogModel catalog : getCmsSiteService().getAllCatalogs(site)) {
               catalogVersions.add(catalog.getActiveCatalogVersion());
            }
        }

        getCatalogVersionService().setSessionCatalogVersions(catalogVersions);

        return true;
    }

    public List<String> getColor() {
        return color;
    }

    public List<String> getRatio() {
        return ratio;
    }

    public List<String> getLayout() {
        return layout;
    }

    public List<String> getLogoType() {
        return logoType;
    }

    public List<String> getLogoPosition() {
        return logoPosition;
    }

    public List<String> getTextColor() {
        return textColor;
    }

    public CMSSiteService getCmsSiteService() {
        return cmsSiteService;
    }

    public void setCmsSiteService(CMSSiteService cmsSiteService) {
        this.cmsSiteService = cmsSiteService;
    }

    public CMSAdminSiteService getCmsAdminSiteService() {
        return cmsAdminSiteService;
    }

    public void setCmsAdminSiteService(CMSAdminSiteService cmsAdminSiteService) {
        this.cmsAdminSiteService = cmsAdminSiteService;
    }

    public CatalogVersionService getCatalogVersionService() {
        return catalogVersionService;
    }

    public void setCatalogVersionService(CatalogVersionService catalogVersionService) {
        this.catalogVersionService = catalogVersionService;
    }

    public BrainTreeCatalogSynchronizationService getBrainTreeCatalogSynchronizationService() {
        return brainTreeCatalogSynchronizationService;
    }

    public void setBrainTreeCatalogSynchronizationService(BrainTreeCatalogSynchronizationService brainTreeCatalogSynchronizationService) {
        this.brainTreeCatalogSynchronizationService = brainTreeCatalogSynchronizationService;
    }
}
