package com.bl.backoffice.actions;

import de.hybris.platform.acceleratorservices.urlresolver.SiteBaseUrlResolutionService;
import de.hybris.platform.core.PK;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.exceptions.ModelLoadingException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.site.BaseSiteService;

import java.net.URLEncoder;
import java.util.Objects;

import javax.annotation.Resource;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Executions;
import org.zkoss.zul.Messagebox;

import com.bl.core.constants.BlCoreConstants;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


/**
 * This class is responsible to print shipping label
 *
 * @author Aditi Sharma
 */

public class PrintLabelAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
	private static final Logger LOG = Logger.getLogger(PrintLabelAction.class);
	@Resource(name = "modelService")
	private ModelService modelService;

	@Resource(name="siteBaseUrlResolutionService")
	private SiteBaseUrlResolutionService siteBaseUrlResolutionService;
	
	@Resource(name="baseSiteService")
	private BaseSiteService baseSiteService;

	protected static final String SOCKET_OUT_CONTEXT = "blPrintLabelContext";
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";
	private static final String SITE_URL = "website.bl.https";

	/**
	 * This method is responsible for fetch the consignment which are not in CANCELLED, CHECKED_INVALID,
	 * PAYMENT_NOT_AUTHORIZED and PAYMENT_DECLINED status
	 *
	 * @param actionContext
	 *           the action context
	 * @return the boolean
	 */

	@Override
	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel consignemtModel = actionContext.getData();

		return (consignemtModel != null);
	}

	/**
	 * This method will fetch the action context data for blPrintLabelContext
	 *
	 * @param actionContext
	 *           the action context
	 * @return the action result
	 */
	public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext)
	{
		try
		{
		final ConsignmentModel consignment = actionContext.getData();
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Printing os shipment label started for consignment {}", consignment.getCode());
		final String pk = consignment.getPk().toString();
		Executions.getCurrent().sendRedirect( getSiteBaseUrlResolutionService()
            .getWebsiteUrlForSite(getBaseSiteService().getBaseSiteForUID(BlCoreConstants.BASE_STORE_ID),
                  StringUtils.EMPTY, Boolean.TRUE, "/shipment/printLabel",
                  "code=".concat(pk)), "_blank");
		Messagebox.show("Printing of shipment label is done for consignment " + consignment.getCode(), "Info", Messagebox.OK, "icon");
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
		}
		catch (final Exception e) {
			Messagebox.show("Error occured while printing shipment label for consignment " + actionContext.getData().getCode(), "Info", Messagebox.OK, "icon");
			return new ActionResult(BlintegrationConstants.SUCCESS);
		}
		return new ActionResult(BlintegrationConstants.CLIENT_SIDE_ERROR);
	}

	/**
	 * @return the siteBaseUrlResolutionService
	 */
	public SiteBaseUrlResolutionService getSiteBaseUrlResolutionService()
	{
		return siteBaseUrlResolutionService;
	}

	/**
	 * @param siteBaseUrlResolutionService the siteBaseUrlResolutionService to set
	 */
	public void setSiteBaseUrlResolutionService(SiteBaseUrlResolutionService siteBaseUrlResolutionService)
	{
		this.siteBaseUrlResolutionService = siteBaseUrlResolutionService;
	}

	/**
	 * @return the baseSiteService
	 */
	public BaseSiteService getBaseSiteService()
	{
		return baseSiteService;
	}

	/**
	 * @param baseSiteService the baseSiteService to set
	 */
	public void setBaseSiteService(BaseSiteService baseSiteService)
	{
		this.baseSiteService = baseSiteService;
	}
}
