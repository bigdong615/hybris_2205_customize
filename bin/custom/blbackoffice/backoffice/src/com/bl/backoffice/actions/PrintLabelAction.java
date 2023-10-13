package com.bl.backoffice.actions;

import de.hybris.platform.acceleratorservices.urlresolver.SiteBaseUrlResolutionService;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.site.BaseSiteService;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zul.Filedownload;
import org.zkoss.zul.Messagebox;

import com.bl.core.constants.BlCoreConstants;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
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
	
	@Resource(name="mediaService")
	private MediaService mediaService;

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
			if(isLabelsNotAvailable(consignment))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, "No labels found on consignment {}",
						consignment.getCode());
				Messagebox.show("No Labels found on consignment: " + consignment.getCode(),
						"Info", Messagebox.OK, "icon");
			}
			else
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Downloading shipment label started for consignment {}",
						consignment.getCode());
				downloadAllFiles(consignment);
			}			
			this.sendOutput(BlCoreConstants.SOCKET_OUT_CONTEXT, actionContext.getData());
		}
		catch (final Exception e)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, e,
					"PrintLabelAction :: perform :: Error occured while downloading shipment label for consignment : {}",
					actionContext.getData().getCode());
			Map params = new HashMap();
			params.put("sclass", "myMessagebox");
			Messagebox.show("Error occured while printing shipment label for consignment " + actionContext.getData().getCode(),
					"Error Occurred", new Messagebox.Button[]
							{Messagebox.Button.OK},null , Messagebox.ERROR, null,
					null, params);
			return new ActionResult<>(BlintegrationConstants.SUCCESS);
		}
		return new ActionResult<>(BlintegrationConstants.CLIENT_SIDE_ERROR);
	}
	
	private boolean isLabelsNotAvailable(final ConsignmentModel consignment)
	{
		return Objects.isNull(consignment) || CollectionUtils.isEmpty(consignment.getPackaginginfos())
				|| CollectionUtils.isEmpty(getAllLablesFromPackages(consignment));
	}
	
	/**
	 * Download all files.
	 *
	 * @param consignment the consignment
	 */
	private void downloadAllFiles(final ConsignmentModel consignment)
	{
		try
		{
			if (Objects.nonNull(consignment) && CollectionUtils.isNotEmpty(consignment.getPackaginginfos()))
			{
				final List<MediaModel> lPackageLables = getAllLablesFromPackages(consignment);				
				if (CollectionUtils.isNotEmpty(lPackageLables))
				{
					final String zipFileName = consignment.getCode() + BlCoreConstants.PACKAGE_LABELS + System.currentTimeMillis();
					final File zipFile = File.createTempFile(zipFileName, BlCoreConstants.ZIP_FILE_EXTENSION);
					if (Objects.nonNull(zipFile))
					{
						final String tempZipFileAbsolutePath = zipFile.getAbsolutePath();
						BlLogger.logFormatMessageInfo(LOG, Level.INFO,
								"PrintLabelAction :: downloadAllFiles :: Temp Zip File created at path: {}", tempZipFileAbsolutePath);
						final FileOutputStream zipFileOutputStream = new FileOutputStream(tempZipFileAbsolutePath);
						final ZipOutputStream zipOutputStream = new ZipOutputStream(zipFileOutputStream);
						addAllLabelsToZipFile(consignment, lPackageLables, zipOutputStream);
						zipOutputStream.close();
						final String downloadZipFileName = BlCoreConstants.BL_INITIAL.concat(zipFileName)
								.concat(BlCoreConstants.ZIP_FILE_EXTENSION);
						Filedownload.save(new FileInputStream(tempZipFileAbsolutePath), BlCoreConstants.ZIP_MIME_TYPE,
								downloadZipFileName);
						BlLogger.logFormatMessageInfo(LOG, Level.INFO,
								"PrintLabelAction :: downloadAllFiles :: Zip file downloaded : {}", downloadZipFileName);
					}
				}
			}
		}
		catch (final Exception e)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, e,
					"PrintLabelAction :: downloadAllFiles :: Error occured while downloading shipment label for consignment : {}",
					consignment.getCode());
		}
	}

	/**
	 * Adds the all labels to zip file.
	 *
	 * @param consignment
	 *           the consignment
	 * @param lPackageLables
	 *           the l package lables
	 * @param zipOutputStream
	 *           the zip output stream
	 */
	private void addAllLabelsToZipFile(final ConsignmentModel consignment, final List<MediaModel> lPackageLables,
			final ZipOutputStream zipOutputStream)
	{
		lPackageLables.forEach(media -> {
			final String extractedFileName = this.extractFileName(media);
			try
			{
				final InputStream streamFromMedia = this.getMediaService().getStreamFromMedia(media);
				zipOutputStream.putNextEntry(new ZipEntry(extractedFileName));
				int length;
				final byte[] buffer = new byte[1024];
				while ((length = streamFromMedia.read(buffer)) > 0)
				{
					zipOutputStream.write(buffer, 0, length);
				}
				zipOutputStream.closeEntry();
				streamFromMedia.close();
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"PrintLabelAction :: addAllLabelsToZipFile :: File : {} added to Temp Zip File", extractedFileName);
			}
			catch (final Exception e)
			{
				BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, e,
						"PrintLabelAction :: addAllLabelsToZipFile :: Error while adding file into zip for file : {} on consignment : {}",
						extractedFileName, consignment.getCode());
			}
		});
	}

	/**
	 * Gets the all lables from packages.
	 *
	 * @param consignment
	 *           the consignment
	 * @param lPackageLables
	 *           the l package lables
	 * @return the all lables from packages
	 */
	private List<MediaModel> getAllLablesFromPackages(final ConsignmentModel consignment)
	{
		final List<MediaModel> lPackageLables = Lists.newArrayList();
		consignment.getPackaginginfos().forEach(pack -> {
			final MediaModel outboubdMediaModel = pack.getOutBoundShippingMedia();
			if (Objects.nonNull(outboubdMediaModel))
			{
				lPackageLables.add(outboubdMediaModel);
			}
			final MediaModel inboubdMediaModel = pack.getInBoundShippingMedia();
			if (Objects.nonNull(inboubdMediaModel))
			{
				lPackageLables.add(inboubdMediaModel);
			}
		});
		return lPackageLables;
	}

	private String extractFileName(final MediaModel mediaModel)
	{
		return StringUtils.defaultIfBlank(mediaModel.getRealFileName(), this.createFallbackFileName(mediaModel));
	}

	private String createFallbackFileName(final MediaModel mediaModel)
	{
		return Objects.nonNull(mediaModel.getPk()) ? mediaModel.getPk().toString() : String.valueOf(System.currentTimeMillis());
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

	/**
	 * @return the mediaService
	 */
	public MediaService getMediaService()
	{
		return mediaService;
	}

	/**
	 * @param mediaService the mediaService to set
	 */
	public void setMediaService(MediaService mediaService)
	{
		this.mediaService = mediaService;
	}
}
