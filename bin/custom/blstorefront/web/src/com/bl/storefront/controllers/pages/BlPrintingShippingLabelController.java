/**
 *
 */
package com.bl.storefront.controllers.pages;

import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import java.awt.print.PrinterJob;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import javax.annotation.Resource;
import javax.print.Doc;
import javax.print.DocFlavor;
import javax.print.DocPrintJob;
import javax.print.PrintException;
import javax.print.PrintService;
import javax.print.SimpleDoc;
import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import com.bl.integration.facades.impl.DefaultBlPrintShippingLabelFacade;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.ControllerConstants;


/**
 * @author Aditi This Class is responsible to print shipping label
 */

@Controller
@RequestMapping(value = "/shipment")
public class BlPrintingShippingLabelController
{
	private static final Logger LOG = Logger.getLogger(BlPrintingShippingLabelController.class);
	
	private static final String ZPL_PRINTER_NAME = "bl.zpl.printer.name";
	
	@Resource(name = "defaultBlPrintShippingLabelFacade")
	private DefaultBlPrintShippingLabelFacade defaultBlPrintShippingLabelFacade;
	
	@Resource(name = "configurationService")
	private ConfigurationService configurationService;

	@GetMapping(value = "/printLabel")
	public String printShippingLabel(@RequestParam("code")
	final String code, final HttpServletRequest request, final Model model)
	{
		final List<PackagingInfoData> packagingInfoData = getDefaultBlPrintShippingLabelFacade().getConsignmentByPk(code);
		model.addAttribute("packageData", packagingInfoData);
		return ControllerConstants.Views.Fragments.PrintShippingLabel.PrintShippingLabel;
	}

	@PostMapping(value = "/printLabelValue")
	public String printShippingPackageLabel(final HttpServletRequest request, final Model model) throws PrintException
	{
		final String shipmentLabelURL = request.getParameter("label");
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "shipment label URL {} in ZPL format ", shipmentLabelURL);
		final PrintService[] printServices = PrinterJob.lookupPrintServices();
		final List<PrintService> availablePrinterList = Arrays.asList(printServices);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Available printer list ", availablePrinterList);
		final Optional<PrintService> printerToUse = availablePrinterList.stream()
				.filter(printer -> printer.getName().equalsIgnoreCase(getConfigurationService().getConfiguration().getString(ZPL_PRINTER_NAME))).findFirst();
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Is ZPL Printer found ", printerToUse.isPresent());
		if (printerToUse.isPresent())
		{
			for (final PrintService printService : printServices)
			{
				final DocPrintJob job = printService.createPrintJob();
				final String zplCode = shipmentLabelURL;
				final DocFlavor flavor = DocFlavor.BYTE_ARRAY.AUTOSENSE;
				final Doc doc = new SimpleDoc(zplCode.getBytes(), flavor, null);
				job.print(doc, null);
			}
		}
		model.addAttribute("label", shipmentLabelURL);

		return ControllerConstants.Views.Fragments.PrintShippingLabel.PrintShippingPackageLabel;
	}

	/**
	 * @return the defaultBlPrintShippingLabelFacade
	 */
	public DefaultBlPrintShippingLabelFacade getDefaultBlPrintShippingLabelFacade()
	{
		return defaultBlPrintShippingLabelFacade;
	}

	/**
	 * @param defaultBlPrintShippingLabelFacade
	 *           the defaultBlPrintShippingLabelFacade to set
	 */
	public void setDefaultBlPrintShippingLabelFacade(final DefaultBlPrintShippingLabelFacade defaultBlPrintShippingLabelFacade)
	{
		this.defaultBlPrintShippingLabelFacade = defaultBlPrintShippingLabelFacade;
	}

	/**
	 * @return the configurationService
	 */
	public ConfigurationService getConfigurationService()
	{
		return configurationService;
	}

	/**
	 * @param configurationService the configurationService to set
	 */
	public void setConfigurationService(ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}
}
