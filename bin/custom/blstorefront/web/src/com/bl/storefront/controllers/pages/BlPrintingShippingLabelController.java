/**
 *
 */
package com.bl.storefront.controllers.pages;

import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import java.awt.print.PrinterJob;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import javax.annotation.Resource;
import javax.print.*;
import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import com.bl.integration.facades.impl.DefaultBlPrintShippingLabelFacade;
import com.bl.storefront.controllers.ControllerConstants;


/**
 * @author Aditi This Class is responsible to print shipping label
 */

@Controller
@RequestMapping(value = "/printLabel")
public class BlPrintingShippingLabelController
{
	@Resource(name = "defaultBlPrintShippingLabelFacade")
	private DefaultBlPrintShippingLabelFacade defaultBlPrintShippingLabelFacade;

	@GetMapping
	public String printShippingLabel(@RequestParam("code")
	final String code, final HttpServletRequest request, final Model model)
	{
		final List<PackagingInfoData> packagingInfoData = getDefaultBlPrintShippingLabelFacade().getConsignmentByPk(code);
		model.addAttribute("packageData", packagingInfoData);
		return ControllerConstants.Views.Fragments.PrintShippingLabel.PrintShippingLabel;
	}

	@PostMapping(value = "/printLabelValue")
	public String printShippingPackageLabel(final HttpServletRequest request, final Model model) throws PrintException {
		final String s = request.getParameter("label");
		final PrintService[] printServices = PrinterJob.lookupPrintServices();
		final PrintService print = null;
      final List<PrintService> lp = Arrays.asList(printServices);
      final Optional<PrintService> op = lp.stream().filter(pp -> pp.getName().equalsIgnoreCase("Microsoft Print to PDF")).findFirst();
      if(op.isPresent())
      {
		for (PrintService printService : printServices) {
			DocPrintJob job = printService.createPrintJob();
			final String zplCode = s;
			DocFlavor flavor = DocFlavor.BYTE_ARRAY.AUTOSENSE;
			Doc doc = new SimpleDoc(zplCode.getBytes(), flavor, null);
			job.print(doc, null);
		}
      }
		model.addAttribute("label", s);

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
}
