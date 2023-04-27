/**
 *
 */
package com.bl.core.service.invoice.impl;

import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.service.invoice.GenerateInvoicePdfService;
import com.bl.core.services.upsscrape.impl.DefaultUPSScrapeService;
import com.bl.logging.BlLogger;
import com.itextpdf.text.BadElementException;
import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Element;
import com.itextpdf.text.Font;
import com.itextpdf.text.Image;
import com.itextpdf.text.PageSize;
import com.itextpdf.text.Paragraph;
import com.itextpdf.text.Phrase;
import com.itextpdf.text.pdf.BaseFont;
import com.itextpdf.text.pdf.GrayColor;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.pdf.PdfWriter;
import com.itextpdf.text.pdf.draw.LineSeparator;


/**
 * @author Admin
 *
 */
public class DefaultBlGenerateInvoicePdfService implements GenerateInvoicePdfService
{

	private static final Logger LOG = Logger.getLogger(DefaultUPSScrapeService.class);
	private static final String DEFAULT_VALUE = "$0.00";

	DecimalFormat formatter = new DecimalFormat("#0.00");
	static final ClassLoader loader = DefaultBlGenerateInvoicePdfService.class.getClassLoader();



	@Override
	public void generateInvoicePdf(final OrderData orderDetails, final HttpServletRequest request,
			final HttpServletResponse response)
	{
		try
		{
			final Document document = new Document();

			final ByteArrayOutputStream baos = new ByteArrayOutputStream();
			PdfWriter.getInstance(document, baos);
			document.setMargins(30, 30, 35, 30);
			document.setPageSize(PageSize.LETTER);
			document.addTitle("BL Invoice");
			document.open();

			addContent(document, orderDetails);

			document.close();

			response.setHeader("Expires", "0");
			response.setHeader("Cache-Control", "must-revalidate, post-check=0, pre-check=0");
			response.setHeader("Pragma", "public");
			// setting the content type
			response.setContentType("application/pdf");
			// the contentlength
			response.setContentLength(baos.size());
			// write ByteArrayOutputStream to the ServletOutputStream
			OutputStream os = null;
			os = response.getOutputStream();
			baos.writeTo(os);
			os.flush();
			os.close();
		}
		catch (final DocumentException e)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error while generating the PDF", e.getMessage());
		}
		catch (final IOException e)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "IO Exception while generting the Invoice  ", e.getMessage());
		}

	}


	/**
	 * @param document
	 * @param orderDetails
	 * @throws IOException
	 * @throws DocumentException
	 */
	private void addContent(final Document document, final OrderData orderDetails) throws IOException, DocumentException
	{

		BaseFont base = null;
		try
		{
			final String fileName = "fonts/calibril.ttf";
			loader.getResource(fileName).toString();
			base = BaseFont.createFont(loader.getResource(fileName).toString(), BaseFont.WINANSI, false);
		}
		catch (final DocumentException e)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error in Document while creating Font  ", e.getMessage());
		}
		catch (final IOException e)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error during Font creation  ", e.getMessage());
		}

		final Font fontTop = new Font(base, 12.00f, Font.BOLD);
		final Font fontTop1 = new Font(base, 12.00f, Font.NORMAL);
		final Font fontTopright1 = new Font(base, 9.96f, Font.BOLD);
		final Font fontTopright2 = new Font(base, 11.04f, Font.NORMAL);

		final Font fontMiddleHeading = new Font(base, 8.04f, Font.NORMAL);
		final Font fontbottomLeft = new Font(base, 9.96f, Font.NORMAL);
		final Font fontbottomRight = new Font(base, 11.04f, Font.BOLD);


		addTopContent(document, fontTop, fontTop1, fontTopright1, fontTopright2, orderDetails);

		addMiddleContent(document, fontMiddleHeading, fontTopright2, orderDetails);
		/// products
		addProductContent(document, fontMiddleHeading, fontTopright2, fontTopright1, fontbottomLeft, fontbottomRight, orderDetails);
		//totals at the bottom


	}

	/**
	 * @param document
	 * @param fontTop
	 * @param fontTop1
	 * @param fontTopright1
	 * @param fontTopright2
	 * @throws IOException
	 * @throws BadElementException
	 */
	private void addTopContent(final Document document, final Font fontTop, final Font fontTop1, final Font fontTopright1,
			final Font fontTopright2, final OrderData orderDetails) throws BadElementException, IOException
	{
		final float[] twoColumnWidth =
		{ 1.5f, 0.5f, 0.9f };

		final PdfPTable topTable = new PdfPTable(twoColumnWidth);
		topTable.setWidthPercentage(100);
		topTable.getDefaultCell().setBorderWidth(0f);
		topTable.setHorizontalAlignment(Element.ALIGN_LEFT);

		final PdfPTable topTableImage = new PdfPTable(2);
		topTableImage.getDefaultCell().setBorderWidth(0f);
		final String img12 = loader.getResource("images/BL-LOGO.jpg").toString();
		final String imgURl = img12;
		final String imageFile = imgURl;
		try
		{
			final Image img = Image.getInstance(imageFile);
			img.scaleToFit(100, 150);
			topTableImage.addCell(img);
		}
		catch (final MalformedURLException e)
		{
			e.printStackTrace();
		}
		final PdfPTable blAddTable = new PdfPTable(1);

		final PdfPCell blAddresLine1 = new PdfPCell(new Phrase("BorrowLenses", fontTop));
		blAddresLine1.setPaddingTop(0);
		blAddresLine1.setPaddingRight(5);
		blAddresLine1.setPaddingLeft(5);
		blAddresLine1.setPaddingBottom(5);
		blAddresLine1.setBorder(0);
		blAddTable.addCell(blAddresLine1);

		final PdfPCell blAddresLine2 = new PdfPCell(new Phrase("1664 Industrial Road", fontTop1));
		blAddresLine2.setPadding(5);
		blAddresLine2.setBorder(0);
		blAddTable.addCell(blAddresLine2);

		final PdfPCell blAddresCity = new PdfPCell(new Phrase("San Carlos, CA 94070", fontTop1));
		blAddresCity.setPadding(5);
		blAddresCity.setBorder(0);
		blAddTable.addCell(blAddresCity);

		final PdfPCell blAddressPhone = new PdfPCell(new Phrase("844-853-6737", fontTop1));
		blAddressPhone.setPadding(5);
		blAddressPhone.setBorder(0);
		blAddTable.addCell(blAddressPhone);
		topTableImage.addCell(blAddTable);

		topTable.addCell(topTableImage);

		final PdfPTable emptyTable1 = new PdfPTable(1);
		topTable.addCell(emptyTable1);

		final PdfPTable orderInfoTable = new PdfPTable(2);
		orderInfoTable.setHorizontalAlignment(Element.ALIGN_RIGHT);

		final PdfPCell orderInfoTable1 = new PdfPCell(new Phrase("Order #", fontTopright1));
		orderInfoTable1.setBorder(0);
		orderInfoTable1.setHorizontalAlignment(Element.ALIGN_RIGHT);
		orderInfoTable1.setPadding(5);
		orderInfoTable.addCell(orderInfoTable1);

		final PdfPCell orderInfoTable2 = new PdfPCell(new Phrase(orderDetails.getCode(), fontTopright2));
		orderInfoTable2.setHorizontalAlignment(Element.ALIGN_CENTER);
		orderInfoTable2.setPadding(5);
		orderInfoTable.addCell(orderInfoTable2);

		final PdfPCell orderInfoTableOrderDate = new PdfPCell(new Phrase("Order Date", fontTopright1));
		orderInfoTableOrderDate.setBorder(0);
		orderInfoTableOrderDate.setHorizontalAlignment(Element.ALIGN_RIGHT);
		orderInfoTableOrderDate.setPadding(5);
		orderInfoTable.addCell(orderInfoTableOrderDate);

		final String orderDate = orderDetails.getOrderedFormatDate();//Feb 17 , 2023	12:13 PM
		final SimpleDateFormat fromUser = new SimpleDateFormat("MMM dd , yyyy h:mm a");
		final SimpleDateFormat myFormat = new SimpleDateFormat("MM/d/yyyy");
		try
		{
			final String reformattedStr = myFormat.format(fromUser.parse(orderDate));
			final PdfPCell orderInfoTableDate = new PdfPCell(new Phrase(reformattedStr, fontTopright2));
			orderInfoTableDate.setHorizontalAlignment(Element.ALIGN_CENTER);
			orderInfoTableDate.setPadding(5);
			orderInfoTable.addCell(orderInfoTableDate);
		}
		catch (final ParseException ex)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error while parsing the Order Date  ", ex.getMessage());
		}


		final PdfPCell orderInfoTable3 = new PdfPCell(new Phrase("Rental Start Date", fontTopright1));
		orderInfoTable3.setBorder(0);
		orderInfoTable3.setHorizontalAlignment(Element.ALIGN_RIGHT);
		orderInfoTable3.setPadding(5);
		orderInfoTable.addCell(orderInfoTable3);


		PdfPCell orderInfoTable4 = new PdfPCell();
		if (null != orderDetails.getRentalStartDate())
		{//
			final SimpleDateFormat fromUser1 = new SimpleDateFormat("yyyy ,MM, dd");
			try
			{
				final String reformattedStr1 = myFormat.format(fromUser1.parse(orderDetails.getRentalStartDateForJs()));
				orderInfoTable4 = new PdfPCell(new Phrase(reformattedStr1, fontTopright2));
			}
			catch (final ParseException ex)
			{
				BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error while parsing the Rental Date  ", ex.getMessage());
			}

		}
		else
		{
			orderInfoTable4 = new PdfPCell(new Phrase("SOLD ORDER", fontTopright2));
		}
		orderInfoTable4.setHorizontalAlignment(Element.ALIGN_CENTER);
		orderInfoTable4.setPadding(5);
		orderInfoTable.addCell(orderInfoTable4);


		final PdfPCell orderInfoTable5 = new PdfPCell(new Phrase("Rental End Date", fontTopright1));
		orderInfoTable5.setBorder(0);
		orderInfoTable5.setHorizontalAlignment(Element.ALIGN_RIGHT);
		orderInfoTable5.setPadding(5);
		orderInfoTable.addCell(orderInfoTable5);

		PdfPCell orderInfoTable6 = null;
		if (null != orderDetails.getRentalEndDate())
		{

			orderInfoTable6 = new PdfPCell(new Phrase(orderDetails.getRentalEndDateForJs(), fontTopright2));
		}
		else
		{
			orderInfoTable6 = new PdfPCell(new Phrase("SOLD ORDER", fontTopright2));
		}
		orderInfoTable6.setHorizontalAlignment(Element.ALIGN_CENTER);
		orderInfoTable6.setPadding(5);
		orderInfoTable.addCell(orderInfoTable6);

		topTable.addCell(orderInfoTable);
		try
		{
			document.add(topTable);
		}
		catch (final DocumentException ex)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error while adding table to the Document  ", ex.getMessage());
		}

	}

	/**
	 * @param document
	 * @param fontMiddleHeading
	 * @param fontTopright2
	 * @throws DocumentException
	 */
	private void addMiddleContent(final Document document, final Font fontMiddleHeading, final Font fontTopright2,
			final OrderData orderDetails) throws DocumentException
	{

		final float[] columnWidthsadd =
		{ 4, 0.5f, 4 };
		final PdfPTable addressTable = new PdfPTable(columnWidthsadd);
		addressTable.setWidthPercentage(100f);
		addressTable.getDefaultCell().setBorderWidth(0f);
		final float[] columnWidths1 =
		{ 1 };
		final PdfPTable billingTable = new PdfPTable(columnWidths1);
		billingTable.setHorizontalAlignment(Element.ALIGN_LEFT);
		final PdfPCell c2 = new PdfPCell(new Phrase("BILLING INFORMATION", fontMiddleHeading));
		c2.setHorizontalAlignment(Element.ALIGN_LEFT);
		c2.setBorder(0);
		billingTable.addCell(c2);

		final PdfPCell billLine1 = new PdfPCell(new Phrase(orderDetails.getPaymentInfo().getBillingAddress() != null
				? orderDetails.getPaymentInfo().getBillingAddress().getFirstName() + " "
						+ orderDetails.getPaymentInfo().getBillingAddress().getLastName()
				: "", fontTopright2));
		billLine1.setPaddingBottom(5);
		billLine1.setPaddingTop(4);
		billingTable.addCell(billLine1);

		final PdfPCell billLine2 = new PdfPCell(new Phrase(orderDetails.getPaymentInfo().getBillingAddress() != null
				? orderDetails.getPaymentInfo().getBillingAddress().getLine1()
				: "", fontTopright2));
		billLine2.setPaddingBottom(5);
		billLine2.setPaddingTop(4);
		billingTable.addCell(billLine2);

		final PdfPCell billCity = new PdfPCell(new Phrase(orderDetails.getPaymentInfo().getBillingAddress() != null
				? orderDetails.getPaymentInfo().getBillingAddress().getTown() + ", "
						+ orderDetails.getPaymentInfo().getBillingAddress().getRegion().getIsocodeShort() + " "
						+ orderDetails.getPaymentInfo().getBillingAddress().getPostalCode()
				: "", fontTopright2));
		billCity.setPaddingBottom(5);
		billCity.setPaddingTop(4);
		billingTable.addCell(billCity);

		final PdfPCell billPhone = new PdfPCell(new Phrase(orderDetails.getPaymentInfo().getBillingAddress() != null
				? orderDetails.getPaymentInfo().getBillingAddress().getPhone()
				: "", fontTopright2));
		billPhone.setPaddingBottom(5);
		billPhone.setPaddingTop(4);
		billingTable.addCell(billPhone);

		final PdfPCell billEmail = new PdfPCell(new Phrase(orderDetails.getPaymentInfo().getBillingAddress() != null
				? orderDetails.getPaymentInfo().getBillingAddress().getEmail()
				: "", fontTopright2));
		billEmail.setPaddingBottom(5);
		billEmail.setPaddingTop(4);
		billingTable.addCell(billEmail);

		final PdfPCell billPayment = new PdfPCell(new Phrase(
				orderDetails.getPaymentInfo() != null ? "Payment Method " + orderDetails.getPaymentInfo().getCardNumber() : "",
				fontTopright2));
		billPayment.setPaddingBottom(5);
		billPayment.setPaddingTop(4);
		billingTable.addCell(billPayment);

		addressTable.addCell(billingTable);

		final PdfPTable emptyTable = new PdfPTable(1);
		addressTable.addCell(emptyTable);
		document.add(new Paragraph("\n"));

		final PdfPTable shippingTable = new PdfPTable(1);
		shippingTable.setHorizontalAlignment(Element.ALIGN_RIGHT);
		final PdfPCell c3 = new PdfPCell(new Phrase("SHIPPING INFORMATION", fontMiddleHeading));
		c3.setHorizontalAlignment(Element.ALIGN_LEFT);
		c3.setBorder(0);
		shippingTable.addCell(c3);

		final PdfPCell shipLine1 = new PdfPCell(
				new Phrase(orderDetails.getDeliveryAddress().getFirstName() + " " + orderDetails.getDeliveryAddress().getLastName(),
						fontTopright2));
		shipLine1.setPaddingBottom(5);
		shipLine1.setPaddingTop(4);
		shippingTable.addCell(shipLine1);

		final PdfPCell shipLine2 = new PdfPCell(new Phrase(orderDetails.getDeliveryAddress().getLine1(), fontTopright2));
		shipLine2.setPaddingBottom(5);
		shipLine2.setPaddingTop(4);
		shippingTable.addCell(shipLine2);

		final PdfPCell shipCity = new PdfPCell(new Phrase(
				orderDetails.getDeliveryAddress().getTown() + ", " + orderDetails.getDeliveryAddress().getRegion().getIsocodeShort()
						+ " " + orderDetails.getDeliveryAddress().getPostalCode(),
				fontTopright2));
		shipCity.setPaddingBottom(5);
		shipCity.setPaddingTop(4);
		shippingTable.addCell(shipCity);

		final PdfPCell shipPhone = new PdfPCell(new Phrase(orderDetails.getDeliveryAddress().getPhone(), fontTopright2));
		shipPhone.setPaddingBottom(5);
		shipPhone.setPaddingTop(4);
		shippingTable.addCell(shipPhone);

		final PdfPCell shipEmail = new PdfPCell(new Phrase(orderDetails.getDeliveryAddress().getEmail(), fontTopright2));
		shipEmail.setPaddingBottom(5);
		shipEmail.setPaddingTop(4);
		shippingTable.addCell(shipEmail);

		final PdfPCell shipPayment = new PdfPCell(new Phrase(orderDetails.getDeliveryMode().getName(), fontTopright2));
		shipPayment.setPaddingBottom(5);
		shipPayment.setPaddingTop(4);
		shippingTable.addCell(shipPayment);

		addressTable.addCell(shippingTable);

		try
		{
			document.add(addressTable);
		}
		catch (final DocumentException ex)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error while adding address to the document  ", ex.getMessage());
		}

		document.add(new Paragraph("\n"));

	}


	/**
	 * @param document
	 * @param fontMiddleHeading
	 * @param fontTopright2
	 * @param fontbottomRight
	 * @param fontbottomLeft
	 * @param fontTopright1
	 * @throws DocumentException
	 */
	private void addProductContent(final Document document, final Font fontMiddleHeading, final Font fontTopright2,
			final Font fontTopright1, final Font fontbottomLeft, final Font fontbottomRight, final OrderData orderDetails)
			throws DocumentException
	{
		/// products
		final float[] columnWidths12 =
		{ 6, 1, 2, 2, 2 };
		final PdfPTable table = new PdfPTable(columnWidths12);
		document.add(new LineSeparator(1, 100, GrayColor.LIGHT_GRAY, Element.ALIGN_CENTER, 0));
		final LineSeparator sep = new LineSeparator();
		sep.setOffset(2);
		sep.setLineColor(GrayColor.LIGHT_GRAY);
		document.add(sep);
		table.setSpacingAfter(4f);
		table.setWidthPercentage(100);
		table.setHorizontalAlignment(Element.ALIGN_CENTER);
		PdfPCell c1 = new PdfPCell(new Phrase("ITEM", fontMiddleHeading));
		c1.setHorizontalAlignment(Element.ALIGN_CENTER);
		c1.setBorder(0);
		table.addCell(c1);

		c1 = new PdfPCell(new Phrase("QTY", fontMiddleHeading));
		c1.setHorizontalAlignment(Element.ALIGN_CENTER);
		c1.setBorder(0);
		table.addCell(c1);

		c1 = new PdfPCell(new Phrase("GEAR PRICE", fontMiddleHeading));
		c1.setHorizontalAlignment(Element.ALIGN_CENTER);
		c1.setBorder(0);
		table.addCell(c1);

		c1 = new PdfPCell(new Phrase("DAMAGE WAIVER", fontMiddleHeading));
		c1.setHorizontalAlignment(Element.ALIGN_CENTER);
		c1.setBorder(0);
		table.addCell(c1);

		c1 = new PdfPCell(new Phrase("GEAR SUBTOTAL", fontMiddleHeading));
		c1.setHorizontalAlignment(Element.ALIGN_CENTER);
		c1.setBorder(0);
		table.addCell(c1);

		document.add(table);

		final PdfPTable table12 = new PdfPTable(columnWidths12);
		document.add(new LineSeparator(1, 100, GrayColor.LIGHT_GRAY, Element.ALIGN_CENTER, 0));

		sep.setOffset(2);
		sep.setLineColor(GrayColor.LIGHT_GRAY);
		document.add(sep);
		table12.setSpacingBefore(4f);
		table12.setWidthPercentage(100);

		Double totalDamageWaiverPrice = 0.00;
		for (final OrderEntryData oe : orderDetails.getEntries())
		{

			final PdfPCell itemCell = new PdfPCell(new Phrase(oe.getProduct().getName(), fontTopright2)); //"Canon EF 200-400mm f/4L IS with 1.4x Extender"
			itemCell.setHorizontalAlignment(Element.ALIGN_CENTER);
			itemCell.setPadding(5);
			table12.addCell(itemCell);

			final PdfPCell qtyCell = new PdfPCell(new Phrase(String.valueOf(oe.getQuantity()), fontTopright2));//1
			qtyCell.setHorizontalAlignment(Element.ALIGN_CENTER);
			qtyCell.setPadding(5);
			table12.addCell(qtyCell);

			final PdfPCell gearPriceCell = new PdfPCell(
					new Phrase(String.valueOf(oe.getTotalPrice().getFormattedValue()), fontTopright2));
			gearPriceCell.setHorizontalAlignment(Element.ALIGN_CENTER);
			gearPriceCell.setPadding(5);
			table12.addCell(gearPriceCell);

			double damageWaiverPrice = 0.00;
			PdfPCell damageCell = null;
			if (!BlCoreConstants.AQUATECH_BRAND_ID.equals(oe.getProduct().getManufacturerAID()))
			{
				if (BooleanUtils.isTrue(oe.getGearGuardProFullWaiverSelected()))
				{
					damageWaiverPrice = oe.getGearGuardProFullWaiverPrice().getValue().doubleValue();
					damageCell = new PdfPCell(new Phrase("$" + formatter.format(damageWaiverPrice), fontTopright2));
					totalDamageWaiverPrice = totalDamageWaiverPrice + damageWaiverPrice;
				}
				else if (BooleanUtils.isTrue(oe.getGearGuardWaiverSelected()))
				{
					damageWaiverPrice = oe.getGearGuardWaiverPrice().getValue().doubleValue();
					damageCell = new PdfPCell(new Phrase("$" + formatter.format(damageWaiverPrice), fontTopright2));
					totalDamageWaiverPrice = totalDamageWaiverPrice + damageWaiverPrice;
				}
				else
				{
					damageCell = new PdfPCell(new Phrase("$" + formatter.format(damageWaiverPrice), fontTopright2));
				}
			}
			else
			{
				damageCell = new PdfPCell(new Phrase("$" + formatter.format(damageWaiverPrice), fontTopright2));
			}

			damageCell.setHorizontalAlignment(Element.ALIGN_CENTER);
			damageCell.setPadding(5);
			table12.addCell(damageCell);

			final PdfPCell gearSubTotalCell = new PdfPCell(
					new Phrase("$" + String.valueOf(formatter.format(oe.getTotalPrice().getValue().doubleValue() + damageWaiverPrice)),
							fontTopright2));
			gearSubTotalCell.setHorizontalAlignment(Element.ALIGN_CENTER);
			gearSubTotalCell.setPadding(5);
			gearSubTotalCell.setBackgroundColor(new BaseColor(226, 226, 226));
			table12.addCell(gearSubTotalCell);
		}

		document.add(table12);

		document.add(new Paragraph("\n"));

		//totals at the bottom
		final float[] columnWidth3 =
		{ 4, 1 };
		final PdfPTable totalTable = new PdfPTable(columnWidth3);
		totalTable.setHorizontalAlignment(Element.ALIGN_RIGHT);

		final PdfPCell totaltableCell1 = new PdfPCell(new Phrase("GEAR TOTAL", fontbottomLeft));
		totaltableCell1.setBorder(0);
		totaltableCell1.setHorizontalAlignment(Element.ALIGN_RIGHT);
		totaltableCell1.setPadding(5);
		totalTable.addCell(totaltableCell1);

		final PdfPCell totaltableGearTotal = new PdfPCell(new Phrase(
				"$" + String.valueOf(formatter.format(orderDetails.getSubTotal().getValue().doubleValue())), fontTopright2));
		totaltableGearTotal.setHorizontalAlignment(Element.ALIGN_CENTER);
		totaltableGearTotal.setPadding(6);
		totalTable.addCell(totaltableGearTotal);

		final PdfPCell totaltableCell2 = new PdfPCell(new Phrase("DAMAGE WAIVER TOTAL", fontbottomLeft));
		totaltableCell2.setBorder(0);
		totaltableCell2.setHorizontalAlignment(Element.ALIGN_RIGHT);
		totaltableCell2.setPadding(5);
		totalTable.addCell(totaltableCell2);

		final PdfPCell totaltableDmTotal = new PdfPCell(new Phrase("$" + formatter.format(totalDamageWaiverPrice), fontTopright2));
		totaltableDmTotal.setHorizontalAlignment(Element.ALIGN_CENTER);
		totaltableDmTotal.setPadding(6);
		totalTable.addCell(totaltableDmTotal);

		final PdfPCell totaltableCell3 = new PdfPCell(new Phrase("SHIPPING", fontbottomLeft));
		totaltableCell3.setBorder(0);
		totaltableCell3.setHorizontalAlignment(Element.ALIGN_RIGHT);
		totaltableCell3.setPadding(5);
		totalTable.addCell(totaltableCell3);

		final PdfPCell totaltableShipping = new PdfPCell(new Phrase(orderDetails.getDeliveryCost() != null
				? "$" + String.valueOf(formatter.format(orderDetails.getDeliveryCost().getValue().doubleValue()))
				: DEFAULT_VALUE, fontTopright2));
		totaltableShipping.setHorizontalAlignment(Element.ALIGN_CENTER);
		totaltableShipping.setPadding(6);
		totalTable.addCell(totaltableShipping);

		final PdfPCell totaltableCell4 = new PdfPCell(new Phrase("TAX", fontbottomLeft));
		totaltableCell4.setBorder(0);
		totaltableCell4.setHorizontalAlignment(Element.ALIGN_RIGHT);
		totaltableCell4.setPadding(5);
		totalTable.addCell(totaltableCell4);

		final PdfPCell totaltableGiftCardTax = new PdfPCell(new Phrase(
				"$" + String.valueOf(formatter.format(orderDetails.getTotalTax().getValue().doubleValue())), fontTopright2));
		totaltableGiftCardTax.setHorizontalAlignment(Element.ALIGN_CENTER);
		totaltableGiftCardTax.setPadding(6);
		totalTable.addCell(totaltableGiftCardTax);

		final PdfPCell totaltableCell5 = new PdfPCell(new Phrase("DISCOUNT", fontbottomLeft));
		totaltableCell5.setBorder(0);
		totaltableCell5.setHorizontalAlignment(Element.ALIGN_RIGHT);
		totaltableCell5.setPadding(5);
		totalTable.addCell(totaltableCell5);

		final PdfPCell totaltableDiscount = new PdfPCell(new Phrase(orderDetails.getTotalDiscounts() != null
				? "-" + "$" + String.valueOf(formatter.format(orderDetails.getTotalDiscounts().getValue().doubleValue()))
				: DEFAULT_VALUE, fontTopright2));
		totaltableDiscount.setHorizontalAlignment(Element.ALIGN_CENTER);
		totaltableDiscount.setPadding(6);
		totalTable.addCell(totaltableDiscount);

		final PdfPCell totaltableCell6 = new PdfPCell(new Phrase("GIFT CARD", fontbottomLeft));
		totaltableCell6.setBorder(0);
		totaltableCell6.setHorizontalAlignment(Element.ALIGN_RIGHT);
		totaltableCell6.setPadding(5);
		totalTable.addCell(totaltableCell6);

		final PdfPCell totaltableGiftCard;
		if (CollectionUtils.isNotEmpty(orderDetails.getGiftCardData()))
		{
			final Double sum = orderDetails.getGiftCardData().stream().filter(gf1 -> gf1.getRedeemamount() != null)
					.mapToDouble(gf1 -> gf1.getRedeemamount().getValue().doubleValue()).sum();

			totaltableGiftCard = new PdfPCell(
					new Phrase("$" + String.valueOf(formatter.format(sum)).replace("-", ""), fontTopright2));
		}
		else
		{
			totaltableGiftCard = new PdfPCell(new Phrase(DEFAULT_VALUE, fontTopright2));
		}

		totaltableGiftCard.setHorizontalAlignment(Element.ALIGN_CENTER);
		totaltableGiftCard.setPadding(6);
		totalTable.addCell(totaltableGiftCard);

		final PdfPCell totaltableCell7 = new PdfPCell(new Phrase("ORDER TOTAL", fontTopright1));
		totaltableCell7.setBorder(0);
		totaltableCell7.setHorizontalAlignment(Element.ALIGN_RIGHT);
		totaltableCell7.setPadding(5);
		totalTable.addCell(totaltableCell7);

		final PdfPCell totaltableOrderTotal = new PdfPCell(new Phrase(
				"$" + String.valueOf(formatter.format(orderDetails.getTotalPrice().getValue().doubleValue())), fontbottomRight));
		totaltableOrderTotal.setHorizontalAlignment(Element.ALIGN_CENTER);
		totaltableOrderTotal.setPadding(6);
		totaltableOrderTotal.setBackgroundColor(new BaseColor(226, 226, 226));
		totalTable.addCell(totaltableOrderTotal);

		document.add(totalTable);

	}

}
