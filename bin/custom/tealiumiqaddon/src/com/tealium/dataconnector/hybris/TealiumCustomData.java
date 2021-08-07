package com.tealium.dataconnector.hybris;

import static com.tealium.dataconnector.hybris.HybrisDataController.HybrisCustomDataConverter;
import static com.tealium.dataconnector.hybris.HybrisDataController.HybrisCustomPageTypeCustomData;
import de.hybris.platform.jalo.JaloSession;
import java.util.*;
import com.tealium.context.TealiumContext;
import com.tealium.util.udohelpers.UDO;
import com.tealium.util.udohelpers.exceptions.UDOUpdateException;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Vijay Vishwakarma
 * This calss is created for customize udo data as per page specific requirement.
 */
public class TealiumCustomData implements HybrisCustomDataConverter
{

	private static final Logger LOG = LoggerFactory.getLogger(TealiumCustomData.class);
	private static final String SHIPPINGPAGE = "shippingpage";
	private static final String PAYMENTPAGE = "paymentpage";
	private static final String SHIPPING_PAGE = "shippingPage";
	private static final String PAYMENT_PAGE = "paymentPage";
	private static Map<String, HybrisCustomPageTypeCustomData> customPagesMap;

	private static final ArrayList<String> productPageFields = new ArrayList<>(
			Arrays.asList("isBuy", "productCategory", "ProductName", "productSKU",
					 "prodid", "rental_days","prodOutOfStock"));

	private static final  ArrayList<String> cartPageFields = new ArrayList<>(
			Arrays.asList("cartSize", "quantity", "couponCode" ,"damage_waiver_cost","isBuy",
					"productCategory","ProductName","productSKU",
					"prodid","shipping_cost","subtotal","unit_price","total_value","rental_days" ,"out_of_stock_for_rental_dates","out_of_stock_for_quantity")); // NOSONAR


  private static final  ArrayList<String> orderConfirmationPageFields = new ArrayList<>(
      Arrays.asList("user_email", "userFirstName", "userLastName",
					"cartSize", "quantity", "couponCode" ,"damage_waiver_cost","isBuy","orderID","productCategory","ProductName","productSKU",
					"prodid","shipping_cost","subtotal","unit_price","total_value","rental_days","order_tax","isVideo")); // NOSONAR


	private static final ArrayList<String> checkoutShippingPage = new ArrayList<>(
			Arrays.asList("cartSize", "quantity", "couponCode" ,"damage_waiver_cost","isBuy","productCategory","ProductName","productSKU",
					"prodid","shipping_cost","subtotal","unit_price","total_value","rental_days"));

	private static final ArrayList<String> checkoutBillingPage = new ArrayList<>(
			Arrays.asList( "cartSize", "quantity", "couponCode" ,"damage_waiver_cost","isBuy","productCategory","ProductName","productSKU",
					"prodid","shipping_cost","subtotal","unit_price","total_value","rental_days"));

	private static final ArrayList<String> allPageFields = new ArrayList<>(
			Arrays.asList("page_type","pagetype", "AccountID", "global_url", "navigation_type", "page_name","tealium_account","utag_main_mycookie"
			));

	private static final ArrayList<String> arrayValues = new ArrayList<>(Arrays.asList(
			"product_price", "product_quantity", "page_category_name", "product_brand","couponCode","quantity","unit_price","prodid","isVideo"
	     ,"out_of_stock_for_rental_dates","out_of_stock_for_quantity"));

	private TealiumContext context;




	@Override
	public Map<String, HybrisCustomPageTypeCustomData> getHybrisCustomPageTypes()
	{
		return customPagesMap;
	}

	@Override
	public UDO homePage(UDO udo)
	{
		defaultDataSources(udo);
	  context.clean();
		return udo;
	}

	@Override
	public UDO genericPage(UDO udo)
	{
		defaultDataSources(udo);
		context.clean();
		return udo;
	}

	@Override
	public UDO searchPage(UDO udo)
	{
		return udo;
	}

	@Override
	public UDO categoryPage(UDO udo)
	{
		return udo;
	}

	@Override
	public UDO productPage(UDO udo)
	{
		defaultDataSources(udo);
		fillUdo(udo, context, productPageFields);
		context.clean();
		return udo;
	}

	@Override
	public UDO cartPage(UDO udo)
	{
		defaultDataSources(udo);
		fillUdo(udo, context, cartPageFields);
		context.clean();
		return udo;
	}

	@Override
	public UDO orderConfirmationPage(UDO udo)
	{
		defaultDataSources(udo);
		fillUdo(udo, context, orderConfirmationPageFields);
		context.clean();
		return udo;
	}

	@Override
	public UDO customerDetailPage(UDO udo)
	{
		return udo;
	}

	@Override
	public void addCustomPages()
	{
		if (customPagesMap == null)
		{
			customPagesMap = new HashMap<>();
		}
		customPagesMap.put(SHIPPINGPAGE, new HybrisCustomPageTypeCustomData(){

			@Override
			public UDO getCustomDataUdo(UDO udo) {
					defaultDataSources(udo);
					fillUdo(udo, context, checkoutShippingPage);
					context.clean();
				  return udo;
			}
		});

		customPagesMap.put(PAYMENTPAGE, new HybrisCustomPageTypeCustomData(){
			@Override
			public UDO getCustomDataUdo(UDO udo) {
					defaultDataSources(udo);
					fillUdo(udo, context, checkoutBillingPage);
					context.clean();
				return udo;
			}
		});

	}

	private UDO defaultDataSources(UDO udo)
	{
		TealiumContext tealiumContext = (TealiumContext) JaloSession.getCurrentSession().getAttribute("tealiumContext");
		this.context = tealiumContext;
		String pageType = context.getAttributes().get("pagetype");
		fillUdo(udo, context, allPageFields);
		if(SHIPPING_PAGE.equals(pageType)){
			fillUdo(udo, context, checkoutShippingPage);
		}
		if(PAYMENT_PAGE.equals(pageType)){
			fillUdo(udo, context, checkoutBillingPage);
		}
		return udo;
	}

	private synchronized void fillUdo(UDO udo, TealiumContext context, List<String> requiredFields)
	{
		requiredFields.forEach((key) -> {
			try
			{
				if (arrayValues.contains(key) && context.arrayValue(key) != null )
				{
						String[] arr = context.arrayValue(key);
						for (int i = 0; i < arr.length; i++)
						{
							if (StringUtils.isEmpty(arr[i]))
							{
								arr[i] = "null";
							}
						}
						udo.addArrayValues(key, context.arrayValue(key));
				}
				else
				{
					if (context.value(key) != null)
					{
						udo.setValue(key, context.value(key));
						context.remove(key);
					}
				}
			}
			catch (UDOUpdateException e)
			{
				LOG.error("tealium cannot add value",e);
			}
		});
	}


}
