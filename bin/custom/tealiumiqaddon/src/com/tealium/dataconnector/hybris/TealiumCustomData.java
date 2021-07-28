package com.tealium.dataconnector.hybris;

import static com.tealium.dataconnector.hybris.HybrisDataController.HybrisCustomDataConverter;
import static com.tealium.dataconnector.hybris.HybrisDataController.HybrisCustomPageTypeCustomData;
import de.hybris.platform.jalo.JaloSession;
import java.util.*;
import com.tealium.context.TealiumContext;
import com.tealium.util.udohelpers.UDO;
import com.tealium.util.udohelpers.exceptions.UDOUpdateException;


public class TealiumCustomData implements HybrisCustomDataConverter
{

	private static Map<String, HybrisCustomPageTypeCustomData> customPagesMap;

	private final static ArrayList<String> productPageFields = new ArrayList<>(
			Arrays.asList("isBuy", "productCategory", "ProductName", "productSKU",
					 "prodid", "rentalDays"));

	private final static ArrayList<String> cartPageFields = new ArrayList<>(
			Arrays.asList("cartSize", "quantity", "couponCode" ,"damage_waiver_cost","isBuy",
					"productCategory","ProductName","productSKU",
					"prodid","shipping_cost","subtotal","unit_price","total_value","rentalDays"));


  private final static ArrayList<String> orderConfirmationPageFields = new ArrayList<>(
      Arrays.asList("userEmail", "userFirstName", "userLastName",
					"cartSize", "quantity", "couponCode" ,"damage_waiver_cost","isBuy","orderID","productCategory","ProductName","productSKU",
					"prodid","shipping_cost","subtotal","unit_price","total_value","rentalDays"));


	private final static ArrayList<String> checkoutShippingPage = new ArrayList<>(
			Arrays.asList("cartSize", "quantity", "couponCode" ,"damage_waiver_cost","isBuy","productCategory","ProductName","productSKU",
					"prodid","shipping_cost","subtotal","unit_price","total_value","rentalDays"));

	private final static ArrayList<String> checkoutBillingPage = new ArrayList<>(
			Arrays.asList( "cartSize", "quantity", "couponCode" ,"damage_waiver_cost","isBuy","productCategory","ProductName","productSKU",
					"prodid","shipping_cost","subtotal","unit_price","total_value","rentalDays"));

	private final static ArrayList<String> allPageFields = new ArrayList<>(
			Arrays.asList("pagetype", "AccountID", "global_url", "navigation_type", "page_name"
			));

	private final static ArrayList<String> arrayValues = new ArrayList<>(Arrays.asList(
			"product_price", "product_quantity", "page_category_name", "product_brand"));

	private TealiumContext context;




	@Override
	public Map<String, HybrisCustomPageTypeCustomData> getHybrisCustomPageTypes()
	{
		return null;
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
		customPagesMap.put("account", udo -> {
			try
			{
				udo.setValue("customer_name", context.value("customer_name"));
				udo.setValue("customer_email", context.value("customer_email"));
			}
			catch (UDOUpdateException e)
			{
				e.printStackTrace();
			}
			return udo;
		});

		customPagesMap.put("custom_two", udo -> {
			try
			{
				udo.setValue("custom_page2_key", "custom value");
			}
			catch (UDOUpdateException e)

			{
				e.printStackTrace();
			}
			return udo;
		});
	}

	private UDO defaultDataSources(UDO udo)
	{
		TealiumContext context = (TealiumContext) JaloSession.getCurrentSession().getAttribute("tealiumContext");
		this.context = context;
		fillUdo(udo, context, allPageFields);
		return udo;
	}

	private synchronized void fillUdo(UDO udo, TealiumContext context, List<String> requiredFields)
	{
		requiredFields.forEach((key) -> {
			try
			{
				if (arrayValues.contains(key))
				{
					if (context.arrayValue(key) != null)
					{
						String[] arr = context.arrayValue(key);
						for (int i = 0; i < arr.length; i++)
						{
							if (arr[i] == null || arr[i].trim() == "")
							{
								arr[i] = "null";
							}
						}

						udo.addArrayValues(key, context.arrayValue(key));
					}
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
				System.out.println("tealium cannot add value");
			}
		});
	}


}
