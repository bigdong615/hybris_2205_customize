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

	private final static ArrayList<String> searchPageFields = new ArrayList<>(Arrays.asList("search_results", "search_keyword"));
	private final static ArrayList<String> categoryPageFields = new ArrayList<>(Arrays.asList("page_category_name"));
	private final static ArrayList<String> productPageFields = new ArrayList<>(
			Arrays.asList("search_results", "search_keyword", "product_id", "product_sku", "product_name", "product_brand",
					"product_category", "product_subcategory", "product_unit_price", "breadcrumb_value", "cart_addition_pdp",
					"product_subcategory", "product_price", "product_quantity"));

	private final static ArrayList<String> cartPageFields = new ArrayList<>(
			Arrays.asList("product_id", "product_sku", "product_name", "product_brand", "product_category", "product_subcategory",
					"product_unit_price", "product_quantity", "continue_to_checkout"));

	private final static ArrayList<String> orderConfirmationPageFields = new ArrayList<>(
			Arrays.asList("order_id  ", "order_subtotal", "order_payment_type", "order_total", "order_discount", "order_shipping",
					"order_tax", "order_currency", "order_coupon_code", "order_type", "product_id", "product_sku", "product_name",
					"product_brand", "product_category", "product_subcategory", "product_unit_price", "product_quantity",
					"customer_email", "state", "postal_code", "guest_order", "customer_account_number"));

	private final static ArrayList<String> customerDetailPageFields = new ArrayList<>(
			Arrays.asList("customer_name", "customer_email"));

	private final static ArrayList<String> allPageFields = new ArrayList<>(
			Arrays.asList("page_type", "global_url", "navigation_type", "page_name", "cart_open", "cart_addition",
					"cart_addition_pdp", "cart_addition_quick_view", "cart_addition_quick_add", "cart_addition_cart_quick_add",
					"cart_addition_order_history", "cart_removed", "checkout", "cart_view", "empty_cart_view", "searched_keyword",
					"type_ahead", "no_search_result_keyword", "keyword_no_search_result_page", "guest_checkout", "existing_user",
					"new_user", "guest_checkout_click_value", "existing_user_checkout", "new_user_checkout", "checkout_step"));

	private final static ArrayList<String> arrayValues = new ArrayList<>(Arrays.asList("breadcrumb_value", "product_subcategory",
			"product_price", "product_quantity", "page_category_name", "product_brand", "product_id", "product_sku",
			"product_name"));

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
	//	context.clean();
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
		defaultDataSources(udo);
		fillUdo(udo, context, searchPageFields);
		context.clean();
		return udo;
	}

	@Override
	public UDO categoryPage(UDO udo)
	{
		defaultDataSources(udo);
		fillUdo(udo, context, categoryPageFields);
		context.clean();
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
		defaultDataSources(udo);
		fillUdo(udo, context, customerDetailPageFields);
		context.clean();
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
					/*if (context.value(key) != null)
					{
						udo.setValue(key, context.value(key));
						context.remove(key);
					}*/
				}
			}
			catch (UDOUpdateException e)
			{
				System.out.println("tealium cannot add value");
			}
		});
	}


}
