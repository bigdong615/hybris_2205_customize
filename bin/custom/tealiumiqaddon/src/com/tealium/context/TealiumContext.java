package com.tealium.context;

import de.hybris.platform.commercefacades.order.data.CartData;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.lang.StringUtils;


public class TealiumContext
{
	private Map<String, String> attributes = new ConcurrentHashMap<>();
	private Map<String, String[]> arrayAttributes = new ConcurrentHashMap<>();
	private CartData productData;
	private String checkoutType;
	private String viewCartProductCode = "no_product";
	private String productPageProductCode = "no_product";
	private Boolean cartOpen = false;

	private List<String> pageTypeValues = Arrays.asList("home", "search", "category",  "product", "cart",
			  "product_grid", "order_status", "order_detail", "order_confirmation",  "checkout",
			"my_list", "no_search", "404_page", "error_page", "product_compare", "confirmation");


	public void set(String key, String value)
	{
		if (StringUtils.isNotEmpty(value))
		 {
			attributes.put(key, value);
		 }
	}

	public void set(String key, Object value)
	{
		if (value != null){
			if (value instanceof String)
			{
				attributes.put(key, (String) value);
			}
			else if (value instanceof Number)
			{
				attributes.put(key, String.valueOf(value));
			}
		}
	}

	public void setArrayValue(String key, String[] value)
	{
		if (value != null && value instanceof String[]){
				arrayAttributes.put(key, value);
		 }
	}

	public void clean()
	{
		arrayAttributes = new HashMap<>();
	}

	public void correctPageType()
	{
		String pageType = attributes.get("pagetype");
		if (!pageTypeValues.contains(pageType) && pageType != null)
		{
			attributes.put("pagetype", "content");
		}
	}

	public void remove(String key)
	{
		attributes.remove(key);
	}


	public String getProductPageProductCode()
	{
		return productPageProductCode;
	}

	public void setProductPageProductCode(String productPageProductCode)
	{
		this.productPageProductCode = productPageProductCode;
	}

	public String[] arrayValue(String key)
	{
		return arrayAttributes.get(key);
	}

	public String value(String key)
	{
		return attributes.get(key);
	}

	public Map<String, String> getAttributes()
	{
		return attributes;
	}

	public void setAttributes(HashMap<String, String> attributes)
	{
		this.attributes = attributes;
	}

	public CartData getProductData()
	{
		return productData;
	}

	public void setProductData(CartData productData)
	{
		this.productData = productData;
	}

	public String getViewCartProductCode()
	{
		return viewCartProductCode;
	}

	public String getCheckoutType()
	{
		return checkoutType;
	}

	public void setCheckoutType(String checkoutType)
	{
		this.checkoutType = checkoutType;
	}

	public void setViewCartProductCode(String viewCartProductCode)
	{
		this.viewCartProductCode = viewCartProductCode;
	}

	public Boolean getCartOpen()
	{
		return cartOpen;
	}

	public void setCartOpen(Boolean cartOpen)
	{
		this.cartOpen = cartOpen;
	}
}
