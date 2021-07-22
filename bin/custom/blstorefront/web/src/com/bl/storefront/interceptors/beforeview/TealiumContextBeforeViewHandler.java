package com.bl.storefront.interceptors.beforeview;

//import com.pragiti.search.easyask.core.data.EasyAskNavigateResults;
import com.bl.core.jalo.BlProduct;
import com.bl.core.model.BlProductModel;
import com.tealium.context.TealiumContext;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.Breadcrumb;
import de.hybris.platform.acceleratorstorefrontcommons.interceptors.BeforeViewHandler;
//import de.hybris.platform.b2b.model.B2BCustomerModel;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.cms2.model.pages.ProductPageModel;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.product.data.BaseOptionData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.product.data.VariantOptionData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.commerceservices.enums.CustomerType;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.order.CartService;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.user.UserService;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.ModelAndView;


public class TealiumContextBeforeViewHandler implements BeforeViewHandler
{

	private static final Logger LOG = LoggerFactory.getLogger(TealiumContextBeforeViewHandler.class);
	private CartService cartService;
	private ProductService productService;
	private UserService userService;
	private Converter<CartModel, CartData> cartConverter;


	private final String FLAG_VALUE = "true";

	@Override
	public void beforeView(
      HttpServletRequest request, HttpServletResponse response, ModelAndView modelAndView) throws Exception
	{
		TealiumContext prevContext = (TealiumContext) JaloSession.getCurrentSession().getAttribute("tealiumContext");
		TealiumContext context;
		Collection<Breadcrumb> breadcrumbArrayList = null;
		if (modelAndView.getModel().get("breadcrumbs") != null)
		{
			breadcrumbArrayList = (Collection<Breadcrumb>) modelAndView.getModel().get("breadcrumbs");
		}

		context = prevContext == null ? new TealiumContext() : prevContext;
		try
		{
			boolean cartAddition = false;

			if (context.getNewUser())
			{
				context.set("new_user", FLAG_VALUE);
			}

			/*//page_type for category // this is no needed as we did not showing breadcrum on plp or slp.
			if ("CATEGORY".equals(modelAndView.getModel().get("pageType")))
			{
				if (breadcrumbArrayList != null)
				{
					context.setArrayValue("page_category_name",
							breadcrumbArrayList.stream().map(Breadcrumb::getName).toArray(String[]::new));

						context.set("page_type", "category");

				}

			}*/

			CartData lastSnapshot = context.getProductData();
			//context.set("page_category_name", modelAndView.getModel().get("categoryName"));
			context.set("page_name", modelAndView.getModel().get("pageTitle"));

			/*//search
			EasyAskNavigateResults eaNR = (EasyAskNavigateResults) modelAndView.getModel().get("eaSearchResults");
			if (eaNR != null)
			{
				context.set("navigation_type", "search");
				context.set("search_results", eaNR.getResultCount());
				if (eaNR.getResultCount() == 0)
				{
					context.set("no_search_result_keyword", FLAG_VALUE);
					context.set("keyword_no_search_result_page", eaNR.getSearchTerm());
				}

				if (eaNR.getSearchTerm() != null || eaNR.getSearchTerm().trim() != "")
				{
					context.set("search_keyword", eaNR.getSearchTerm());
				}
				else
				{
					context.set("search_keyword", "no_search_text");
				}
			}
			else
			{
				context.set("search_keyword", "no_search_text");
				context.set("search_results", "no_search");
			}*/


		/*	if (request.getParameter("searchText") != null)
			{
				context.set("navigation_type", "searched");
			}
			*//*else if (prevContext == null)
			{
				context.set("navigation_type", "entry page");
				if (eaNR != null)
				{
					context.set("search_keyword", eaNR.getSearchTerm() + " - as entry page");
				}
			}*//*
			else
			{
				context.set("navigation_type", "navigated");
			}*/



			//cart page
			CartData cartData = ((CartData) modelAndView.getModel().get("cartData"));
			if (cartData == null)
			{
				cartData = cartConverter.convert(cartService.getSessionCart());
			}
			if (cartData != null)
			{
				context.setProductData(cartData);//set new cart data

				if (lastSnapshot != null && cartData.getTotalUnitCount() < lastSnapshot.getTotalUnitCount())
				{
					context.set("cart_removed", FLAG_VALUE);
				}
				else if (lastSnapshot == null && cartData.getTotalUnitCount() > 0
						|| (lastSnapshot != null && cartData.getTotalUnitCount() > lastSnapshot.getTotalUnitCount()))
				{
					context.set("cart_addition", FLAG_VALUE);
					if (!context.getCartOpen())
					{
						if (lastSnapshot == null || lastSnapshot.getTotalUnitCount() == 0)
						{
							context.set("cart_open", FLAG_VALUE);
						}
						context.setCartOpen(true);
					}
					cartAddition = true;
					for (OrderEntryData entry : cartData.getEntries())
					{
						if (entry.getProduct().getCode().equals(context.getViewCartProductCode()))
						{
							context.set("cart_addition_quick_view", FLAG_VALUE);
							context.setViewCartProductCode("no_product");
						}
						else if (entry.getProduct().getCode().equals(context.getProductPageProductCode()))
						{
							context.set("cart_addition_pdp", FLAG_VALUE);
							context.setProductPageProductCode("no_product");
						}
					}
				}
			}

			//order
			OrderData orderData = ((OrderData) modelAndView.getModel().get("orderData"));



			if (orderData != null)
			{
        setOrderTags(context,orderData);


				/*if (orderData.getPaymentType() != null)
				{
					context.set("order_payment_type", orderData.getPaymentType().getCode());
				}*/
				if (orderData.getTotalDiscounts() != null)
				{
					context.set("order_discount", orderData.getTotalDiscounts().getValue());
				}
				if (orderData.getTotalTax() != null)
				{
					context.set("order_tax", orderData.getTotalTax().getValue());
				}
				if (orderData.getTotalPriceWithTax() != null)
				{
					context.set("order_currency", orderData.getTotalPriceWithTax().getCurrencyIso());
				}

			//	AddressData billingAddress = orderData.getBillingAddress();
				AddressData billingAddress = orderData.getDeliveryAddress();
				if (billingAddress != null)
				{
					if (billingAddress.getRegion() != null)
					{
						context.set("state", billingAddress.getRegion().getName());
					}
					context.set("postal_code", billingAddress.getPostalCode());

				}
				//CustomerData customer = orderData.getB2bCustomerData();
				/*if (customer != null)
				{
					B2BCustomerModel userModel = (B2BCustomerModel) userService.getUserForUID(customer.getUid());
					if (userModel != null)
					{
						if (!context.getNewUser() && userModel.getType().equals(CustomerType.GUEST))
						{
							context.set("guest_order", FLAG_VALUE);
						}
					}
					context.set("customer_email", customer.getEmail());
					context.set("hybris_customer_id", customer.getCustContactProfileId());
				}*/
			}
			else if (cartData != null)
			{
				AddressData billingAddress = cartData.getDeliveryAddress();
				if (billingAddress != null)
				{
					context.set("state", billingAddress.getRegion());
					context.set("postal_code", billingAddress.getPostalCode());
				}

				//CustomerData customer = cartData.getB2bCustomerData();
				CustomerModel customer =(CustomerModel) userService.getCurrentUser();
				//CustomerData user = (CustomerData) modelAndView.getModel().get("user");
				/*if (user != null)
				{
					context.set("customer_email", user.getEmail());
					context.set("hybris_customer_id", user.getCustContactProfileId());
				}
				else
				{
					if (customer != null)
					{
						context.set("customer_email", customer.getEmail());

						context.set("hybris_customer_id", customer.getCustContactProfileId());
					}
				}*/
			}

			CustomerData user = (CustomerData) modelAndView.getModel().get("user");
			if (breadcrumbArrayList != null)
			{
				breadcrumbArrayList.forEach((o) -> {
					if ("Checkout".equals(o.getName()))
					{
						//checkout
						setCheckoutTags(context, user);
					}
					if ("Quick Order".equals(o.getName()))
					{
						context.set("page_type", "quick_order");
					}
				});
				ArrayList<String> breadcrumbs = new ArrayList<>();
				breadcrumbArrayList.forEach((o) -> {
					breadcrumbs.add(o.getName());
				});
				context.setArrayValue("breadcrumb_value", breadcrumbs.toArray(new String[breadcrumbs.size()]));
			}
			else if ((String) modelAndView.getModel().get("previousStepUrl") != null
					&& ((String) modelAndView.getModel().get("previousStepUrl")).contains("delivery"))
			{
				setCheckoutTags(context, user);
			}
			context.set("global_url", request.getRequestURL().append('?').append(request.getQueryString()).toString());


			Object cmsPage = modelAndView.getModel().get("cmsPage");
			if (cmsPage != null && cmsPage instanceof ContentPageModel)
			{
				ContentPageModel page = (ContentPageModel) cmsPage;
				if ("orderConfirmationPage".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					if (context.getCheckoutType() != null)
					{
						context.set(context.getCheckoutType(), FLAG_VALUE);
						context.setCheckoutType(null);
					}
					context.set("page_type", "confirmation");
					List<String> listId = new ArrayList<>();
					List<String> brands = new ArrayList<>();
					if (orderData.getEntries() != null)
					{
						for (OrderEntryData entry : orderData.getEntries())
						{
							String id = productService.getProductForCode(entry.getProduct().getCode()).getPk().getLongValueAsString();
							String brand = productService.getProductForCode(entry.getProduct().getCode()).getManufacturerName();
							listId.add(id != null ? id : "null");
							brands.add(brand != null ? id : "null");
						}
						context.setArrayValue("product_id", listId.toArray(new String[listId.size()]));
						context.setArrayValue("product_brand", brands.toArray(new String[brands.size()]));
					}
				}
				if ("checkOrderPage".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.set("page_type", "order_status");
				}
				if ("freeActivitiesLessonPlanPage".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.set("page_type", "lesson_plans");
				}
				if ("aboutUsPage".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.set("page_type", "content");
				}
				if ("register".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.set("page_type", "registration");
				}
				if ("checkout-login".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.set("page_type", "checkout_login");
				}
				if ("account".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					//user settings
					if (user != null)
					{
						context.set("customer_name", user.getName());
						context.set("customer_email", user.getCustomerId());
					}
				}
				if ("notFound".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.set("page_type", "404_page");
				}
				if ("searchEmpty".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.set("page_type", "no_search");
				}
				if ("search".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.set("page_type", "search");
				}
				if ("register".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.setNewUser(true);
				}
				if ("homepage".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					context.set("page_type", "home");
				}
				if ("cartPage".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					//cartPage
					context.set("page_type", "cart");
					List<String> listId = new ArrayList<>();
					List<String> brands = new ArrayList<>();
					if (cartData.getEntries() != null)
					{
						for (OrderEntryData entry : cartData.getEntries())
						{
							String id = productService.getProductForCode(entry.getProduct().getCode()).getPk().getLongValueAsString();
							String brand = productService.getProductForCode(entry.getProduct().getCode()).getManufacturerName();
							listId.add(id != null ? id : "null");
							brands.add(brand != null ? brand : "null");
						}
						context.setArrayValue("product_id", listId.toArray(new String[listId.size()]));
						context.setArrayValue("product_brand", brands.toArray(new String[brands.size()]));
					}

					if (cartData.getTotalUnitCount() == 0)
					{
						context.set("empty_cart_view", FLAG_VALUE);
					}
					else
					{
						context.set("cart_view", FLAG_VALUE);
					}
				}
				if (cartAddition)
				{
					context.set("cart_addition_quick_add", FLAG_VALUE);
				}
				if ("multiStepCheckoutSummaryPage".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					//checkout steps
					context.set("page_type", "checkout");
					context.set("page_name", modelAndView.getModel().get("progressBarId"));
					context.set("checkout_step", modelAndView.getModel().get("progressBarId"));
				}
				if ("QuickViewPage".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					//quick view page
					ProductData product = (ProductData) modelAndView.getModel().get("product");
					if (product != null)
					{
						context.setViewCartProductCode(product.getCode());
					}
				}

			}
			if (cmsPage != null && cmsPage instanceof ProductPageModel)
			{
				ProductPageModel page = (ProductPageModel) modelAndView.getModel().get("cmsPage");
				if ("productDetails".equals(page.getItemModelContext().getOriginalValue("uid")))
				{
					//product page

					context.set("page_type", "product");
					ProductData product = (ProductData) modelAndView.getModel().get("product");
					if (product != null)
					{
						context.setProductPageProductCode(product.getCode());
						ProductModel productModel = productService.getProductForCode(product.getCode());
						if (productModel != null)
						{

							Set<String> skus = new LinkedHashSet<>();
							List<String> ids = new ArrayList<>();
							List<String> brands = new ArrayList<>();
							ids.add(productModel.getPk().getLongValueAsString());
							brands.add(productModel.getManufacturerName());
							context.setArrayValue("product_name", new String[]{productModel.getName()});
							if (productModel.getSupercategories() != null)
							{
								context.setArrayValue("product_subcategory", productModel.getSupercategories().stream().distinct()
										.map(x -> x.getName()).toArray(size -> new String[size]));
							}
							List<String> qty = new ArrayList<>();
							List<String> price = new ArrayList<>();
							for (PriceData priceData : product.getVolumePrices())
							{
								String qtyEnd = priceData.getMaxQuantity() == null ? "+" : "-" + priceData.getMaxQuantity();
								qty.add(priceData.getMinQuantity() + qtyEnd);
								price.add(priceData.getFormattedValue());
							}
							if (qty.size() > 0)
							{
								context.setArrayValue("product_quantity", qty.toArray(new String[qty.size()]));
								context.setArrayValue("product_price", price.toArray(new String[price.size()]));
							}
							List<BaseOptionData> baseOptions = product.getBaseOptions();

							if (baseOptions != null && baseOptions.size() > 0)
							{
								for (BaseOptionData bo : baseOptions)
								{
									List<VariantOptionData> variants = bo.getOptions();
									if (variants != null && variants.size() > 0)
									{
										for (VariantOptionData variant : variants)
										{
											skus.add(variant.getCode());
										}
									}
								}
							}
							if (skus.size() > 0)
							{
								context.setArrayValue("product_sku", skus.toArray(new String[skus.size()]));
							}
							if (ids.size() > 0)
							{
								context.setArrayValue("product_id", ids.toArray(new String[ids.size()]));
							}
							if (brands.size() > 0)
							{
								context.setArrayValue("product_brand", brands.toArray(new String[brands.size()]));
							}

						}
					}
				}
			}


			cartService.getSessionCart().getCode();
			String url = request.getRequestURL().append('?').append(request.getQueryString()).toString();
			context.set("global_url", url);

			if (url.contains("my-account"))
			{
				context.set("page_type", "account_manage");
			}

			context.correctPageType();
		}
		catch (Throwable e)
		{
			LOG.error("TealiumContextBeforeViewHandler failed with message:" + e.getMessage());
		}
		finally
		{
			JaloSession session = JaloSession.getCurrentSession();
			session.setAttribute("tealiumContext", context);
		}

	}

	private void setCheckoutTags(TealiumContext context, CustomerData user)
	{
		context.set("checkout", FLAG_VALUE);
		if (context.getCheckoutType() != null)
		{
			context.set(context.getCheckoutType(), FLAG_VALUE);
		}
		else if (user != null && user.getCustomerId() != null)
		{
			if (context.getNewUser())
			{
				context.set("new_user_checkout", FLAG_VALUE);
				context.setCheckoutType("new_user_checkout");
			}
			else
			{
				context.set("existing_user_checkout", FLAG_VALUE);
				context.setCheckoutType("existing_user_checkout");
			}

		}
		else
		{
			context.set("guest_checkout", FLAG_VALUE);
			context.setCheckoutType("guest_checkout");
		}
	}

	public void setOrderTags(TealiumContext context,OrderData orderData){
   // orderData.getor
		context.set("orderID",orderData.getCode());
		context.set("AccountID",orderData.getUser().getUid());
		context.set("damage waiver cost",orderData.getTotalDamageWaiverCost());
		context.set("isBuy",!orderData.getIsRentalCart());
		context.set("shipping cost",orderData.getDeliveryCost());
		context.set("subtotal",orderData.getSubTotal());
		context.set("rentalDays",orderData.getRentalDates().getNumberOfDays());
		context.set("pagetype","confirmation");
		context.set("totalvalue",orderData.getTotalPrice());

		AddressData addressData = orderData.getDeliveryAddress();
		context.set("userEmail",addressData.getEmail());
		context.set("userFirstName",addressData.getFirstName());
		context.set("userLastName",addressData.getLastName());

		List<OrderEntryData> entryDataList = orderData.getEntries();
		if(CollectionUtils.isNotEmpty(entryDataList)){
			entryDataList.forEach( entry ->{
				ProductData productData =entry.getProduct();
				context.set("productSKU",productData.getCode());
				context.set("ProductName",productData.getName());
				context.set("quantity",entry.getQuantity());
				context.set("unit_price",entry.getTotalPrice());
			});
			context.set("cartSize",entryDataList.size());
		}

		context.set("couponCode", (CollectionUtils.isNotEmpty(orderData.getAppliedVouchers()) ?orderData.getAppliedVouchers().get(0) :"no coupan applied"));
		context.set("productList",orderData.getCode());
		context.set("searchTerm",orderData.getCode());
		context.set("isVideo",orderData.getCode());
		context.set("marketingOptin",orderData.getCode());
  }

	public void setCartConverter(Converter<CartModel, CartData> cartConverter)
	{
		this.cartConverter = cartConverter;
	}

	public UserService getUserService()
	{
		return userService;
	}

	public void setUserService(UserService userService)
	{
		this.userService = userService;
	}

	public CartService getCartService()
	{
		return cartService;
	}

	public void setCartService(CartService cartService)
	{
		this.cartService = cartService;
	}

	public ProductService getProductService()
	{
		return productService;
	}

	public void setProductService(ProductService productService)
	{
		this.productService = productService;
	}
}
