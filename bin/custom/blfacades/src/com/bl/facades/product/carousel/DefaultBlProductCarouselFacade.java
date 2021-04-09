package com.bl.facades.product.carousel;

import de.hybris.platform.acceleratorfacades.productcarousel.impl.DefaultProductCarouselFacade;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.cms2lib.model.components.ProductCarouselComponentModel;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


/**
 * This class is used to get all the list of products data assigned in the component. Products such as Active,
 * Discontiuned and Upcoming products.
 *
 * @author Ravikumar
 *
 */
public class DefaultBlProductCarouselFacade extends DefaultProductCarouselFacade
{
	/**
	 * Fetches list of products for a given product carousel component when not in preview (i.e., no cmsTicketId in
	 * present in the session).
	 *
	 * @param component
	 *           The product carousel component model
	 * @return List<ProductData> list of available products
	 */
	@Override
	protected List<ProductData> fetchProductsForNonPreviewMode(final ProductCarouselComponentModel component)
	{
		final List<ProductData> products = new ArrayList<>();

		for (final ProductModel productModel : component.getProducts())
		{
			products.add(getProductFacade().getProductForCodeAndOptions(productModel.getCode(), getProductOptionsForCarousel()));
		}

		for (final CategoryModel categoryModel : component.getCategories())
		{
			for (final ProductModel productModel : categoryModel.getProducts())
			{
				products.add(getProductFacade().getProductForCodeAndOptions(productModel.getCode(), getProductOptionsForCarousel()));
			}
		}

		return products;
	}

	/**
	 * Fetches list of products for a given product carousel component when in preview (i.e., cmsTicketId in present in
	 * the session).
	 *
	 * @param component
	 *           The product carousel component model
	 * @return List<ProductData> list of available products
	 */
	@Override
	protected List<ProductData> fetchProductsForPreviewMode(final ProductCarouselComponentModel component)
	{
		return getSessionService().executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public Object execute()
			{
				try
				{
					getSearchRestrictionService().disableSearchRestrictions();

					final List<ProductData> products = new ArrayList<>();

					for (final ProductModel productModel : getDisplayableProductsForProductCarousel(component))
					{
						products.add(getProductForOptions(productModel, getProductOptionsForCarousel()));
					}

					for (final CategoryModel categoryModel : getListOfCategoriesForProductCarousel(component))
					{
						for (final ProductModel productModel : getDisplayableProductsForCategory(categoryModel))
						{
							products.add(getProductForOptions(productModel, getProductOptionsForCarousel()));
						}
					}
					return products;
				}
				finally
				{
					getSearchRestrictionService().enableSearchRestrictions();
				}
			}
		});
	}

	/**
	 * Gets the product options to populate products for carousel.
	 *
	 * @return the product options for carousel
	 */
	private List<ProductOption> getProductOptionsForCarousel()
	{
		return Arrays.asList(ProductOption.BASIC, ProductOption.PRICE, ProductOption.GALLERY, ProductOption.STOCK,
				ProductOption.REQUIRED_DATA);
	}

}
