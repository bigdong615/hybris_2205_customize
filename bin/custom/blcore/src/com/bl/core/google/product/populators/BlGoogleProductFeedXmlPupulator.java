package com.bl.core.google.product.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.model.ModelService;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.shipping.service.impl.DefaultBlDeliveryModeService;
import com.bl.integration.marketplace.jaxb.Channel;
import com.bl.integration.marketplace.jaxb.Item;
import com.bl.integration.marketplace.jaxb.Rss;
import com.bl.integration.marketplace.jaxb.Shipping;


public class BlGoogleProductFeedXmlPupulator implements Populator<List<BlProductModel>, Rss>
{
	private static final Logger LOG = Logger.getLogger(BlGoogleProductFeedXmlPupulator.class);
	private DefaultBlDeliveryModeService blDeliveryModeService;
	private ModelService modelService;

	@Override
	public void populate(final List<BlProductModel> source, final Rss target) throws ConversionException
	{
		target.setVersion("2.0");
		final Channel channel = new Channel();
		channel.setTitle("BorrowLenses.com");
		channel.setLink("https://www.borrowlenses.com");
		channel.setDescription("BorrowLenses.com");
		target.setChannel(channel);
		createItems(source, channel);
	}

	private void createItems(final List<BlProductModel> source, final Channel target)
	{
		final List<Item> items = new ArrayList<Item>();
		for (final BlProductModel product : source)
		{
			final Item item = new Item();
			final Shipping shipping = new Shipping();
			item.setId(product.getCode());
			item.setTitle(product.getName());
			item.setLink(target.getLink() + "/buy/product/" + product.getCode());
			item.setDescription(product.getDescription());
			item.setCondition("Used");
			item.setAvailability("in_stock");
			item.setBrand(product.getManufacturerName());
			if(product.getPicture()!=null) {
				item.setImage_Link(product.getPicture().getURL());
			}
			item.setModel_Number(product.getCode());
			if (!product.getMpn().isBlank() && !product.getMpn().contains("NA"))
			{
				item.setMpn(product.getMpn());
			}
			else if (!product.getUpc().isBlank() && !product.getUpc().contains("NA"))
			{
				item.setGtin(product.getUpc());
			}

			item.setPrice(getSerialPrice(product.getSerialProducts().iterator().next()));
			shipping.setCountry("US");
			final double price = getShippingPrice(product);
			shipping.setPrice(String.valueOf(price));
			item.setShipping(shipping);
			items.add(item);
		}
		target.setItems(items);
	}

	private double getShippingPrice(final BlProductModel product)
	{
		double price = 0.0;
		try {
   		final Collection<ZoneDeliveryModeModel> zoneDeliveryModeModelList = getBlDeliveryModeService()
   				.getShipToHomeDeliveryModesWithoutRentalDates("UPS", true);
   		if (!zoneDeliveryModeModelList.isEmpty())
   		{
   			price = zoneDeliveryModeModelList.stream().findFirst().get().getValues().stream().findFirst().get().getValue();
   		}
		}
		catch(final Exception e)
		{
			LOG.info("Shipping Price calculation for Google Product Feed went wrong "+ e.getMessage());
		}
		return price;
	}

	private String getSerialPrice(final BlSerialProductModel blSerialProduct)
	{
		String price = StringUtils.EMPTY;
		final BlProductModel skuProduct = blSerialProduct.getBlProduct();
		if (Objects.nonNull(blSerialProduct.getFinalSalePrice()) && Objects.nonNull(skuProduct)
				&& Objects.nonNull(skuProduct.getForSaleDiscount()))
		{
			final BigDecimal finalSalePrice = blSerialProduct.getFinalSalePrice().setScale(BlCoreConstants.DECIMAL_PRECISION,
					BlCoreConstants.ROUNDING_MODE);
			final Integer forSaleDiscount = skuProduct.getForSaleDiscount();
			final BigDecimal calculatedIncentivizedPrice = finalSalePrice.subtract(finalSalePrice
					.multiply(BigDecimal.valueOf(forSaleDiscount)).divide(BigDecimal.valueOf(BlCoreConstants.DIVIDE_BY_HUNDRED))
					.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
			price = calculatedIncentivizedPrice.toString();
		}
		return price;
	}

	public DefaultBlDeliveryModeService getBlDeliveryModeService()
	{
		return blDeliveryModeService;
	}

	public void setBlDeliveryModeService(final DefaultBlDeliveryModeService blDeliveryModeService)
	{
		this.blDeliveryModeService = blDeliveryModeService;
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

}
