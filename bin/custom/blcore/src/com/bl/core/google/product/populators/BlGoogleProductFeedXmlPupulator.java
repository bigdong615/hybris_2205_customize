package com.bl.core.google.product.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeValueModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.model.ModelService;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ShippingCostEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.shipping.service.impl.DefaultBlDeliveryModeService;
import com.bl.integration.marketplace.jaxb.Channel;
import com.bl.integration.marketplace.jaxb.Item;
import com.bl.integration.marketplace.jaxb.Shipping;
import com.microsoft.sqlserver.jdbc.StringUtils;


public class BlGoogleProductFeedXmlPupulator implements Populator<List<BlProductModel>, Channel>
{
	private static final Logger LOG = Logger.getLogger(BlGoogleProductFeedXmlPupulator.class);
	private DefaultBlDeliveryModeService blDeliveryModeService;
	private ModelService modelService;

	@Override
	public void populate(final List<BlProductModel> source, final Channel target) throws ConversionException
	{
		target.setTitle("BorrowLenses.com");
		target.setLink("https://www.borrowlenses.com");
		target.setDescription("BorrowLenses.com");
		createItems(source, target);
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
			if(product.getProductType()!=null) {
				item.setProduct_Type(product.getProductType().getCode());
				item.setGoogle_Product_Category(product.getProductType().getCode());
			}
			item.setBrand(product.getManufacturerName());
			if(product.getPicture()!=null) {
				item.setImage_Link(product.getPicture().getURL());
			}
			item.setModel_Number(product.getCode());
			item.setGtin(product.getUpc());
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
		final CartModel cart = getModelService().create(CartModel.class);
		final CartEntryModel entry = getModelService().create(CartEntryModel.class);
		entry.setProduct(product);
		entry.setQuantity(1l);
		final List<AbstractOrderEntryModel> cartEntries = new ArrayList<AbstractOrderEntryModel>();
		cartEntries.add(entry);
		cart.setEntries(cartEntries);
		final ZoneDeliveryModeModel deliveryMode = getModelService().create(ZoneDeliveryModeModel.class);
		deliveryMode.setCode("UPS_STANDARD_ROUND_TRIP");
		deliveryMode.setShippingCostCode(ShippingCostEnum.UPS_STANDARD_ROUND_TRIP);
		final ZoneDeliveryModeValueModel zoneDeliveryMode = getModelService().create(ZoneDeliveryModeValueModel.class);
		zoneDeliveryMode.setMinimum(0.0);
		zoneDeliveryMode.setValue(0.0);
		zoneDeliveryMode.setFixedAmount(false);
		final Set<ZoneDeliveryModeValueModel> zoneDeliveryModeValues = new HashSet<ZoneDeliveryModeValueModel>();
		zoneDeliveryModeValues.add(zoneDeliveryMode);
		deliveryMode.setValues(zoneDeliveryModeValues);
		final double price = getBlDeliveryModeService().getShippingCostAmount(cart, deliveryMode);
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
