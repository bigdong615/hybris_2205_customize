package com.bl.core.services.cart.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.services.cart.BlCartService;
import com.bl.logging.BlLogger;

import de.hybris.platform.commerceservices.order.CommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.impl.DefaultCartService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Default implementation of the {@link BlCartService}.
 *
 * @author Neeraj Singh
 */
public class DefaultBlCartService extends DefaultCartService implements BlCartService {

  private static final Logger LOGGER = Logger.getLogger(DefaultBlCartService.class);

  private CommerceCartService commerceCartService;
  
  private CommerceCartCalculationStrategy commerceCartCalculationStrategy;

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearCartEntries() {

    final CartModel cartModel = getSessionCart();

    if (CollectionUtils.isNotEmpty(cartModel.getEntries())) {

      final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
      commerceCartParameter.setEnableHooks(true);
      commerceCartParameter.setCart(cartModel);
      getCommerceCartService().removeAllEntries(commerceCartParameter);

      BlLogger.logFormattedMessage(LOGGER, Level.DEBUG, BlCoreConstants.EMPTY_STRING,
          "All entries removed from cart with code : {}", cartModel.getCode());
    }
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
	public void resetCartCalculationFlag()
	{
		final CartModel cartModel = getSessionCart();
		if (CollectionUtils.isNotEmpty(cartModel.getEntries()))
		{
			cartModel.getEntries().forEach(entry -> {
				entry.setCalculated(Boolean.FALSE);
				getModelService().save(entry);
			});
		}
		cartModel.setCalculated(Boolean.FALSE);
		getModelService().save(cartModel);
		getModelService().refresh(cartModel);
	}

  /**
   * {@inheritDoc}
   */
	@Override
	public void recalculateCartIfRequired()
	{
		final CartModel cartModel = getSessionCart();
		if (BooleanUtils.isFalse(cartModel.getCalculated()))
		{
			final CommerceCartParameter parameter = getCommerceCartParameter(cartModel);
			getCommerceCartCalculationStrategy().recalculateCart(parameter);
		}
	}

	 /**
	   * {@inheritDoc}
	   */
	@Override
	public void updateCartEntryDamageWavier(final long entryNumber, final String damageWavierType)
	{
		final CartModel cartModel = getSessionCart();
		final Integer cartEntryNumber = Integer.valueOf((int) entryNumber);
		if (CollectionUtils.isNotEmpty(cartModel.getEntries()))
		{
			final AbstractOrderEntryModel cartEntryModel = cartModel.getEntries().stream()
					.filter(cartEntry -> cartEntryNumber.equals(cartEntry.getEntryNumber())).findFirst().orElse(null);
			checkAndSetFlagForSelectedDamageWavier(cartEntryModel, damageWavierType);
			cartModel.setCalculated(Boolean.FALSE);
			getModelService().save(cartEntryModel);
			getModelService().save(cartModel);
			final CommerceCartParameter parameter = getCommerceCartParameter(cartModel);
			getCommerceCartCalculationStrategy().recalculateCart(parameter);
		}
	}

	/**
	 * Gets the commerce cart parameter.
	 *
	 * @param cartModel the cart model
	 * @return the commerce cart parameter
	 */
	private CommerceCartParameter getCommerceCartParameter(final CartModel cartModel)
	{
		final CommerceCartParameter parameter = new CommerceCartParameter();
		parameter.setCart(cartModel);
		parameter.setEnableHooks(Boolean.TRUE);
		return parameter;
	}

	/**
	 * Check and set flag for selected damage wavier.
	 *
	 * @param cartEntryModel the cart entry model
	 * @param damageWavierType the damage wavier type
	 */
	private void checkAndSetFlagForSelectedDamageWavier(final AbstractOrderEntryModel cartEntryModel,
			final String damageWavierType)
	{
		switch (damageWavierType)
		{
			case BlCoreConstants.GEAR_GAURD_PRO_FULL:
				setFlags(cartEntryModel, Boolean.TRUE, Boolean.FALSE, Boolean.FALSE);
				break;
			case BlCoreConstants.GEAR_GAURD:
				setFlags(cartEntryModel, Boolean.FALSE, Boolean.TRUE, Boolean.FALSE);
				break;
			case BlCoreConstants.NO_GEAR_GAURD:
				setFlags(cartEntryModel, Boolean.FALSE, Boolean.FALSE, Boolean.TRUE);
				break;
			default:
				break;
		}
	}

	/**
	 * Sets the flags for Damage Wavier.
	 *
	 * @param cartEntryModel the cart entry model
	 * @param gearGaurdProFullWaiverSelected the gear gaurd pro full waiver selected
	 * @param gearGaurdWaiverSelected the gear gaurd waiver selected
	 * @param noGearGaurdWaiverSelected the no gear gaurd waiver selected
	 */
	private void setFlags(final AbstractOrderEntryModel cartEntryModel, final Boolean gearGaurdProFullWaiverSelected,
			final Boolean gearGaurdWaiverSelected, final Boolean noGearGaurdWaiverSelected)
	{
		cartEntryModel.setGearGaurdProFullWaiverSelected(gearGaurdProFullWaiverSelected);
		cartEntryModel.setGearGaurdWaiverSelected(gearGaurdWaiverSelected);
		cartEntryModel.setNoDamageWaiverSelected(noGearGaurdWaiverSelected);
		cartEntryModel.setCalculated(Boolean.FALSE);
	}

  public CommerceCartService getCommerceCartService() {
    return commerceCartService;
  }

  public void setCommerceCartService(CommerceCartService commerceCartService) {
    this.commerceCartService = commerceCartService;
  }
  
  /**
	 * @return the commerceCartCalculationStrategy
	 */
	public CommerceCartCalculationStrategy getCommerceCartCalculationStrategy()
	{
		return commerceCartCalculationStrategy;
	}

	/**
	 * @param commerceCartCalculationStrategy the commerceCartCalculationStrategy to set
	 */
	public void setCommerceCartCalculationStrategy(final CommerceCartCalculationStrategy commerceCartCalculationStrategy)
	{
		this.commerceCartCalculationStrategy = commerceCartCalculationStrategy;
	}
	
}
