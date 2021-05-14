package com.bl.core.attributes;

import de.hybris.platform.servicelayer.model.attribute.AbstractDynamicAttributeHandler;

import java.util.List;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import org.springframework.util.CollectionUtils;


/**
 * @author Admin
 *
 */
public class GiftCardBalanceHandler extends AbstractDynamicAttributeHandler<Double, GiftCardModel>
{

    @Override
    public Double get(final GiftCardModel model)
    {
        double balance = 0;

        try
        {
            final List<GiftCardMovementModel> movements = model.getMovements();
            if (!CollectionUtils.isEmpty(movements))
            {
                for (final GiftCardMovementModel giftCardMovementModel : movements) {
                    balance += giftCardMovementModel.getAmount();
                }
            }

        }
        catch (final Exception e)
        {
            e.printStackTrace();
        }
        return balance;
    }
}
