package com.bl.core.services.dao.impl;

import com.bl.core.services.dao.BLGiftCardDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;

import java.util.List;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import org.springframework.util.CollectionUtils;


/**
 * @author Admin
 *
 */
public class BlGiftCardDaoImpl implements BLGiftCardDao
{
    private FlexibleSearchService flexibleSearchService;


    private static final String GIFTCARD = "SELECT {mp:pk} FROM {GiftCard AS mp} WHERE {mp:code} = ?giftId ";
    private static final String GETCART = "SELECT {mp:pk} FROM {Cart AS mp} WHERE {mp:code} = ?cartId ";
    private static final String GETCARTMOVEMENT = "SELECT {b:pk} FROM { GiftCardMovement AS b JOIN GiftCard2Movement AS cpr ON {cpr.target} = {b:pk} JOIN GiftCard as C ON {C.pk} = {cpr.source} } where {C.code} = ?giftId";
    private static final String GETCARTMOVEMENT1 = "SELECT {b:pk} FROM { GiftCardMovement AS b JOIN GiftCard2Movement AS cpr ON {cpr.target} = {b:pk} JOIN GiftCard as C ON {C.pk} = {cpr.source} }";


    public GiftCardModel getGiftCard(final String giftId)
    {
        final StringBuilder builder = new StringBuilder();
        builder.append(GIFTCARD);
        final FlexibleSearchQuery query = new FlexibleSearchQuery(builder.toString());
        query.addQueryParameter("giftId", giftId);
        final List<GiftCardModel> result = flexibleSearchService.<GiftCardModel> search(query).getResult();

        return !CollectionUtils.isEmpty(result)  ? result.get(0) : null;
    }


    public CartModel getCart(final String cartId)
    {
        final StringBuilder builder = new StringBuilder();
        builder.append(GETCART);
        final FlexibleSearchQuery query = new FlexibleSearchQuery(builder.toString());
        query.addQueryParameter("cartId", cartId);
        final List<CartModel> result = flexibleSearchService.<CartModel> search(query).getResult();

        return !CollectionUtils.isEmpty(result) ? result.get(0) : null;
    }

    public List<GiftCardMovementModel> getCardMovement(final String giftId)
    {
        final StringBuilder builder = new StringBuilder();
        if (giftId != null)
        {
            builder.append(GETCARTMOVEMENT);
        }
        else
        {
            builder.append(GETCARTMOVEMENT1);
        }
        final FlexibleSearchQuery query = new FlexibleSearchQuery(builder.toString());

        if (giftId != null)
        {
            query.addQueryParameter("giftId", giftId);
        }

        final List<GiftCardMovementModel> result = flexibleSearchService.<GiftCardMovementModel> search(query).getResult();

        return !CollectionUtils.isEmpty(result) ? result : null;
    }


    public FlexibleSearchService getFlexibleSearchService()
    {
        return flexibleSearchService;
    }

    public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService)
    {
        this.flexibleSearchService = flexibleSearchService;
    }
}

