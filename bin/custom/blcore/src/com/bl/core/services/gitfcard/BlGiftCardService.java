package com.bl.core.services.gitfcard;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.springframework.ui.Model;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.bl.core.model.GiftCardModel;
import com.bl.facades.giftcard.data.BLGiftCardData;

public interface BlGiftCardService {

    public boolean applyGiftCard(String giftCardCode);

    public double calculateGiftCardBalance(GiftCardModel giftCardModel);

    public void clearUncommitedMovements(GiftCardModel giftCardModel);

    /*public List<BLGiftCardData> popupGiftcard(String giftCardCode);*/

    public void removeGiftcard(String giftCardCode);

    void calculateGiftCard(AbstractOrderModel order, double totalplustax);

    boolean ValidateGiftplaceOrder(OrderModel order);

    /**
     * It will issue a new gift card in case of return
     *
     * @param item
     * @param amount
     */
    //public void giftCardRefund(ItemModel item, double amount);

}
