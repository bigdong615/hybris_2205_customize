package com.bl.facades.populators;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.facades.giftcard.data.BLGiftCardData;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.model.ModelService;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;

public class BlOrderGiftCardPopulator <SOURCE extends AbstractOrderModel, TARGET extends AbstractOrderData> implements
    Populator<AbstractOrderModel, AbstractOrderData> {

  private PriceDataFactory priceDataFactory;


  private ModelService modelService;

  @Override
  public void populate(final AbstractOrderModel source, final AbstractOrderData target) throws ConversionException {
    if (CollectionUtils.isNotEmpty(source.getGiftCard()))
    {
      final List<BLGiftCardData> blGiftCardDataList = new ArrayList<>();
      for (final GiftCardModel giftCardModel : source.getGiftCard())
      {
        final BLGiftCardData blGiftCardData = new BLGiftCardData();
        blGiftCardData.setCode(giftCardModel.getCode());
        if(!source.isGiftCardOrder()){
          final List<GiftCardMovementModel> giftCardMovementModelList = giftCardModel.getMovements();
          final GiftCardMovementModel giftCardMovementModel = giftCardMovementModelList.get(giftCardMovementModelList.size() - 1);
          blGiftCardData.setRedeemamount(getPriceDataFactory().create(PriceDataType.BUY , BigDecimal.valueOf(giftCardMovementModel.getAmount())
              , source.getCurrency()));
        /*  blGiftCardData.setBalanceamount(getPriceDataFactory().create(PriceDataType.BUY , BigDecimal.valueOf(giftCardMovementModel.getBalanceAmount())
              , source.getCurrency()));*/
          blGiftCardDataList.add(blGiftCardData);
        }

      }
      target.setGiftCardData(blGiftCardDataList);
    }
  }


  public PriceDataFactory getPriceDataFactory() {
    return priceDataFactory;
  }

  public void setPriceDataFactory(
      PriceDataFactory priceDataFactory) {
    this.priceDataFactory = priceDataFactory;
  }


  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

}
