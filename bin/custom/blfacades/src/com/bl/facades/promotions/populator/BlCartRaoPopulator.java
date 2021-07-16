package com.bl.facades.promotions.populator;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import java.math.BigDecimal;
import java.util.Date;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;


public class BlCartRaoPopulator implements Populator<CartModel, CartRAO> {

  private BlProductDao blProductDao;

  @Override
  public void populate(final CartModel source, final CartRAO target)
  {

    target.setRentalCart(source.getIsRentalCart());
    if(BlRentalDateUtils.getRentalsDuration() != null) {
      target.setRentalDurationDays(Integer.valueOf(BlRentalDateUtils.getRentalsDuration().getNumberOfDays()));
      final RentalDateDto rentalDatesFromSession = BlRentalDateUtils.getBlDatePickerService()
          .getRentalDatesFromSession();
      if(rentalDatesFromSession != null && (rentalDatesFromSession.getSelectedFromDate() != null || rentalDatesFromSession.getSelectedToDate() != null)) {
        target.setRentalArrivalDate(getFormattedDate(rentalDatesFromSession.getSelectedFromDate()));
        target.setRentalToDate(getFormattedDate(rentalDatesFromSession.getSelectedToDate()));
      }
    }
    target.setTotalIncludingDamageWaiver(
        BigDecimal.valueOf(source.getSubtotal() + source.getTotalDamageWaiverCost()));
    target.setUsedGearOnSale(true);

 }

  private boolean hasEligibleProductsForSale() {
    boolean hasOnSaleTrue = false;
    for (BlProductModel blProductModel : getBlProductDao().getAllActiveSkuProducts()) {
      if (blProductModel.getOnSale() != null && BooleanUtils.isTrue(blProductModel.getOnSale())
          && BooleanUtils.isTrue(blProductModel.getForSale()) && CollectionUtils
          .isNotEmpty(blProductModel.getSerialProducts())) {
        hasOnSaleTrue = blProductModel.getSerialProducts().stream().anyMatch(blSerialProductModel ->
            (blSerialProductModel.getSerialStatus() != null && blSerialProductModel
                .getSerialStatus().equals(SerialStatusEnum.ACTIVE)) &&
                (blSerialProductModel.getOnSale() != null && BooleanUtils
                    .isTrue(blSerialProductModel.getOnSale())) && BooleanUtils
                .isTrue(blSerialProductModel.getForSale()));
      }
    }
    return  hasOnSaleTrue;
  }

  /**
   * Get the formatted date
   * @param selectedDate
   * @return
   */
  private Date getFormattedDate(final String selectedDate) {
    return BlDateTimeUtils.getDate(selectedDate, BlCoreConstants.DATE_FORMAT);
  }


  public BlProductDao getBlProductDao() {
    return blProductDao;
  }

  public void setBlProductDao(BlProductDao blProductDao) {
    this.blProductDao = blProductDao;
  }
}
