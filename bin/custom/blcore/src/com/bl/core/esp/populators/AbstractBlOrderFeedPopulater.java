package com.bl.core.esp.populators;

import com.bl.core.enums.GearGaurdEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.esp.constants.BlespintegrationConstants;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.product.ProductService;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This populator is created to provide the common API to convert data to XML.
 * @author Vijay Vishwakarma
 */
public class AbstractBlOrderFeedPopulater {

  private ProductService productService;
  private CatalogVersionService catalogVersionService;

  protected Element createRootElementForRootElement(final Document document, final Element rootElement, final String rootElementName) {
    final Element childElement = document.createElement(rootElementName);
    rootElement.appendChild(childElement);
    return childElement;
  }

  protected Element createElementForRootElement(final Document document, final Element rootElement, final String element, final String value) {
    final Element childElement = document.createElement(element);
    childElement.appendChild(document.createTextNode(value));
    rootElement.appendChild(childElement);
    return childElement;
  }
  /**
   * To get the request value based
   * @param value value get from order
   * @return value to set on request
   */
  protected String getRequestValue(final String value){
    return StringUtils.isBlank(value) ? StringUtils.EMPTY :value;
  }

  /**
   * This method created to get the order type
   * @param orderModel ordermodel
   * @return string
   */
  protected String getOrderType(final AbstractOrderModel orderModel){
    final AtomicReference<String> orderType = new AtomicReference<>(StringUtils.EMPTY);
    if(BooleanUtils.isTrue(orderModel.isGiftCardOrder())) {
      orderType.set(BlespintegrationConstants.GIFT_CARD_ORDER);
    }
    else if(BooleanUtils.isTrue(orderModel.getIsNewGearOrder())){
      orderType.set(BlespintegrationConstants.NEW_GEAR_ORDER);
    }
    else if(BooleanUtils.isTrue(orderModel.getIsRentalCart())){
      orderType.set(BlespintegrationConstants.RENTAL);
    }
    else if(BooleanUtils.isFalse(orderModel.getIsRentalCart())){
      orderType.set(BlespintegrationConstants.USED_GEAR);
    }
    return orderType.get();
  }

  /**
   * This method created to get order status from order model
   * @param orderModel orderModel
   * @return String
   */
  protected String getOrderStatus(final AbstractOrderModel orderModel) {
    return Objects.isNull(orderModel.getStatus()) ? StringUtils.EMPTY : orderModel.getStatus().getCode();
  }

  /* This method is to get the damage waiver text  from order  entry model
   * @param abstractOrderEntryModel AbstractOrderEntryModel
   * @return values to set on request
   */
  protected String getDamageWaiverName(final AbstractOrderEntryModel abstractOrderEntryModel) {
    final AtomicReference<String> damageWaiverText = new AtomicReference<>(StringUtils.EMPTY);
    if(BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardWaiverSelected())) {
      damageWaiverText.set(GearGaurdEnum.GEAR_GAURD.getCode());
    }
    else if(BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardProFullWaiverSelected())){
      damageWaiverText.set(GearGaurdEnum.GEAR_GAURD_PRO.getCode());
    }
    else if(BooleanUtils.isTrue(abstractOrderEntryModel.getNoDamageWaiverSelected())){
      damageWaiverText.set(GearGaurdEnum.NONE.getCode());
    }
    return damageWaiverText.get();
  }
  /**
   * This method created to get the product title
   * @param serialProductCode serial code
   * @return string
   */
  protected String getProductTitle(final String serialProductCode) {
    final AtomicReference<String> productTitle = new AtomicReference<>(StringUtils.EMPTY);
    final CatalogVersionModel catalogVersion = getCatalogVersionService().getCatalogVersion(BlespintegrationConstants.CATALOG_VALUE,BlespintegrationConstants.ONLINE);
    final BlSerialProductModel blSerialProduct = (BlSerialProductModel) getProductService().getProductForCode(catalogVersion, serialProductCode);
    if(Objects.nonNull(blSerialProduct)) {
      final BlProductModel blProductModel = blSerialProduct.getBlProduct();
      if(Objects.nonNull(blProductModel)){
        productTitle.set(blProductModel.getName());
      }
    }
    return productTitle.get();
  }

  public ProductService getProductService() {
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }

  public CatalogVersionService getCatalogVersionService() {
    return catalogVersionService;
  }

  public void setCatalogVersionService(
      CatalogVersionService catalogVersionService) {
    this.catalogVersionService = catalogVersionService;
  }
}
