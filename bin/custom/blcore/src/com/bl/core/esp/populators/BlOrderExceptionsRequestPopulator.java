package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderexceptions.OrderExceptionEventRequest;
import com.bl.esp.dto.orderexceptions.data.OrderExceptionsData;
import com.bl.esp.dto.orderexceptions.data.OrderExceptionsExtraData;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Objects;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This populator created for order Exception ESP Event
 * @author Manikandan
 */
public class BlOrderExceptionsRequestPopulator  extends ESPEventCommonPopulator<OrderModel, OrderExceptionEventRequest> {

  private static final org.apache.log4j.Logger LOG = Logger.getLogger(BlOrderExceptionsRequestPopulator.class);
  private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";


  private ProductService productService;


  /**
   * Populate the Order Exceptions Request instance with values from the OrderModel.
   *
   * @param order the source object
   * @param orderExceptionEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel order, final OrderExceptionEventRequest orderExceptionEventRequest)
      throws ConversionException {

    Assert.notNull(order, "Parameter order cannot be null.");
    Assert.notNull(orderExceptionEventRequest, "Parameter orderExceptionEventRequest cannot be null.");

    final UserModel userModel = order.getUser();
    if(Objects.nonNull(userModel)) {
      orderExceptionEventRequest.setContactKey(userModel.getUid());
    }
    orderExceptionEventRequest.setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
        getString(BlCoreConstants.ORDER_EXCEPTION_EVENT_DEFINITION_KEY)));

    populateExceptionOrderData(order , orderExceptionEventRequest);
  }


  /**
   * This method populate order data from order model
   * @param orderModel orderodel
   * @param orderExceptionEventRequest request to be get updated
   */

  private void populateExceptionOrderData(final OrderModel orderModel , final OrderExceptionEventRequest orderExceptionEventRequest) {
    final OrderExceptionsData orderExceptionsData = new OrderExceptionsData();
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    populateCommonData(orderModel , orderExceptionsData);
    orderExceptionsData.setOldorderid(StringUtils.EMPTY);
    orderExceptionsData.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_EXCEPTION_EVENT_TEMPLATE)));
    orderExceptionsData.setDateplaced(formatter.format(orderModel.getDate()));
    orderExceptionsData.setStatus(getRequestValue(Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode() : StringUtils.EMPTY));
    orderExceptionsData.setSubstatus(getSubStatusFromOrder(orderModel));
    if(BooleanUtils.isTrue(orderModel.getIsRentalCart()) && BooleanUtils.isFalse(
        orderModel.isGiftCardOrder())) {
      orderExceptionsData
          .setActualreturndate(formatter.format(orderModel.getActualRentalEndDate()));
    }
    populateOrderItemsInXML(orderExceptionsData,orderExceptionEventRequest);
    orderExceptionEventRequest.setData(orderExceptionsData);
  }



  /**
   * This method populate items info details
   * @param orderExceptionsData data to be set
   */

  private void populateOrderItemsInXML(final OrderExceptionsData orderExceptionsData,
      final OrderExceptionEventRequest orderExceptionEventRequest) {
    try {
      final Document orderItemsInXMLDocument = createNewXMLDocument();
      final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument,
          BlCoreConstants.ITEMS_ROOT_ELEMENT);
      final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
          rootOrderItems, BlCoreConstants.ITEM_ROOT_ELEMENT);
      final OrderExceptionsExtraData orderExceptionsExtraData = orderExceptionEventRequest
          .getExtraData();
      if (Objects.nonNull(orderExceptionsExtraData)) {
        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
            BlCoreConstants.ORDER_ITEM_PRODUCT_CODE,
            getRequestValue(orderExceptionsExtraData.getSerialCode()));
        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
            BlCoreConstants.ORDER_ITEM_PRODUCT_TITLE,
            getRequestValue(getProductTitle(orderExceptionsExtraData.getSerialCode())));
        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
            BlCoreConstants.ITEM_PRODUCT_URL,
            getRequestValue(getSerialProductUrl(orderExceptionsExtraData.getSerialCode())));
        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
            BlCoreConstants.ITEM_AMOUNT_DUE_ROOT_ELEMENT,
            getRequestValue(String.valueOf(orderExceptionsExtraData.getTotalChargedAmount())));
        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
            BlCoreConstants.ITEM_NOTES_ROOT_ELEMENT,
            getRequestValue(orderExceptionsExtraData.getAllUnPaidBillNotes()));
        orderExceptionEventRequest.setExtraData(null);
      }

      final Transformer transformer = getTransformerFactoryObject();
      final StringWriter writer = new StringWriter();

      //transform document to string
      transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
      orderExceptionsData.setItemsinfo(writer.getBuffer().toString());

    } catch (final Exception exception) {
      BlLogger.logMessage(LOG, Level.ERROR, POPULATOR_ERROR, exception);
      throw new BlESPIntegrationException(exception.getMessage(),
          LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode(), exception);
    }
  }

  /**
   * This method created to get consignment status from order
   * @param abstractOrderModel order model
   * @return String
   */
  private String getSubStatusFromOrder(final AbstractOrderModel abstractOrderModel){
    final StringBuilder stringBuilder = new StringBuilder(StringUtils.EMPTY);
    if(CollectionUtils.isNotEmpty(abstractOrderModel.getConsignments())) {
      abstractOrderModel.getConsignments().forEach(consignmentModel -> {
        if(Objects.nonNull(consignmentModel.getStatus())){
          stringBuilder.append(consignmentModel.getStatus().getCode()).append(BlCoreConstants.SHARE_A_SALE_COMMA);
        }
      });
    }
    return StringUtils.chop(stringBuilder.toString());
  }

  public ProductService getProductService() {
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }


}
