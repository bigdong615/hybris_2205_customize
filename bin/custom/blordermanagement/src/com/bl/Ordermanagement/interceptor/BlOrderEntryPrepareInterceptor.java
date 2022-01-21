package com.bl.Ordermanagement.interceptor;


import com.bl.Ordermanagement.services.impl.DefaultBlOrderModificationService;
import com.bl.constants.BlloggingConstants;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.google.common.collect.Lists;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;


/**
 * This class is for modifying used gear orders
 *
 * @author Aditi
 */
public class BlOrderEntryPrepareInterceptor implements PrepareInterceptor<OrderEntryModel> {

	private static final Logger LOG = Logger.getLogger(BlOrderEntryPrepareInterceptor.class);

	private DefaultBlUserService defaultBlUserService;
	private DefaultBlOrderModificationService blOrderModificationService;

  @Override
  public void onPrepare(final OrderEntryModel orderEntryModel,
      final InterceptorContext interceptorContext) throws InterceptorException {

	  if (getDefaultBlUserService().isCsUser() && !interceptorContext.isNew(orderEntryModel) && BooleanUtils.isFalse(orderEntryModel.getOrder().getIsRentalCart() && !interceptorContext.isNew(OrderEntryModel.PRODUCT) && !interceptorContext.isModified(orderEntryModel, OrderEntryModel.ISMODIFIEDORDER)))
	  {
		 final SourcingResults resultsForUsedGearOrder = getBlOrderModificationService().getResultsForUsedGearOrder(orderEntryModel);
		 final  SourcingResult sourceResult = getBlOrderModificationService().createSourcingResultForUsedGear(orderEntryModel, resultsForUsedGearOrder);
		 final Optional<ConsignmentModel> consignmentModel = getBlOrderModificationService().checkifConsignmentIsPresent(orderEntryModel,sourceResult);
		 final BlSerialProductModel previousWarehouseCode = getProductPreviousValue(orderEntryModel);
		 final BlSerialProductModel updatedSerialProduct =  (BlSerialProductModel) orderEntryModel.getProduct();
		 if(consignmentModel.isPresent() && previousWarehouseCode !=null && previousWarehouseCode.getWarehouseLocation() !=null && previousWarehouseCode.getWarehouseLocation().getCode().equals(sourceResult.getWarehouse().getCode()))
		 {
			 final Map<String, ItemStatusEnum> itemsMap = new HashMap<>();
		 	orderEntryModel.getOrder().getConsignments().forEach(consignmentModel1 -> {
		 		consignmentModel1.getConsignmentEntries().forEach(consignmentEntryModel -> {
		 			if(consignmentEntryModel.getOrderEntry().getPk().equals(orderEntryModel.getPk()))
					{
		 				final Set<BlSerialProductModel> blSerialProducts = new HashSet<>();
						blSerialProducts.add(updatedSerialProduct);
						itemsMap.put(orderEntryModel.getProduct().getCode(), ItemStatusEnum.NOT_INCLUDED);
						consignmentEntryModel.setSerialProducts(Lists.newArrayList(blSerialProducts));
						consignmentEntryModel.setItems(itemsMap);
						interceptorContext.getModelService().save(consignmentEntryModel);
						interceptorContext.getModelService().refresh(consignmentEntryModel);
					}
				});
			});
		 }
		 else
		 {
			 final List<ConsignmentEntryModel> consignmentEntryToRemove = new ArrayList<>();
			 final List<ConsignmentModel> consignmentToRemove = new ArrayList<>();
			 orderEntryModel.getOrder().getConsignments().forEach(consignmentModel1 -> consignmentModel1.getConsignmentEntries().forEach(consignmentEntryModel -> {
				 if(consignmentEntryModel.getOrderEntry().getPk().equals(orderEntryModel.getPk()))
				 {
					 final String originalSerialPk = previousWarehouseCode.getPk().toString();
					 blOrderModificationService.getConsignmentToRemove(orderEntryModel.getOrder(),originalSerialPk, consignmentEntryToRemove, consignmentToRemove);
				 }
			 }));
			getBlOrderModificationService().createConsignmentForModifiedOrder(orderEntryModel,resultsForUsedGearOrder);
		 }
		 updatedSerialProduct.setSerialStatus(SerialStatusEnum.SOLD);
		 updatedSerialProduct.setHardAssigned(Boolean.TRUE);
		 interceptorContext.getModelService().save(updatedSerialProduct);
	  }
  }
  private BlSerialProductModel getProductPreviousValue(OrderEntryModel orderEntryModel)
  {
	  final Object previousProductValue = orderEntryModel.getItemModelContext().getOriginalValue(BlloggingConstants.PRODUCT_VALUE);
	  if(previousProductValue instanceof BlSerialProductModel)
	  {
		  return ((BlSerialProductModel) previousProductValue);
	  }	
	  return null;
  }

	public DefaultBlUserService getDefaultBlUserService() {
		return defaultBlUserService;
	}

	public void setDefaultBlUserService(DefaultBlUserService defaultBlUserService) {
		this.defaultBlUserService = defaultBlUserService;
	}


	public DefaultBlOrderModificationService getBlOrderModificationService() {
		return blOrderModificationService;
	}

	public void setBlOrderModificationService(DefaultBlOrderModificationService blOrderModificationService) {
		this.blOrderModificationService = blOrderModificationService;
	}
}
