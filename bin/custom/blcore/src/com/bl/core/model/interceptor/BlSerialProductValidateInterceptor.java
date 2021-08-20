package com.bl.core.model.interceptor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.stream.Collectors;


/**
 * The validator is used to validate blSerialProduct model before saving it.
 * @author Moumita
 */
public class BlSerialProductValidateInterceptor implements ValidateInterceptor<BlSerialProductModel> {

	private BaseStoreService baseStoreService;

	@Override
	public void onValidate(final BlSerialProductModel blSerialProduct,
			final InterceptorContext interceptorContext) throws InterceptorException {
		if (blSerialProduct.getIsBufferedInventory().booleanValue()) {
			final BaseStoreModel baseStore = getBaseStoreService().getBaseStoreForUid(
					BlCoreConstants.BASE_STORE_ID);
			if (null != baseStore && null != baseStore.getMinQtyForBufferInventory() &&
					baseStore.getMinQtyForBufferInventory() > 0) {
				final int minQtyForBufferInv = baseStore.getMinQtyForBufferInventory();
					final int totalSerialProducts = blSerialProduct.getBlProduct().getSerialProducts()
							.stream().filter(serialProduct -> null != serialProduct.getSerialStatus()
									&& serialProduct.getSerialStatus().equals(SerialStatusEnum.ACTIVE))
							.collect(Collectors.toList()).size();
					if (minQtyForBufferInv > totalSerialProducts) {
						throw new InterceptorException(
								"Can't mark this serial product as buffer inventory");
					}
			}
		}
	}

	public BaseStoreService getBaseStoreService() {
		return baseStoreService;
	}

	public void setBaseStoreService(BaseStoreService baseStoreService) {
		this.baseStoreService = baseStoreService;
	}
}
