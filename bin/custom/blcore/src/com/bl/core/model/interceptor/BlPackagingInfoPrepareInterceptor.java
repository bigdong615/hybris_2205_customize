package com.bl.core.model.interceptor;

import com.bl.core.enums.CarrierEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ItemModelContextImpl;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Objects;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

/**
 * Package Interceptor to perform custom logic on attribute before saving
 *
 * @author Ritika
 *
 */
public class BlPackagingInfoPrepareInterceptor implements PrepareInterceptor<PackagingInfoModel>
{
    private static final Logger LOG = Logger.getLogger(BlPackagingInfoPrepareInterceptor.class);

    @Value("${blintegration.ups.shipment.label.url}")
    private String upsShipmentURL;

    @Value("${blintegration.fedex.shipment.label.url}")
    private String fedExShipmentURL;

    @Override
    public void onPrepare(final PackagingInfoModel packagingInfoModel,final InterceptorContext ctx) throws InterceptorException {
        if(Objects.nonNull(packagingInfoModel.getInboundWarehouse()) && ctx.isModified(packagingInfoModel,PackagingInfoModel.INBOUNDWAREHOUSE) && !packagingInfoModel.getInboundWarehouse().equals(getInitialValue(packagingInfoModel,
                PackagingInfoModel.INBOUNDWAREHOUSE)))
        {
            packagingInfoModel.getSerialProducts().forEach(serialProduct ->{
                if(serialProduct instanceof BlSerialProductModel && Objects.nonNull(packagingInfoModel.getInboundWarehouse())){
                    final BlSerialProductModel serialProductModel = ((BlSerialProductModel) serialProduct);
                    if( Objects.nonNull(serialProductModel.getWarehouseLocation()) && !StringUtils.equalsIgnoreCase(serialProductModel.getWarehouseLocation().getCode(), packagingInfoModel.getInboundWarehouse().getCode())){
                        serialProductModel.setWarehouseLocation(packagingInfoModel.getInboundWarehouse());
                        ctx.getModelService().save(serialProductModel);
                        ctx.getModelService().refresh(serialProductModel);
                        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Warehouse Location updated to {} for serial {}",
                                serialProductModel.getWarehouseLocation().getCode(), serialProductModel.getCode());
                    }
                }
            });
        }

        if (Objects.nonNull(packagingInfoModel.getCarrier()) && StringUtils
            .isNotEmpty(packagingInfoModel.getOutBoundTrackingNumber()) &&
            (StringUtils.isEmpty(packagingInfoModel.getLabelURL()) || ctx.isModified(packagingInfoModel,PackagingInfoModel.CARRIER))) {
            String ocLocalURL = CarrierEnum.UPS.equals(packagingInfoModel.getCarrier()) ? upsShipmentURL : fedExShipmentURL;
            packagingInfoModel.setLabelURL(ocLocalURL + packagingInfoModel.getOutBoundTrackingNumber());
        }
    }

    /**
     * It gets the initial value of the attribute before update
     *
     * @param packagingInfoModel
     *           the packaging info model
     */
    private Object getInitialValue(final PackagingInfoModel packagingInfoModel, final String status) {
        final ItemModelContextImpl itemModelCtx = (ItemModelContextImpl) packagingInfoModel
                .getItemModelContext();
        return itemModelCtx.exists() ? itemModelCtx.getOriginalValue(status) : null;
    }

}
