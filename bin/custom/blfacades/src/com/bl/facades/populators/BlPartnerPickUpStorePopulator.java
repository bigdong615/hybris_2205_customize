package com.bl.facades.populators;

import com.bl.core.model.PartnerPickUpStoreModel;
import com.bl.facades.shipping.data.BlPartnerPickUpStoreData;
import de.hybris.platform.converters.Populator;

public class BlPartnerPickUpStorePopulator implements Populator<PartnerPickUpStoreModel, BlPartnerPickUpStoreData> {

    @Override
    public void populate(PartnerPickUpStoreModel partnerPickUpStore, BlPartnerPickUpStoreData blPartnerPickUpStoreData) {
        if(null != partnerPickUpStore && null != blPartnerPickUpStoreData) {
            blPartnerPickUpStoreData.setCode(partnerPickUpStore.getCode());
            blPartnerPickUpStoreData.setName(partnerPickUpStore.getName());
        }
    }
}
