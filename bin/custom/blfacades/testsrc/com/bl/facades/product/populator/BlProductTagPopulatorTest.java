package com.bl.facades.product.populator;

import com.bl.core.model.BlProductModel;
import com.bl.facades.populators.BlProductTagPopulator;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.data.ProductData;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.runners.MockitoJUnitRunner;

import static org.mockito.Mockito.when;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlProductTagPopulatorTest {

    @InjectMocks
    private final BlProductTagPopulator populator = Mockito.spy(new BlProductTagPopulator());


    public static String PRODUCT_NEW = "new";
    public static String PRODUCT_MOSTPOPULAR = "MOSTPOPULAR";
    public static String PRODUCT_GREATVALUE = "GREATVALUE";
    public static String PRODUCT_STAFFPICK = "STAFFPICK";
    public static String PRODUCT_FORRENT = "PRODUCT_FORRENT";

    @Mock
    BlProductModel productModel;
    @Mock
    ProductData productData;

    @Test
    public void populateProductTagDetails() {
        when(productModel.getIsNew()).thenReturn(Boolean.valueOf(PRODUCT_NEW));
        when(productModel.getMostPopular()).thenReturn(Boolean.valueOf(PRODUCT_MOSTPOPULAR));
        when(productModel.getGreatValue()).thenReturn(Boolean.valueOf(PRODUCT_GREATVALUE));
        when(productModel.getStaffPick()).thenReturn(Boolean.valueOf(PRODUCT_STAFFPICK));
        when(productModel.getForRent()).thenReturn(Boolean.valueOf(PRODUCT_FORRENT));

        populator.populate(productModel, productData);
        Assert.assertTrue(productModel.getForRent());
        Assert.assertTrue(StringUtils.isBlank(productData.getProductTagValues()));
    }
}
