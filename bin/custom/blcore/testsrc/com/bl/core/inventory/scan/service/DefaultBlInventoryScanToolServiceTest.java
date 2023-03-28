package com.bl.core.inventory.scan.service;

import static org.mockito.Mockito.mock;

import de.hybris.bootstrap.annotations.UnitTest;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.inventory.scan.dao.impl.DefaultBlInventoryScanToolDao;
import com.bl.core.inventory.scan.service.impl.DefaultBlInventoryScanToolService;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlInventoryScanToolServiceTest {

    @Mock
    private BlInventoryScanToolDao blInventoryScanToolDao;

    private DefaultBlInventoryScanToolService blDefaultInventoryScanToolService;

    final Collection<String> sampleBarcodes = new ArrayList<>();
    final List<String> sampleList = new ArrayList<>();

    @Before
    public void setUp() throws Exception {
		 //  MockitoAnnotations.initMocks(this);
        blDefaultInventoryScanToolService = new DefaultBlInventoryScanToolService();
        blInventoryScanToolDao = mock(DefaultBlInventoryScanToolDao.class);
        blDefaultInventoryScanToolService.setBlInventoryScanToolDao(blInventoryScanToolDao);
        sampleBarcodes.add(BlInventoryScanLoggingConstants.SAMPLE1);
        sampleBarcodes.add(BlInventoryScanLoggingConstants.SAMPLE2);
        sampleBarcodes.add(BlInventoryScanLoggingConstants.SAMPLE3);
        sampleList.add(BlInventoryScanLoggingConstants.LOCATION);
    }

    @Test
    public void testGetInventoryLocationByIdForLocationNull() {
        Assert.assertNull(blDefaultInventoryScanToolService.getInventoryLocationById(null));
    }

    @Test
    public void testGetInventoryLocationByIdForLocationEmpty() {
        Assert.assertNull(blDefaultInventoryScanToolService.getInventoryLocationById(StringUtils.EMPTY));
    }

    @Test
    public void testGetInventoryLocationByIdForLocationNotExist() {
        Assert.assertNull(blDefaultInventoryScanToolService.
                getInventoryLocationById(BlInventoryScanLoggingConstants.SAMPLE1));
    }

    @Test
    public void testGetSerialProductsByBarcodeForBarcodesNull() {
        Assert.assertEquals(Collections.emptyList(),
                blDefaultInventoryScanToolService.getSerialProductsByBarcode(null));
    }

    @Test
    public void testGetSerialProductsByBarcodeForBarcodesEmpty() {
        Assert.assertEquals(Collections.emptyList(), blDefaultInventoryScanToolService.
                getSerialProductsByBarcode(Collections.emptyList()));
    }

    @Test
    public void testGetSerialProductsByBarcodeForBarcodesNotExist() {
        Assert.assertEquals(Collections.emptyList(), blDefaultInventoryScanToolService.
                getSerialProductsByBarcode(sampleBarcodes));
    }

    @Test(expected = NullPointerException.class)
    public void testCheckValidLocationInBarcodeListForBarcodesNull() {
        blDefaultInventoryScanToolService.checkValidLocationInBarcodeList(null, Collections.emptyList());
    }

    @Test(expected = IndexOutOfBoundsException.class)
    public void testCheckValidLocationInBarcodeListForBarcodesEmpty() {
        blDefaultInventoryScanToolService.checkValidLocationInBarcodeList(Collections.emptyList(), Collections.emptyList());
    }

    @Test
    public void testCheckValidLocationInBarcodeListForBarcodesExists() {
        Assert.assertEquals(BlInventoryScanLoggingConstants.THREE,
                blDefaultInventoryScanToolService.checkValidLocationInBarcodeList((List<String>) sampleBarcodes, Collections.emptyList()));
    }

    @Test
    public void testCheckValidInventoryLocationForEmptyFilteredList() {
        Assert.assertEquals(BlInventoryScanLoggingConstants.THREE,
                blDefaultInventoryScanToolService.checkValidInventoryLocation(StringUtils.EMPTY,
                        Collections.emptyList(), Collections.emptyList()));
    }

    @Test
    public void testCheckValidInventoryLocationForNotEmptyFilteredListWithNonMatchingLocation() {
        Assert.assertEquals(BlInventoryScanLoggingConstants.FOUR,
                blDefaultInventoryScanToolService.checkValidInventoryLocation(StringUtils.EMPTY,
                        (List<String>) sampleBarcodes, Collections.emptyList()));
    }

    @Test(expected = NullPointerException.class)
    public void testValidateLocationForFilteredListIsNull() {
        blDefaultInventoryScanToolService.validateLocation(StringUtils.EMPTY, null, Collections.emptyList());
    }

    @Test(expected = IndexOutOfBoundsException.class)
    public void testValidateLocationForFilteredListIfEmpty() {
        blDefaultInventoryScanToolService.validateLocation(StringUtils.EMPTY, Collections.emptyList(), Collections.emptyList());
    }

    @Test
    public void testValidateLocationForInventoryLocationIsNull() {
        Assert.assertEquals(BlInventoryScanLoggingConstants.THREE,
                blDefaultInventoryScanToolService.validateLocation(null, (List<String>) sampleBarcodes, Collections.emptyList()));
    }

    @Test
    public void testValidateLocationForInventoryLocationIfEmpty() {
        Assert.assertEquals(BlInventoryScanLoggingConstants.THREE,
                blDefaultInventoryScanToolService.validateLocation(StringUtils.EMPTY, (List<String>) sampleBarcodes, Collections.emptyList()));
    }

    @Test
    public void testValidateLocationForInventoryLocationIfMatchButNoRecord() {
        Assert.assertEquals(BlInventoryScanLoggingConstants.TWO,
                blDefaultInventoryScanToolService.validateLocation(BlInventoryScanLoggingConstants.SAMPLE1,
                        (List<String>) sampleBarcodes, Collections.emptyList()));
    }

    @Test(expected = NullPointerException.class)
    public void testGetFailedBarcodeListIfBarcodesNull() {
        blDefaultInventoryScanToolService.getFailedBarcodeList(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetFailedBarcodeListIfBarcodesEmpty() {
        blDefaultInventoryScanToolService.getFailedBarcodeList(Collections.emptyList());
    }

    @Test
    public void testGetConfigKeyFromScanConfigurationIfKeyNull() {
        Assert.assertEquals(String.valueOf(BlInventoryScanLoggingConstants.TEN),
                blDefaultInventoryScanToolService.getConfigKeyFromScanConfiguration(null));
    }

    @Test
    public void testGetConfigKeyFromScanConfigurationIfEmpty() {
        Assert.assertEquals(String.valueOf(BlInventoryScanLoggingConstants.TEN),
                blDefaultInventoryScanToolService.getConfigKeyFromScanConfiguration(StringUtils.EMPTY));
    }

    @Test(expected = NullPointerException.class)
    public void testSetInventoryLocationOnSerialIfBlSerialProductsNull() {
        blDefaultInventoryScanToolService.setInventoryLocationOnSerial(null, null,
                null);
    }

    @Test(expected = NullPointerException.class)
    public void testSetInventoryLocationOnSerialIfBlSerialProductsEmpty() {
        blDefaultInventoryScanToolService.setInventoryLocationOnSerial(null, Collections.emptyList(),
                null);
    }

}
