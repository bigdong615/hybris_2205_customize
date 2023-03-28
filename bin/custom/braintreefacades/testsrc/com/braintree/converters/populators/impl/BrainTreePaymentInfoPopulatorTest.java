/**
 *
 */
package com.braintree.converters.populators.impl;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.braintree.enums.BrainTreeCardType;
import com.braintree.model.BrainTreePaymentInfoModel;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BrainTreePaymentInfoPopulatorTest
{
	public static final long ID = 1234567890L;

	public static final String PK_ID = "PK_ID";

	public static final String CARD_NUMBER = "CARD_NUMBER";

	public static final String EXPIRATION_YEAR = "EXPIRATION_YEAR";

	public static final String EXPIRATION_MONTH = "EXPIRATION_MONTH";

	public static final String ACCOUNT_HOLDER_NAME = "ACCOUNT_HOLDER_NAME";

	public static final String SUBSCRIPTION_ID = "SUBSCRIPTION_ID";

	public static final String CARD_TYPE = "CARD_TYPE";

	public static final boolean SAVED = true;

	@Mock
	private Converter<AddressModel, AddressData> addressConverter;

	@InjectMocks
	private final BrainTreePaymentInfoPopulator brainTreePaymentInfoPopulator = new BrainTreePaymentInfoPopulator();

	@Before
	public void init()
	{
		//MockitoAnnotations.initMocks(this);
	}

	@Test
	public void populateCCPaymentInfoData()
	{
		// given
		final BrainTreePaymentInfoModel brainTreePaymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		final CCPaymentInfoData ccPaymentInfoData = mock(CCPaymentInfoData.class);
		final PK pk = PK.fromLong(ID);
		final BrainTreeCardType brainTreeCardType = mock(BrainTreeCardType.class);
		final AddressModel addressModel = mock(AddressModel.class);
		final AddressData addressData = mock(AddressData.class);
		when(brainTreePaymentInfoModel.getPk()).thenReturn(pk);
		when(brainTreePaymentInfoModel.getCardNumber()).thenReturn(CARD_NUMBER);
		when(brainTreePaymentInfoModel.getExpirationYear()).thenReturn(EXPIRATION_YEAR);
		when(brainTreePaymentInfoModel.getExpirationMonth()).thenReturn(EXPIRATION_MONTH);
		when(brainTreePaymentInfoModel.getImageSource()).thenReturn(ACCOUNT_HOLDER_NAME);
		when(brainTreePaymentInfoModel.getPaymentProvider()).thenReturn(SUBSCRIPTION_ID);
		when(Boolean.valueOf(brainTreePaymentInfoModel.isSaved())).thenReturn(Boolean.valueOf(SAVED));
		when(brainTreePaymentInfoModel.getCardType()).thenReturn(brainTreeCardType);
		when(brainTreePaymentInfoModel.getCardType().toString()).thenReturn(CARD_TYPE);
		when(brainTreePaymentInfoModel.getBillingAddress()).thenReturn(addressModel);
		when(addressConverter.convert(addressModel)).thenReturn(addressData);

		// when
		brainTreePaymentInfoPopulator.populate(brainTreePaymentInfoModel, ccPaymentInfoData);

		//then
		verify(ccPaymentInfoData).setId(pk.toString());
		verify(ccPaymentInfoData).setCardNumber(CARD_NUMBER);
		verify(ccPaymentInfoData).setExpiryYear(EXPIRATION_YEAR);
		verify(ccPaymentInfoData).setExpiryMonth(EXPIRATION_MONTH);
		verify(ccPaymentInfoData).setAccountHolderName(ACCOUNT_HOLDER_NAME);
		verify(ccPaymentInfoData).setSubscriptionId(SUBSCRIPTION_ID);
		verify(ccPaymentInfoData).setSaved(SAVED);
		verify(ccPaymentInfoData).setCardType(CARD_TYPE);
		verify(ccPaymentInfoData).setBillingAddress(addressData);
	}

}
