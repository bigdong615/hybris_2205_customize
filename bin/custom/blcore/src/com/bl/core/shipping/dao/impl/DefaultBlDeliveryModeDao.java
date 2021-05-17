package com.bl.core.shipping.dao.impl;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.core.model.*;
import com.bl.core.shipping.dao.BlDeliveryModeDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.order.daos.impl.DefaultZoneDeliveryModeDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.store.BaseStoreModel;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collection;
import java.util.Collections;

public class DefaultBlDeliveryModeDao extends DefaultZoneDeliveryModeDao implements BlDeliveryModeDao {

    private static final Logger LOG = Logger.getLogger(DefaultBlDeliveryModeDao.class);

    @Autowired
    private FlexibleSearchService flexibleSearchService;

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ShippingGroupModel> getAllShippingGroups() {
        final String barcodeList = "select distinct{sg.pk} from {ShippingGroup as sg}, {ZoneDeliveryMode as zdm} " +
                "where {zdm.shippingGroup} = {sg.pk} and {zdm.active} = 1";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        final Collection<ShippingGroupModel> results = getFlexibleSearchService().<ShippingGroupModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_SHIPPING_GROUP);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModes(final String carrier, final String mode, final String pstCutOffTime,
                                                                        final boolean payByCustomer) {
        final StringBuilder barcodeList = new StringBuilder("select {zdm.pk} from {ZoneDeliveryMode as zdm}, {ShippingGroup as sg}, {CarrierEnum as ce} " +
                "where {sg.pk} = {zdm.shippingGroup} and {sg.code} = 'SHIP_HOME_HOTEL_BUSINESS' and {zdm.active} = 1 and " +
                "{zdm.carrier} = {ce.pk} and {ce.code} = ?carrier and {zdm.code} like '%" + mode + "%' and {zdm.payByCustomer} = ?payByCustomer");
        if (pstCutOffTime != null) {
            barcodeList.append(" and {zdm.cutOffTime} > ?pstCutOffTime");
        }
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("carrier", carrier);
        query.addQueryParameter("mode", mode);
        query.addQueryParameter(BlDeliveryModeLoggingConstants.PAY_BY_CUSTOMER, payByCustomer);
        if (pstCutOffTime != null) {
            query.addQueryParameter(BlDeliveryModeLoggingConstants.PST_CUT_OFF_TIME, pstCutOffTime);
        }
        final Collection<ZoneDeliveryModeModel> results = getFlexibleSearchService().<ZoneDeliveryModeModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_SHIP_TO_HOME_ZONE_DELIVERY_MODE + carrier);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesNotLike(final String carrier, final String mode,
                                                                               final String pstCutOffTime, final boolean payByCustomer) {
        final StringBuilder barcodeList = new StringBuilder("select {zdm.pk} from {ZoneDeliveryMode as zdm}, {ShippingGroup as sg}, " +
                "{CarrierEnum as ce} where {sg.pk} = {zdm.shippingGroup} and {sg.code} = 'SHIP_HOME_HOTEL_BUSINESS' and {zdm.active} = 1 and " +
                "{zdm.carrier} = {ce.pk} and {ce.code} = ?carrier and {zdm.code} like '%" + mode + "%' and {zdm.code} not like '%AM%'" +
                " and {zdm.payByCustomer} = ?payByCustomer");
        if (pstCutOffTime != null) {
            barcodeList.append(" and {zdm.cutOffTime} > ?pstCutOffTime");
        }
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("carrier", carrier);
        query.addQueryParameter("mode", mode);
        query.addQueryParameter(BlDeliveryModeLoggingConstants.PAY_BY_CUSTOMER, payByCustomer);
        if (pstCutOffTime != null) {
            query.addQueryParameter(BlDeliveryModeLoggingConstants.PST_CUT_OFF_TIME, pstCutOffTime);
        }
        final Collection<ZoneDeliveryModeModel> results = getFlexibleSearchService().<ZoneDeliveryModeModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_SHIP_TO_HOME_ZONE_DELIVERY_MODE_AM + carrier);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<PartnerPickUpStoreModel> getPartnerPickUpStore() {
        final String barcodeList = "select distinct{pickStore.pk} from {PartnerPickUpStore as pickStore}, " +
                "{BlPickUpZoneDeliveryMode as pick} where {pick.partnerPickUpStore} = {pickStore.pk} and {pick.active} = 1 ";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        final Collection<PartnerPickUpStoreModel> results = getFlexibleSearchService().<PartnerPickUpStoreModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_PARTNER_PICKUP_ZONE_GROUP);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    @Override
    public PartnerPickUpStoreModel getPartnerPickUpStoreFromPartnerZone(final String partnerZone) {
        final String barcodeList = "select distinct{pickStore.pk} from {PartnerPickUpStore as pickStore}, " +
                "{BlPickUpZoneDeliveryMode as pick} where {pick.partnerPickUpStore} = {pickStore.pk} and {pick.active} = 1 " +
                "and {pickStore.code} = ?partnerZone";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("partnerZone", partnerZone);
        final Collection<PartnerPickUpStoreModel> results = getFlexibleSearchService().<PartnerPickUpStoreModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_PARTNER_PICKUP_ZONE_GROUP);
        return CollectionUtils.isNotEmpty(results) ? results.iterator().next() : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModes(final String partnerZone, final boolean payByCustomer) {
        final StringBuilder barcodeList = new StringBuilder("select {pickZone.pk} from {BlPickUpZoneDeliveryMode as pickZone}, {ShippingGroup as sg}, " +
                "{PartnerPickUpStore as pickStore} where {sg.pk} = {pickZone.shippingGroup} and {sg.code} = 'BL_PARTNER_PICKUP' " +
                "and {pickZone.active} = 1 and {pickZone.partnerPickUpStore} = {pickStore.pk} and {pickStore.code} = ?partnerZone" +
                " and {pickZone.payByCustomer} = ?payByCustomer");
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("partnerZone", partnerZone);
        query.addQueryParameter(BlDeliveryModeLoggingConstants.PAY_BY_CUSTOMER, payByCustomer);
        final Collection<BlPickUpZoneDeliveryModeModel> results = getFlexibleSearchService().<BlPickUpZoneDeliveryModeModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_PARTNER_PICKUP_ZONE_DELIVERY_MODE + partnerZone);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModes(final String mode, final String pstCutOffTime,
                                                                                         final boolean payByCustomer) {
        final StringBuilder barcodeList = new StringBuilder("select {pickZone.pk} from {BlPickUpZoneDeliveryMode as pickZone}, {ShippingGroup as sg} " +
                "where {sg.pk} = {pickZone.shippingGroup} and {sg.code} = 'SHIP_HOLD_UPS_OFFICE' and {pickZone.active} = 1 and" +
                " {pickZone.code} like '%" + mode + "%' and {pickZone.payByCustomer} = ?payByCustomer");
        if (pstCutOffTime != null) {
            barcodeList.append(BlDeliveryModeLoggingConstants.PST_CUT_OFF_TIME_CONST);
        }
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("mode", mode);
        query.addQueryParameter(BlDeliveryModeLoggingConstants.PAY_BY_CUSTOMER, payByCustomer);
        if (pstCutOffTime != null) {
            query.addQueryParameter(BlDeliveryModeLoggingConstants.PST_CUT_OFF_TIME, pstCutOffTime);
        }
        final Collection<BlPickUpZoneDeliveryModeModel> results = getFlexibleSearchService().<BlPickUpZoneDeliveryModeModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_PARTNER_PICKUP_UPS_STORE_ZONE_DELIVERY_MODE);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModesNotLike(final String mode, final String pstCutOffTime,
                                                                                                final boolean payByCustomer) {
        final StringBuilder barcodeList = new StringBuilder("select {pickZone.pk} from {BlPickUpZoneDeliveryMode as pickZone}, {ShippingGroup as sg} " +
                "where {sg.pk} = {pickZone.shippingGroup} and {sg.code} = 'SHIP_HOLD_UPS_OFFICE' and {pickZone.active} = 1 and" +
                " {pickZone.code} like '%" + mode + "%' and {zdm.code} not like '%AM%' and {pickZone.payByCustomer} = ?payByCustomer");
        if (pstCutOffTime != null) {
            barcodeList.append(BlDeliveryModeLoggingConstants.PST_CUT_OFF_TIME_CONST);
        }
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("mode", mode);
        query.addQueryParameter(BlDeliveryModeLoggingConstants.PAY_BY_CUSTOMER, payByCustomer);
        if (pstCutOffTime != null) {
            query.addQueryParameter(BlDeliveryModeLoggingConstants.PST_CUT_OFF_TIME, pstCutOffTime);
        }
        final Collection<BlPickUpZoneDeliveryModeModel> results = getFlexibleSearchService().<BlPickUpZoneDeliveryModeModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_PARTNER_PICKUP_UPS_STORE_ZONE_DELIVERY_MODE_AM);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlRushDeliveryModeModel> getBlRushDeliveryModes(final String deliveryMode, final String pstCutOffTime,
                                                                      final boolean payByCustomer) {
        final StringBuilder barcodeList = new StringBuilder("select {rush.pk} from {BlRushDeliveryMode as rush}, {DeliveryTypeEnum as dt} " +
                "where {dt.pk} = {rush.deliveryType} and {dt.code} = ?deliveryMode and {rush.active} = 1 and {rush.payByCustomer} = ?payByCustomer");
        if (pstCutOffTime != null) {
            barcodeList.append(" and {rush.cutOffTime} > ?pstCutOffTime");
        }
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("deliveryMode", deliveryMode);
        query.addQueryParameter(BlDeliveryModeLoggingConstants.PAY_BY_CUSTOMER, payByCustomer);
        if (pstCutOffTime != null) {
            query.addQueryParameter(BlDeliveryModeLoggingConstants.PST_CUT_OFF_TIME, pstCutOffTime);
        }
        final Collection<BlRushDeliveryModeModel> results = getFlexibleSearchService().<BlRushDeliveryModeModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_RUSH_ZONE_DELIVERY_MODE + deliveryMode);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BlRushDeliveryModeModel getBlRushDeliveryModeForWarehouseZipCode(final String deliveryMode, final boolean payByCustomer) {
        final StringBuilder barcodeList = new StringBuilder("select {rush.pk} from {BlRushDeliveryMode as rush}, {DeliveryTypeEnum as dt} " +
                "where {dt.pk} = {rush.deliveryType} and {dt.code} = ?deliveryMode and {rush.active} = 1 and {rush.payByCustomer} = ?payByCustomer");
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("deliveryMode", deliveryMode);
        query.addQueryParameter(BlDeliveryModeLoggingConstants.PAY_BY_CUSTOMER, payByCustomer);
        final Collection<BlRushDeliveryModeModel> results = getFlexibleSearchService().<BlRushDeliveryModeModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_RUSH_ZONE_DELIVERY_MODE + deliveryMode);
        return CollectionUtils.isNotEmpty(results) ? results.iterator().next() : null;
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public ShippingCostModel getShippingCostForCalculatedDeliveryCost(final String calculatedCost, final String deliveryMethod,
                                                                      final boolean payByCustomer) {
        final String barcodeList = "select {sc.pk} from {ShippingCost as sc}, {ZoneDeliveryMode as zone} " +
                "where {sc.zoneDeliveryMode} = {zone.pk} and {zone.active} = 1 and {zone.code} = ?deliveryMethod and " +
                "{sc.floor} < ?calculatedCost and ?calculatedCost <= {sc.ceiling} and {zone.payByCustomer} = ?payByCustomer";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("deliveryMethod", deliveryMethod);
        query.addQueryParameter("calculatedCost", calculatedCost);
        query.addQueryParameter("payByCustomer", payByCustomer);
        final Collection<ShippingCostModel> results = getFlexibleSearchService().<ShippingCostModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.FETCH_SHIPPING_COST);
        return CollectionUtils.isNotEmpty(results) ? results.iterator().next() : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Integer getDimensionalFactorForDeliveryFromStore(final String baseStore) {
        final String barcodeList = "select {base.pk} from {BaseStore as base} where {base.uid} = ?baseStore";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("baseStore", baseStore);
        final Collection<BaseStoreModel> store = getFlexibleSearchService().<BaseStoreModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.BASE_STORE_DIMENSIONAL_FACTOR);
        return CollectionUtils.isNotEmpty(store) ? store.iterator().next().getDimensionalFactor() : BlDeliveryModeLoggingConstants.DIMENSIONAL_FACTOR;
    }

    @Override
    public FlexibleSearchService getFlexibleSearchService() {
        return flexibleSearchService;
    }

    @Override
    public void setFlexibleSearchService(FlexibleSearchService flexibleSearchService) {
        this.flexibleSearchService = flexibleSearchService;
    }
}
