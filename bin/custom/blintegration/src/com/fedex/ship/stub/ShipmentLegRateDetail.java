/**
 * ShipmentLegRateDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;


/**
 * Data for a single leg of a shipment's total/summary rates, as calculated
 * per a specific rate type.
 */
public class ShipmentLegRateDetail  implements java.io.Serializable {
    /* Human-readable text describing the shipment leg. */
    private java.lang.String legDescription;

    /* Origin for this leg. */
    private com.fedex.ship.stub.Address legOrigin;

    /* Specifies the location id the origin of shipment leg. */
    private java.lang.String legOriginLocationId;

    /* Destination for this leg. */
    private com.fedex.ship.stub.Address legDestination;

    /* Specifies the location id the destination of shipment leg. */
    private java.lang.String legDestinationLocationId;

    /* Type used for this specific set of rate data. */
    private com.fedex.ship.stub.ReturnedRateType rateType;

    /* Indicates the rate scale used. */
    private java.lang.String rateScale;

    /* Indicates the rate zone used (based on origin and destination). */
    private java.lang.String rateZone;

    private com.fedex.ship.stub.PricingCodeType pricingCode;

    /* Indicates which weight was used. */
    private com.fedex.ship.stub.RatedWeightMethod ratedWeightMethod;

    /* INTERNAL FEDEX USE ONLY. */
    private com.fedex.ship.stub.MinimumChargeType minimumChargeType;

    /* Specifies the currency exchange performed on financial amounts
     * for this rate. */
    private com.fedex.ship.stub.CurrencyExchangeRate currencyExchangeRate;

    /* Indicates which special rating cases applied to this shipment. */
    private com.fedex.ship.stub.SpecialRatingAppliedType[] specialRatingApplied;

    private org.apache.axis.types.NonNegativeInteger dimDivisor;

    /* Identifies the type of dim divisor that was applied. */
    private com.fedex.ship.stub.RateDimensionalDivisorType dimDivisorType;

    private java.math.BigDecimal fuelSurchargePercent;

    private com.fedex.ship.stub.Weight totalBillingWeight;

    /* Sum of dimensional weights for all packages. */
    private com.fedex.ship.stub.Weight totalDimWeight;

    private com.fedex.ship.stub.Money totalBaseCharge;

    private com.fedex.ship.stub.Money totalFreightDiscounts;

    private com.fedex.ship.stub.Money totalNetFreight;

    private com.fedex.ship.stub.Money totalSurcharges;

    /* This shipment's totalNetFreight + totalSurcharges (not including
     * totalTaxes). */
    private com.fedex.ship.stub.Money totalNetFedExCharge;

    /* Total of the transportation-based taxes. */
    private com.fedex.ship.stub.Money totalTaxes;

    private com.fedex.ship.stub.Money totalNetCharge;

    private com.fedex.ship.stub.Money totalRebates;

    /* Total of all values under this shipment's dutiesAndTaxes; only
     * provided if estimated duties and taxes were calculated for this shipment. */
    private com.fedex.ship.stub.Money totalDutiesAndTaxes;

    /* This shipment's totalNetCharge + totalDutiesAndTaxes; only
     * provided if estimated duties and taxes were calculated for this shipment
     * AND duties, taxes and transportation charges are all paid by the same
     * sender's account. */
    private com.fedex.ship.stub.Money totalNetChargeWithDutiesAndTaxes;

    /* Rate data specific to FedEx Freight and FedEx National Freight
     * services. */
    private com.fedex.ship.stub.FreightRateDetail freightRateDetail;

    /* All rate discounts that apply to this shipment. */
    private com.fedex.ship.stub.RateDiscount[] freightDiscounts;

    /* All rebates that apply to this shipment. */
    private com.fedex.ship.stub.Rebate[] rebates;

    /* All surcharges that apply to this shipment. */
    private com.fedex.ship.stub.Surcharge[] surcharges;

    /* All transportation-based taxes applicable to this shipment. */
    private com.fedex.ship.stub.Tax[] taxes;

    /* All commodity-based duties and taxes applicable to this shipment. */
    private com.fedex.ship.stub.EdtCommodityTax[] dutiesAndTaxes;

    /* The "order level" variable handling charges. */
    private com.fedex.ship.stub.VariableHandlingCharges variableHandlingCharges;

    /* The total of all variable handling charges at both shipment
     * (order) and package level. */
    private com.fedex.ship.stub.VariableHandlingCharges totalVariableHandlingCharges;

    public ShipmentLegRateDetail() {
    }

    public ShipmentLegRateDetail(
           java.lang.String legDescription,
           com.fedex.ship.stub.Address legOrigin,
           java.lang.String legOriginLocationId,
           com.fedex.ship.stub.Address legDestination,
           java.lang.String legDestinationLocationId,
           com.fedex.ship.stub.ReturnedRateType rateType,
           java.lang.String rateScale,
           java.lang.String rateZone,
           com.fedex.ship.stub.PricingCodeType pricingCode,
           com.fedex.ship.stub.RatedWeightMethod ratedWeightMethod,
           com.fedex.ship.stub.MinimumChargeType minimumChargeType,
           com.fedex.ship.stub.CurrencyExchangeRate currencyExchangeRate,
           com.fedex.ship.stub.SpecialRatingAppliedType[] specialRatingApplied,
           org.apache.axis.types.NonNegativeInteger dimDivisor,
           com.fedex.ship.stub.RateDimensionalDivisorType dimDivisorType,
           java.math.BigDecimal fuelSurchargePercent,
           com.fedex.ship.stub.Weight totalBillingWeight,
           com.fedex.ship.stub.Weight totalDimWeight,
           com.fedex.ship.stub.Money totalBaseCharge,
           com.fedex.ship.stub.Money totalFreightDiscounts,
           com.fedex.ship.stub.Money totalNetFreight,
           com.fedex.ship.stub.Money totalSurcharges,
           com.fedex.ship.stub.Money totalNetFedExCharge,
           com.fedex.ship.stub.Money totalTaxes,
           com.fedex.ship.stub.Money totalNetCharge,
           com.fedex.ship.stub.Money totalRebates,
           com.fedex.ship.stub.Money totalDutiesAndTaxes,
           com.fedex.ship.stub.Money totalNetChargeWithDutiesAndTaxes,
           com.fedex.ship.stub.FreightRateDetail freightRateDetail,
           com.fedex.ship.stub.RateDiscount[] freightDiscounts,
           com.fedex.ship.stub.Rebate[] rebates,
           com.fedex.ship.stub.Surcharge[] surcharges,
           com.fedex.ship.stub.Tax[] taxes,
           com.fedex.ship.stub.EdtCommodityTax[] dutiesAndTaxes,
           com.fedex.ship.stub.VariableHandlingCharges variableHandlingCharges,
           com.fedex.ship.stub.VariableHandlingCharges totalVariableHandlingCharges) {
           this.legDescription = legDescription;
           this.legOrigin = legOrigin;
           this.legOriginLocationId = legOriginLocationId;
           this.legDestination = legDestination;
           this.legDestinationLocationId = legDestinationLocationId;
           this.rateType = rateType;
           this.rateScale = rateScale;
           this.rateZone = rateZone;
           this.pricingCode = pricingCode;
           this.ratedWeightMethod = ratedWeightMethod;
           this.minimumChargeType = minimumChargeType;
           this.currencyExchangeRate = currencyExchangeRate;
           this.specialRatingApplied = specialRatingApplied;
           this.dimDivisor = dimDivisor;
           this.dimDivisorType = dimDivisorType;
           this.fuelSurchargePercent = fuelSurchargePercent;
           this.totalBillingWeight = totalBillingWeight;
           this.totalDimWeight = totalDimWeight;
           this.totalBaseCharge = totalBaseCharge;
           this.totalFreightDiscounts = totalFreightDiscounts;
           this.totalNetFreight = totalNetFreight;
           this.totalSurcharges = totalSurcharges;
           this.totalNetFedExCharge = totalNetFedExCharge;
           this.totalTaxes = totalTaxes;
           this.totalNetCharge = totalNetCharge;
           this.totalRebates = totalRebates;
           this.totalDutiesAndTaxes = totalDutiesAndTaxes;
           this.totalNetChargeWithDutiesAndTaxes = totalNetChargeWithDutiesAndTaxes;
           this.freightRateDetail = freightRateDetail;
           this.freightDiscounts = freightDiscounts;
           this.rebates = rebates;
           this.surcharges = surcharges;
           this.taxes = taxes;
           this.dutiesAndTaxes = dutiesAndTaxes;
           this.variableHandlingCharges = variableHandlingCharges;
           this.totalVariableHandlingCharges = totalVariableHandlingCharges;
    }


    /**
     * Gets the legDescription value for this ShipmentLegRateDetail.
     * 
     * @return legDescription   * Human-readable text describing the shipment leg.
     */
    public java.lang.String getLegDescription() {
        return legDescription;
    }


    /**
     * Sets the legDescription value for this ShipmentLegRateDetail.
     * 
     * @param legDescription   * Human-readable text describing the shipment leg.
     */
    public void setLegDescription(java.lang.String legDescription) {
        this.legDescription = legDescription;
    }


    /**
     * Gets the legOrigin value for this ShipmentLegRateDetail.
     * 
     * @return legOrigin   * Origin for this leg.
     */
    public com.fedex.ship.stub.Address getLegOrigin() {
        return legOrigin;
    }


    /**
     * Sets the legOrigin value for this ShipmentLegRateDetail.
     * 
     * @param legOrigin   * Origin for this leg.
     */
    public void setLegOrigin(com.fedex.ship.stub.Address legOrigin) {
        this.legOrigin = legOrigin;
    }


    /**
     * Gets the legOriginLocationId value for this ShipmentLegRateDetail.
     * 
     * @return legOriginLocationId   * Specifies the location id the origin of shipment leg.
     */
    public java.lang.String getLegOriginLocationId() {
        return legOriginLocationId;
    }


    /**
     * Sets the legOriginLocationId value for this ShipmentLegRateDetail.
     * 
     * @param legOriginLocationId   * Specifies the location id the origin of shipment leg.
     */
    public void setLegOriginLocationId(java.lang.String legOriginLocationId) {
        this.legOriginLocationId = legOriginLocationId;
    }


    /**
     * Gets the legDestination value for this ShipmentLegRateDetail.
     * 
     * @return legDestination   * Destination for this leg.
     */
    public com.fedex.ship.stub.Address getLegDestination() {
        return legDestination;
    }


    /**
     * Sets the legDestination value for this ShipmentLegRateDetail.
     * 
     * @param legDestination   * Destination for this leg.
     */
    public void setLegDestination(com.fedex.ship.stub.Address legDestination) {
        this.legDestination = legDestination;
    }


    /**
     * Gets the legDestinationLocationId value for this ShipmentLegRateDetail.
     * 
     * @return legDestinationLocationId   * Specifies the location id the destination of shipment leg.
     */
    public java.lang.String getLegDestinationLocationId() {
        return legDestinationLocationId;
    }


    /**
     * Sets the legDestinationLocationId value for this ShipmentLegRateDetail.
     * 
     * @param legDestinationLocationId   * Specifies the location id the destination of shipment leg.
     */
    public void setLegDestinationLocationId(java.lang.String legDestinationLocationId) {
        this.legDestinationLocationId = legDestinationLocationId;
    }


    /**
     * Gets the rateType value for this ShipmentLegRateDetail.
     * 
     * @return rateType   * Type used for this specific set of rate data.
     */
    public com.fedex.ship.stub.ReturnedRateType getRateType() {
        return rateType;
    }


    /**
     * Sets the rateType value for this ShipmentLegRateDetail.
     * 
     * @param rateType   * Type used for this specific set of rate data.
     */
    public void setRateType(com.fedex.ship.stub.ReturnedRateType rateType) {
        this.rateType = rateType;
    }


    /**
     * Gets the rateScale value for this ShipmentLegRateDetail.
     * 
     * @return rateScale   * Indicates the rate scale used.
     */
    public java.lang.String getRateScale() {
        return rateScale;
    }


    /**
     * Sets the rateScale value for this ShipmentLegRateDetail.
     * 
     * @param rateScale   * Indicates the rate scale used.
     */
    public void setRateScale(java.lang.String rateScale) {
        this.rateScale = rateScale;
    }


    /**
     * Gets the rateZone value for this ShipmentLegRateDetail.
     * 
     * @return rateZone   * Indicates the rate zone used (based on origin and destination).
     */
    public java.lang.String getRateZone() {
        return rateZone;
    }


    /**
     * Sets the rateZone value for this ShipmentLegRateDetail.
     * 
     * @param rateZone   * Indicates the rate zone used (based on origin and destination).
     */
    public void setRateZone(java.lang.String rateZone) {
        this.rateZone = rateZone;
    }


    /**
     * Gets the pricingCode value for this ShipmentLegRateDetail.
     * 
     * @return pricingCode
     */
    public com.fedex.ship.stub.PricingCodeType getPricingCode() {
        return pricingCode;
    }


    /**
     * Sets the pricingCode value for this ShipmentLegRateDetail.
     * 
     * @param pricingCode
     */
    public void setPricingCode(com.fedex.ship.stub.PricingCodeType pricingCode) {
        this.pricingCode = pricingCode;
    }


    /**
     * Gets the ratedWeightMethod value for this ShipmentLegRateDetail.
     * 
     * @return ratedWeightMethod   * Indicates which weight was used.
     */
    public com.fedex.ship.stub.RatedWeightMethod getRatedWeightMethod() {
        return ratedWeightMethod;
    }


    /**
     * Sets the ratedWeightMethod value for this ShipmentLegRateDetail.
     * 
     * @param ratedWeightMethod   * Indicates which weight was used.
     */
    public void setRatedWeightMethod(com.fedex.ship.stub.RatedWeightMethod ratedWeightMethod) {
        this.ratedWeightMethod = ratedWeightMethod;
    }


    /**
     * Gets the minimumChargeType value for this ShipmentLegRateDetail.
     * 
     * @return minimumChargeType   * INTERNAL FEDEX USE ONLY.
     */
    public com.fedex.ship.stub.MinimumChargeType getMinimumChargeType() {
        return minimumChargeType;
    }


    /**
     * Sets the minimumChargeType value for this ShipmentLegRateDetail.
     * 
     * @param minimumChargeType   * INTERNAL FEDEX USE ONLY.
     */
    public void setMinimumChargeType(com.fedex.ship.stub.MinimumChargeType minimumChargeType) {
        this.minimumChargeType = minimumChargeType;
    }


    /**
     * Gets the currencyExchangeRate value for this ShipmentLegRateDetail.
     * 
     * @return currencyExchangeRate   * Specifies the currency exchange performed on financial amounts
     * for this rate.
     */
    public com.fedex.ship.stub.CurrencyExchangeRate getCurrencyExchangeRate() {
        return currencyExchangeRate;
    }


    /**
     * Sets the currencyExchangeRate value for this ShipmentLegRateDetail.
     * 
     * @param currencyExchangeRate   * Specifies the currency exchange performed on financial amounts
     * for this rate.
     */
    public void setCurrencyExchangeRate(com.fedex.ship.stub.CurrencyExchangeRate currencyExchangeRate) {
        this.currencyExchangeRate = currencyExchangeRate;
    }


    /**
     * Gets the specialRatingApplied value for this ShipmentLegRateDetail.
     * 
     * @return specialRatingApplied   * Indicates which special rating cases applied to this shipment.
     */
    public com.fedex.ship.stub.SpecialRatingAppliedType[] getSpecialRatingApplied() {
        return specialRatingApplied;
    }


    /**
     * Sets the specialRatingApplied value for this ShipmentLegRateDetail.
     * 
     * @param specialRatingApplied   * Indicates which special rating cases applied to this shipment.
     */
    public void setSpecialRatingApplied(com.fedex.ship.stub.SpecialRatingAppliedType[] specialRatingApplied) {
        this.specialRatingApplied = specialRatingApplied;
    }

    public com.fedex.ship.stub.SpecialRatingAppliedType getSpecialRatingApplied(int i) {
        return this.specialRatingApplied[i];
    }

    public void setSpecialRatingApplied(int i, com.fedex.ship.stub.SpecialRatingAppliedType _value) {
        this.specialRatingApplied[i] = _value;
    }


    /**
     * Gets the dimDivisor value for this ShipmentLegRateDetail.
     * 
     * @return dimDivisor
     */
    public org.apache.axis.types.NonNegativeInteger getDimDivisor() {
        return dimDivisor;
    }


    /**
     * Sets the dimDivisor value for this ShipmentLegRateDetail.
     * 
     * @param dimDivisor
     */
    public void setDimDivisor(org.apache.axis.types.NonNegativeInteger dimDivisor) {
        this.dimDivisor = dimDivisor;
    }


    /**
     * Gets the dimDivisorType value for this ShipmentLegRateDetail.
     * 
     * @return dimDivisorType   * Identifies the type of dim divisor that was applied.
     */
    public com.fedex.ship.stub.RateDimensionalDivisorType getDimDivisorType() {
        return dimDivisorType;
    }


    /**
     * Sets the dimDivisorType value for this ShipmentLegRateDetail.
     * 
     * @param dimDivisorType   * Identifies the type of dim divisor that was applied.
     */
    public void setDimDivisorType(com.fedex.ship.stub.RateDimensionalDivisorType dimDivisorType) {
        this.dimDivisorType = dimDivisorType;
    }


    /**
     * Gets the fuelSurchargePercent value for this ShipmentLegRateDetail.
     * 
     * @return fuelSurchargePercent
     */
    public java.math.BigDecimal getFuelSurchargePercent() {
        return fuelSurchargePercent;
    }


    /**
     * Sets the fuelSurchargePercent value for this ShipmentLegRateDetail.
     * 
     * @param fuelSurchargePercent
     */
    public void setFuelSurchargePercent(java.math.BigDecimal fuelSurchargePercent) {
        this.fuelSurchargePercent = fuelSurchargePercent;
    }


    /**
     * Gets the totalBillingWeight value for this ShipmentLegRateDetail.
     * 
     * @return totalBillingWeight
     */
    public com.fedex.ship.stub.Weight getTotalBillingWeight() {
        return totalBillingWeight;
    }


    /**
     * Sets the totalBillingWeight value for this ShipmentLegRateDetail.
     * 
     * @param totalBillingWeight
     */
    public void setTotalBillingWeight(com.fedex.ship.stub.Weight totalBillingWeight) {
        this.totalBillingWeight = totalBillingWeight;
    }


    /**
     * Gets the totalDimWeight value for this ShipmentLegRateDetail.
     * 
     * @return totalDimWeight   * Sum of dimensional weights for all packages.
     */
    public com.fedex.ship.stub.Weight getTotalDimWeight() {
        return totalDimWeight;
    }


    /**
     * Sets the totalDimWeight value for this ShipmentLegRateDetail.
     * 
     * @param totalDimWeight   * Sum of dimensional weights for all packages.
     */
    public void setTotalDimWeight(com.fedex.ship.stub.Weight totalDimWeight) {
        this.totalDimWeight = totalDimWeight;
    }


    /**
     * Gets the totalBaseCharge value for this ShipmentLegRateDetail.
     * 
     * @return totalBaseCharge
     */
    public com.fedex.ship.stub.Money getTotalBaseCharge() {
        return totalBaseCharge;
    }


    /**
     * Sets the totalBaseCharge value for this ShipmentLegRateDetail.
     * 
     * @param totalBaseCharge
     */
    public void setTotalBaseCharge(com.fedex.ship.stub.Money totalBaseCharge) {
        this.totalBaseCharge = totalBaseCharge;
    }


    /**
     * Gets the totalFreightDiscounts value for this ShipmentLegRateDetail.
     * 
     * @return totalFreightDiscounts
     */
    public com.fedex.ship.stub.Money getTotalFreightDiscounts() {
        return totalFreightDiscounts;
    }


    /**
     * Sets the totalFreightDiscounts value for this ShipmentLegRateDetail.
     * 
     * @param totalFreightDiscounts
     */
    public void setTotalFreightDiscounts(com.fedex.ship.stub.Money totalFreightDiscounts) {
        this.totalFreightDiscounts = totalFreightDiscounts;
    }


    /**
     * Gets the totalNetFreight value for this ShipmentLegRateDetail.
     * 
     * @return totalNetFreight
     */
    public com.fedex.ship.stub.Money getTotalNetFreight() {
        return totalNetFreight;
    }


    /**
     * Sets the totalNetFreight value for this ShipmentLegRateDetail.
     * 
     * @param totalNetFreight
     */
    public void setTotalNetFreight(com.fedex.ship.stub.Money totalNetFreight) {
        this.totalNetFreight = totalNetFreight;
    }


    /**
     * Gets the totalSurcharges value for this ShipmentLegRateDetail.
     * 
     * @return totalSurcharges
     */
    public com.fedex.ship.stub.Money getTotalSurcharges() {
        return totalSurcharges;
    }


    /**
     * Sets the totalSurcharges value for this ShipmentLegRateDetail.
     * 
     * @param totalSurcharges
     */
    public void setTotalSurcharges(com.fedex.ship.stub.Money totalSurcharges) {
        this.totalSurcharges = totalSurcharges;
    }


    /**
     * Gets the totalNetFedExCharge value for this ShipmentLegRateDetail.
     * 
     * @return totalNetFedExCharge   * This shipment's totalNetFreight + totalSurcharges (not including
     * totalTaxes).
     */
    public com.fedex.ship.stub.Money getTotalNetFedExCharge() {
        return totalNetFedExCharge;
    }


    /**
     * Sets the totalNetFedExCharge value for this ShipmentLegRateDetail.
     * 
     * @param totalNetFedExCharge   * This shipment's totalNetFreight + totalSurcharges (not including
     * totalTaxes).
     */
    public void setTotalNetFedExCharge(com.fedex.ship.stub.Money totalNetFedExCharge) {
        this.totalNetFedExCharge = totalNetFedExCharge;
    }


    /**
     * Gets the totalTaxes value for this ShipmentLegRateDetail.
     * 
     * @return totalTaxes   * Total of the transportation-based taxes.
     */
    public com.fedex.ship.stub.Money getTotalTaxes() {
        return totalTaxes;
    }


    /**
     * Sets the totalTaxes value for this ShipmentLegRateDetail.
     * 
     * @param totalTaxes   * Total of the transportation-based taxes.
     */
    public void setTotalTaxes(com.fedex.ship.stub.Money totalTaxes) {
        this.totalTaxes = totalTaxes;
    }


    /**
     * Gets the totalNetCharge value for this ShipmentLegRateDetail.
     * 
     * @return totalNetCharge
     */
    public com.fedex.ship.stub.Money getTotalNetCharge() {
        return totalNetCharge;
    }


    /**
     * Sets the totalNetCharge value for this ShipmentLegRateDetail.
     * 
     * @param totalNetCharge
     */
    public void setTotalNetCharge(com.fedex.ship.stub.Money totalNetCharge) {
        this.totalNetCharge = totalNetCharge;
    }


    /**
     * Gets the totalRebates value for this ShipmentLegRateDetail.
     * 
     * @return totalRebates
     */
    public com.fedex.ship.stub.Money getTotalRebates() {
        return totalRebates;
    }


    /**
     * Sets the totalRebates value for this ShipmentLegRateDetail.
     * 
     * @param totalRebates
     */
    public void setTotalRebates(com.fedex.ship.stub.Money totalRebates) {
        this.totalRebates = totalRebates;
    }


    /**
     * Gets the totalDutiesAndTaxes value for this ShipmentLegRateDetail.
     * 
     * @return totalDutiesAndTaxes   * Total of all values under this shipment's dutiesAndTaxes; only
     * provided if estimated duties and taxes were calculated for this shipment.
     */
    public com.fedex.ship.stub.Money getTotalDutiesAndTaxes() {
        return totalDutiesAndTaxes;
    }


    /**
     * Sets the totalDutiesAndTaxes value for this ShipmentLegRateDetail.
     * 
     * @param totalDutiesAndTaxes   * Total of all values under this shipment's dutiesAndTaxes; only
     * provided if estimated duties and taxes were calculated for this shipment.
     */
    public void setTotalDutiesAndTaxes(com.fedex.ship.stub.Money totalDutiesAndTaxes) {
        this.totalDutiesAndTaxes = totalDutiesAndTaxes;
    }


    /**
     * Gets the totalNetChargeWithDutiesAndTaxes value for this ShipmentLegRateDetail.
     * 
     * @return totalNetChargeWithDutiesAndTaxes   * This shipment's totalNetCharge + totalDutiesAndTaxes; only
     * provided if estimated duties and taxes were calculated for this shipment
     * AND duties, taxes and transportation charges are all paid by the same
     * sender's account.
     */
    public com.fedex.ship.stub.Money getTotalNetChargeWithDutiesAndTaxes() {
        return totalNetChargeWithDutiesAndTaxes;
    }


    /**
     * Sets the totalNetChargeWithDutiesAndTaxes value for this ShipmentLegRateDetail.
     * 
     * @param totalNetChargeWithDutiesAndTaxes   * This shipment's totalNetCharge + totalDutiesAndTaxes; only
     * provided if estimated duties and taxes were calculated for this shipment
     * AND duties, taxes and transportation charges are all paid by the same
     * sender's account.
     */
    public void setTotalNetChargeWithDutiesAndTaxes(com.fedex.ship.stub.Money totalNetChargeWithDutiesAndTaxes) {
        this.totalNetChargeWithDutiesAndTaxes = totalNetChargeWithDutiesAndTaxes;
    }


    /**
     * Gets the freightRateDetail value for this ShipmentLegRateDetail.
     * 
     * @return freightRateDetail   * Rate data specific to FedEx Freight and FedEx National Freight
     * services.
     */
    public com.fedex.ship.stub.FreightRateDetail getFreightRateDetail() {
        return freightRateDetail;
    }


    /**
     * Sets the freightRateDetail value for this ShipmentLegRateDetail.
     * 
     * @param freightRateDetail   * Rate data specific to FedEx Freight and FedEx National Freight
     * services.
     */
    public void setFreightRateDetail(com.fedex.ship.stub.FreightRateDetail freightRateDetail) {
        this.freightRateDetail = freightRateDetail;
    }


    /**
     * Gets the freightDiscounts value for this ShipmentLegRateDetail.
     * 
     * @return freightDiscounts   * All rate discounts that apply to this shipment.
     */
    public com.fedex.ship.stub.RateDiscount[] getFreightDiscounts() {
        return freightDiscounts;
    }


    /**
     * Sets the freightDiscounts value for this ShipmentLegRateDetail.
     * 
     * @param freightDiscounts   * All rate discounts that apply to this shipment.
     */
    public void setFreightDiscounts(com.fedex.ship.stub.RateDiscount[] freightDiscounts) {
        this.freightDiscounts = freightDiscounts;
    }

    public com.fedex.ship.stub.RateDiscount getFreightDiscounts(int i) {
        return this.freightDiscounts[i];
    }

    public void setFreightDiscounts(int i, com.fedex.ship.stub.RateDiscount _value) {
        this.freightDiscounts[i] = _value;
    }


    /**
     * Gets the rebates value for this ShipmentLegRateDetail.
     * 
     * @return rebates   * All rebates that apply to this shipment.
     */
    public com.fedex.ship.stub.Rebate[] getRebates() {
        return rebates;
    }


    /**
     * Sets the rebates value for this ShipmentLegRateDetail.
     * 
     * @param rebates   * All rebates that apply to this shipment.
     */
    public void setRebates(com.fedex.ship.stub.Rebate[] rebates) {
        this.rebates = rebates;
    }

    public com.fedex.ship.stub.Rebate getRebates(int i) {
        return this.rebates[i];
    }

    public void setRebates(int i, com.fedex.ship.stub.Rebate _value) {
        this.rebates[i] = _value;
    }


    /**
     * Gets the surcharges value for this ShipmentLegRateDetail.
     * 
     * @return surcharges   * All surcharges that apply to this shipment.
     */
    public com.fedex.ship.stub.Surcharge[] getSurcharges() {
        return surcharges;
    }


    /**
     * Sets the surcharges value for this ShipmentLegRateDetail.
     * 
     * @param surcharges   * All surcharges that apply to this shipment.
     */
    public void setSurcharges(com.fedex.ship.stub.Surcharge[] surcharges) {
        this.surcharges = surcharges;
    }

    public com.fedex.ship.stub.Surcharge getSurcharges(int i) {
        return this.surcharges[i];
    }

    public void setSurcharges(int i, com.fedex.ship.stub.Surcharge _value) {
        this.surcharges[i] = _value;
    }


    /**
     * Gets the taxes value for this ShipmentLegRateDetail.
     * 
     * @return taxes   * All transportation-based taxes applicable to this shipment.
     */
    public com.fedex.ship.stub.Tax[] getTaxes() {
        return taxes;
    }


    /**
     * Sets the taxes value for this ShipmentLegRateDetail.
     * 
     * @param taxes   * All transportation-based taxes applicable to this shipment.
     */
    public void setTaxes(com.fedex.ship.stub.Tax[] taxes) {
        this.taxes = taxes;
    }

    public com.fedex.ship.stub.Tax getTaxes(int i) {
        return this.taxes[i];
    }

    public void setTaxes(int i, com.fedex.ship.stub.Tax _value) {
        this.taxes[i] = _value;
    }


    /**
     * Gets the dutiesAndTaxes value for this ShipmentLegRateDetail.
     * 
     * @return dutiesAndTaxes   * All commodity-based duties and taxes applicable to this shipment.
     */
    public com.fedex.ship.stub.EdtCommodityTax[] getDutiesAndTaxes() {
        return dutiesAndTaxes;
    }


    /**
     * Sets the dutiesAndTaxes value for this ShipmentLegRateDetail.
     * 
     * @param dutiesAndTaxes   * All commodity-based duties and taxes applicable to this shipment.
     */
    public void setDutiesAndTaxes(com.fedex.ship.stub.EdtCommodityTax[] dutiesAndTaxes) {
        this.dutiesAndTaxes = dutiesAndTaxes;
    }

    public com.fedex.ship.stub.EdtCommodityTax getDutiesAndTaxes(int i) {
        return this.dutiesAndTaxes[i];
    }

    public void setDutiesAndTaxes(int i, com.fedex.ship.stub.EdtCommodityTax _value) {
        this.dutiesAndTaxes[i] = _value;
    }


    /**
     * Gets the variableHandlingCharges value for this ShipmentLegRateDetail.
     * 
     * @return variableHandlingCharges   * The "order level" variable handling charges.
     */
    public com.fedex.ship.stub.VariableHandlingCharges getVariableHandlingCharges() {
        return variableHandlingCharges;
    }


    /**
     * Sets the variableHandlingCharges value for this ShipmentLegRateDetail.
     * 
     * @param variableHandlingCharges   * The "order level" variable handling charges.
     */
    public void setVariableHandlingCharges(com.fedex.ship.stub.VariableHandlingCharges variableHandlingCharges) {
        this.variableHandlingCharges = variableHandlingCharges;
    }


    /**
     * Gets the totalVariableHandlingCharges value for this ShipmentLegRateDetail.
     * 
     * @return totalVariableHandlingCharges   * The total of all variable handling charges at both shipment
     * (order) and package level.
     */
    public com.fedex.ship.stub.VariableHandlingCharges getTotalVariableHandlingCharges() {
        return totalVariableHandlingCharges;
    }


    /**
     * Sets the totalVariableHandlingCharges value for this ShipmentLegRateDetail.
     * 
     * @param totalVariableHandlingCharges   * The total of all variable handling charges at both shipment
     * (order) and package level.
     */
    public void setTotalVariableHandlingCharges(com.fedex.ship.stub.VariableHandlingCharges totalVariableHandlingCharges) {
        this.totalVariableHandlingCharges = totalVariableHandlingCharges;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof ShipmentLegRateDetail)) return false;
        ShipmentLegRateDetail other = (ShipmentLegRateDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.legDescription==null && other.getLegDescription()==null) || 
             (this.legDescription!=null &&
              this.legDescription.equals(other.getLegDescription()))) &&
            ((this.legOrigin==null && other.getLegOrigin()==null) || 
             (this.legOrigin!=null &&
              this.legOrigin.equals(other.getLegOrigin()))) &&
            ((this.legOriginLocationId==null && other.getLegOriginLocationId()==null) || 
             (this.legOriginLocationId!=null &&
              this.legOriginLocationId.equals(other.getLegOriginLocationId()))) &&
            ((this.legDestination==null && other.getLegDestination()==null) || 
             (this.legDestination!=null &&
              this.legDestination.equals(other.getLegDestination()))) &&
            ((this.legDestinationLocationId==null && other.getLegDestinationLocationId()==null) || 
             (this.legDestinationLocationId!=null &&
              this.legDestinationLocationId.equals(other.getLegDestinationLocationId()))) &&
            ((this.rateType==null && other.getRateType()==null) || 
             (this.rateType!=null &&
              this.rateType.equals(other.getRateType()))) &&
            ((this.rateScale==null && other.getRateScale()==null) || 
             (this.rateScale!=null &&
              this.rateScale.equals(other.getRateScale()))) &&
            ((this.rateZone==null && other.getRateZone()==null) || 
             (this.rateZone!=null &&
              this.rateZone.equals(other.getRateZone()))) &&
            ((this.pricingCode==null && other.getPricingCode()==null) || 
             (this.pricingCode!=null &&
              this.pricingCode.equals(other.getPricingCode()))) &&
            ((this.ratedWeightMethod==null && other.getRatedWeightMethod()==null) || 
             (this.ratedWeightMethod!=null &&
              this.ratedWeightMethod.equals(other.getRatedWeightMethod()))) &&
            ((this.minimumChargeType==null && other.getMinimumChargeType()==null) || 
             (this.minimumChargeType!=null &&
              this.minimumChargeType.equals(other.getMinimumChargeType()))) &&
            ((this.currencyExchangeRate==null && other.getCurrencyExchangeRate()==null) || 
             (this.currencyExchangeRate!=null &&
              this.currencyExchangeRate.equals(other.getCurrencyExchangeRate()))) &&
            ((this.specialRatingApplied==null && other.getSpecialRatingApplied()==null) || 
             (this.specialRatingApplied!=null &&
              java.util.Arrays.equals(this.specialRatingApplied, other.getSpecialRatingApplied()))) &&
            ((this.dimDivisor==null && other.getDimDivisor()==null) || 
             (this.dimDivisor!=null &&
              this.dimDivisor.equals(other.getDimDivisor()))) &&
            ((this.dimDivisorType==null && other.getDimDivisorType()==null) || 
             (this.dimDivisorType!=null &&
              this.dimDivisorType.equals(other.getDimDivisorType()))) &&
            ((this.fuelSurchargePercent==null && other.getFuelSurchargePercent()==null) || 
             (this.fuelSurchargePercent!=null &&
              this.fuelSurchargePercent.equals(other.getFuelSurchargePercent()))) &&
            ((this.totalBillingWeight==null && other.getTotalBillingWeight()==null) || 
             (this.totalBillingWeight!=null &&
              this.totalBillingWeight.equals(other.getTotalBillingWeight()))) &&
            ((this.totalDimWeight==null && other.getTotalDimWeight()==null) || 
             (this.totalDimWeight!=null &&
              this.totalDimWeight.equals(other.getTotalDimWeight()))) &&
            ((this.totalBaseCharge==null && other.getTotalBaseCharge()==null) || 
             (this.totalBaseCharge!=null &&
              this.totalBaseCharge.equals(other.getTotalBaseCharge()))) &&
            ((this.totalFreightDiscounts==null && other.getTotalFreightDiscounts()==null) || 
             (this.totalFreightDiscounts!=null &&
              this.totalFreightDiscounts.equals(other.getTotalFreightDiscounts()))) &&
            ((this.totalNetFreight==null && other.getTotalNetFreight()==null) || 
             (this.totalNetFreight!=null &&
              this.totalNetFreight.equals(other.getTotalNetFreight()))) &&
            ((this.totalSurcharges==null && other.getTotalSurcharges()==null) || 
             (this.totalSurcharges!=null &&
              this.totalSurcharges.equals(other.getTotalSurcharges()))) &&
            ((this.totalNetFedExCharge==null && other.getTotalNetFedExCharge()==null) || 
             (this.totalNetFedExCharge!=null &&
              this.totalNetFedExCharge.equals(other.getTotalNetFedExCharge()))) &&
            ((this.totalTaxes==null && other.getTotalTaxes()==null) || 
             (this.totalTaxes!=null &&
              this.totalTaxes.equals(other.getTotalTaxes()))) &&
            ((this.totalNetCharge==null && other.getTotalNetCharge()==null) || 
             (this.totalNetCharge!=null &&
              this.totalNetCharge.equals(other.getTotalNetCharge()))) &&
            ((this.totalRebates==null && other.getTotalRebates()==null) || 
             (this.totalRebates!=null &&
              this.totalRebates.equals(other.getTotalRebates()))) &&
            ((this.totalDutiesAndTaxes==null && other.getTotalDutiesAndTaxes()==null) || 
             (this.totalDutiesAndTaxes!=null &&
              this.totalDutiesAndTaxes.equals(other.getTotalDutiesAndTaxes()))) &&
            ((this.totalNetChargeWithDutiesAndTaxes==null && other.getTotalNetChargeWithDutiesAndTaxes()==null) || 
             (this.totalNetChargeWithDutiesAndTaxes!=null &&
              this.totalNetChargeWithDutiesAndTaxes.equals(other.getTotalNetChargeWithDutiesAndTaxes()))) &&
            ((this.freightRateDetail==null && other.getFreightRateDetail()==null) || 
             (this.freightRateDetail!=null &&
              this.freightRateDetail.equals(other.getFreightRateDetail()))) &&
            ((this.freightDiscounts==null && other.getFreightDiscounts()==null) || 
             (this.freightDiscounts!=null &&
              java.util.Arrays.equals(this.freightDiscounts, other.getFreightDiscounts()))) &&
            ((this.rebates==null && other.getRebates()==null) || 
             (this.rebates!=null &&
              java.util.Arrays.equals(this.rebates, other.getRebates()))) &&
            ((this.surcharges==null && other.getSurcharges()==null) || 
             (this.surcharges!=null &&
              java.util.Arrays.equals(this.surcharges, other.getSurcharges()))) &&
            ((this.taxes==null && other.getTaxes()==null) || 
             (this.taxes!=null &&
              java.util.Arrays.equals(this.taxes, other.getTaxes()))) &&
            ((this.dutiesAndTaxes==null && other.getDutiesAndTaxes()==null) || 
             (this.dutiesAndTaxes!=null &&
              java.util.Arrays.equals(this.dutiesAndTaxes, other.getDutiesAndTaxes()))) &&
            ((this.variableHandlingCharges==null && other.getVariableHandlingCharges()==null) || 
             (this.variableHandlingCharges!=null &&
              this.variableHandlingCharges.equals(other.getVariableHandlingCharges()))) &&
            ((this.totalVariableHandlingCharges==null && other.getTotalVariableHandlingCharges()==null) || 
             (this.totalVariableHandlingCharges!=null &&
              this.totalVariableHandlingCharges.equals(other.getTotalVariableHandlingCharges())));
        __equalsCalc = null;
        return _equals;
    }

    private boolean __hashCodeCalc = false;
    public synchronized int hashCode() {
        if (__hashCodeCalc) {
            return 0;
        }
        __hashCodeCalc = true;
        int _hashCode = 1;
        if (getLegDescription() != null) {
            _hashCode += getLegDescription().hashCode();
        }
        if (getLegOrigin() != null) {
            _hashCode += getLegOrigin().hashCode();
        }
        if (getLegOriginLocationId() != null) {
            _hashCode += getLegOriginLocationId().hashCode();
        }
        if (getLegDestination() != null) {
            _hashCode += getLegDestination().hashCode();
        }
        if (getLegDestinationLocationId() != null) {
            _hashCode += getLegDestinationLocationId().hashCode();
        }
        if (getRateType() != null) {
            _hashCode += getRateType().hashCode();
        }
        if (getRateScale() != null) {
            _hashCode += getRateScale().hashCode();
        }
        if (getRateZone() != null) {
            _hashCode += getRateZone().hashCode();
        }
        if (getPricingCode() != null) {
            _hashCode += getPricingCode().hashCode();
        }
        if (getRatedWeightMethod() != null) {
            _hashCode += getRatedWeightMethod().hashCode();
        }
        if (getMinimumChargeType() != null) {
            _hashCode += getMinimumChargeType().hashCode();
        }
        if (getCurrencyExchangeRate() != null) {
            _hashCode += getCurrencyExchangeRate().hashCode();
        }
        if (getSpecialRatingApplied() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getSpecialRatingApplied());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getSpecialRatingApplied(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getDimDivisor() != null) {
            _hashCode += getDimDivisor().hashCode();
        }
        if (getDimDivisorType() != null) {
            _hashCode += getDimDivisorType().hashCode();
        }
        if (getFuelSurchargePercent() != null) {
            _hashCode += getFuelSurchargePercent().hashCode();
        }
        if (getTotalBillingWeight() != null) {
            _hashCode += getTotalBillingWeight().hashCode();
        }
        if (getTotalDimWeight() != null) {
            _hashCode += getTotalDimWeight().hashCode();
        }
        if (getTotalBaseCharge() != null) {
            _hashCode += getTotalBaseCharge().hashCode();
        }
        if (getTotalFreightDiscounts() != null) {
            _hashCode += getTotalFreightDiscounts().hashCode();
        }
        if (getTotalNetFreight() != null) {
            _hashCode += getTotalNetFreight().hashCode();
        }
        if (getTotalSurcharges() != null) {
            _hashCode += getTotalSurcharges().hashCode();
        }
        if (getTotalNetFedExCharge() != null) {
            _hashCode += getTotalNetFedExCharge().hashCode();
        }
        if (getTotalTaxes() != null) {
            _hashCode += getTotalTaxes().hashCode();
        }
        if (getTotalNetCharge() != null) {
            _hashCode += getTotalNetCharge().hashCode();
        }
        if (getTotalRebates() != null) {
            _hashCode += getTotalRebates().hashCode();
        }
        if (getTotalDutiesAndTaxes() != null) {
            _hashCode += getTotalDutiesAndTaxes().hashCode();
        }
        if (getTotalNetChargeWithDutiesAndTaxes() != null) {
            _hashCode += getTotalNetChargeWithDutiesAndTaxes().hashCode();
        }
        if (getFreightRateDetail() != null) {
            _hashCode += getFreightRateDetail().hashCode();
        }
        if (getFreightDiscounts() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getFreightDiscounts());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getFreightDiscounts(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getRebates() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getRebates());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getRebates(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getSurcharges() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getSurcharges());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getSurcharges(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getTaxes() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getTaxes());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getTaxes(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getDutiesAndTaxes() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getDutiesAndTaxes());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getDutiesAndTaxes(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getVariableHandlingCharges() != null) {
            _hashCode += getVariableHandlingCharges().hashCode();
        }
        if (getTotalVariableHandlingCharges() != null) {
            _hashCode += getTotalVariableHandlingCharges().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(ShipmentLegRateDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShipmentLegRateDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("legDescription");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "LegDescription"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("legOrigin");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "LegOrigin"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Address"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("legOriginLocationId");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "LegOriginLocationId"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("legDestination");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "LegDestination"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Address"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("legDestinationLocationId");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "LegDestinationLocationId"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("rateType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RateType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ReturnedRateType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("rateScale");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RateScale"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("rateZone");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RateZone"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("pricingCode");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PricingCode"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PricingCodeType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("ratedWeightMethod");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RatedWeightMethod"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RatedWeightMethod"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("minimumChargeType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "MinimumChargeType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "MinimumChargeType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("currencyExchangeRate");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CurrencyExchangeRate"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CurrencyExchangeRate"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("specialRatingApplied");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SpecialRatingApplied"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SpecialRatingAppliedType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dimDivisor");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DimDivisor"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "nonNegativeInteger"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dimDivisorType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DimDivisorType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RateDimensionalDivisorType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("fuelSurchargePercent");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FuelSurchargePercent"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "decimal"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalBillingWeight");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalBillingWeight"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Weight"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalDimWeight");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalDimWeight"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Weight"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalBaseCharge");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalBaseCharge"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalFreightDiscounts");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalFreightDiscounts"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalNetFreight");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalNetFreight"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalSurcharges");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalSurcharges"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalNetFedExCharge");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalNetFedExCharge"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalTaxes");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalTaxes"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalNetCharge");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalNetCharge"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalRebates");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalRebates"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalDutiesAndTaxes");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalDutiesAndTaxes"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalNetChargeWithDutiesAndTaxes");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalNetChargeWithDutiesAndTaxes"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("freightRateDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightRateDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightRateDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("freightDiscounts");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightDiscounts"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RateDiscount"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("rebates");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Rebates"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Rebate"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("surcharges");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Surcharges"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Surcharge"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("taxes");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Taxes"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Tax"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dutiesAndTaxes");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DutiesAndTaxes"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EdtCommodityTax"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("variableHandlingCharges");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "VariableHandlingCharges"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "VariableHandlingCharges"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("totalVariableHandlingCharges");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TotalVariableHandlingCharges"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "VariableHandlingCharges"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
    }

    /**
     * Return type metadata object
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

    /**
     * Get Custom Serializer
     */
    public static org.apache.axis.encoding.Serializer getSerializer(
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanSerializer(
            _javaType, _xmlType, typeDesc);
    }

    /**
     * Get Custom Deserializer
     */
    public static org.apache.axis.encoding.Deserializer getDeserializer(
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanDeserializer(
            _javaType, _xmlType, typeDesc);
    }

}
