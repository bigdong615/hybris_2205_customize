<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="multi-checkout" tagdir="/WEB-INF/tags/responsive/checkout/multi"%>
<%@ taglib prefix="multi-checkout-paypal" tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/checkout/multi" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="custom-fields" tagdir="/WEB-INF/tags/addons/braintreeaddon/responsive/custom/fields" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>
<%@ taglib prefix="formElement"
	tagdir="/WEB-INF/tags/responsive/formElement"%>
<%@ taglib prefix="checkout"
	tagdir="/WEB-INF/tags/responsive/checkout/multi"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>	


<spring:url value="/checkout/multi/summary/braintree/placeOrder" var="placeOrderUrl"/>
<spring:url value="/checkout/multi/termsAndConditions" var="getTermsAndConditionsUrl"/>
<c:url value="/checkout/multi/payment-method/add" var="paymentAction" />

	


<template:page pageTitle="${pageTitle}" hideHeaderLinks="true">

<%-- <div class="row">
    <div class="col-sm-6">
        <div class="checkout-headline">
            <span class="glyphicon glyphicon-lock"></span>
            <spring:theme code="checkout.multi.secure.checkout" />
        </div>

		<multi-checkout:checkoutSteps checkoutSteps="${checkoutSteps}" progressBarId="${progressBarId}">
			<ycommerce:testId code="checkoutStepFour">
				<div class="checkout-review hidden-xs">
                    <div class="checkout-order-summary">
                        <multi-checkout:orderTotals cartData="${cartData}" showTaxEstimate="${showTaxEstimate}" showTax="${showTax}" subtotalsCssClasses="dark"/>
                    </div>
                </div>
                <div class="place-order-form hidden-xs">
                    <form:form action="${placeOrderUrl}" id="placeOrderForm1" modelAttribute="placeOrderForm">
                        <input type="hidden" id="shipsFromPostalCode" name="shipsFromPostalCode" value="${shipsFromPostalCode}">
                        <div class="checkbox">
                            <label> <form:checkbox id="Terms1" path="termsCheck" />
                                <spring:theme code="checkout.summary.placeOrder.readTermsAndConditions" arguments="${getTermsAndConditionsUrl}" text="Terms and Conditions"/>
                            </label>
                        </div>

                        <button id="placeOrder" type="submit" class="btn btn-primary btn-place-order btn-block">
                            <spring:theme code="checkout.summary.placeOrder" text="Place Order"/>
                        </button>
                    </form:form>
                </div>
			</ycommerce:testId>
		</multi-checkout:checkoutSteps>
    </div>

    <div class="col-sm-6">
		<multi-checkout-paypal:checkoutOrderSummary cartData="${cartData}" showDeliveryAddress="true" showPaymentInfo="true" showTaxEstimate="true" showTax="true" />
	</div>

    <div class="col-sm-12 col-lg-12">
        <br class="hidden-lg">
        <cms:pageSlot position="SideContent" var="feature" element="div" class="checkout-help">
            <cms:component component="${feature}"/>
        </cms:pageSlot>
    </div>
</div> --%>


<section id="cartProcess">
        <div class="container">
            <div id="cartSteps" class="row justify-content-center">
                <div class="col-xl-10">
                    <span class="step1 complete"><i class="icon-check"></i> Your Rental</span><span class="step2 complete"><i class="icon-check"></i> Delivery or Pickup</span><span class="step3 complete"><i class="icon-check"></i> Payment</span><span class="step4 active"><i class="number">4</i> Review</span>
                </div>
            </div>
            <div class="row justify-content-center">
                <div class="col-xl-10">
                    <div class="row">
                        <div id="order" class="col-lg-7">
                            <h1>Review Your Rental</h1>
                            <hr>
                            
                            <div class="reviewCart">
                                <h5 class="mb-4">Rental Dates <a href="#" class="edit-cart lightteal float-end" data-bs-toggle="modal" data-bs-target="#editWarning">Edit</a></h5>
                                <div class="row">
                                    <div class="col-4">
                                        <p class="overline">Starts</p>
                                        <p class="lightteal mb-0"><b>Wednesday, Jan 31</b></p>
                                        <p class="body14">UPS delivers to you</p>
                                    </div>
                                    <div class="col-2 text-center">
                                        <img class="rental-arrow" src="assets/icon-arrow.svg">
                                    </div>
                                    <div class="col-4">
                                        <p class="overline">Ends</p>
                                        <p class="lightteal mb-0"><b>Friday, Feb 2</b></p>
                                        <p class="body14">You drop at UPS</p>
                                    </div>
                                </div>
                            </div>
                            <div class="reviewCart">
                                <h5 class="mb-4">Your Rental <a href="#" class="edit-cart lightteal float-end" data-bs-toggle="modal" data-bs-target="#editWarning">Edit</a></h5>
                                <div class="row mb-4">
                                    <div class="col-md-3 text-center"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></div>
                                    <div class="col-md-9 mt-3">
                                        <p class="gray80 body14">
                                            <b class="gray100">Canon EOS R5 Mirrorless Digital Camera</b>
                                            Qty 1<br>
                                            + Gear Guard Pro Damage Waiver<br>
                                            Total $86.23
                                        </p>    
                                    </div>
                                </div>
                                <div class="row mb-4">
                                    <div class="col-md-3 text-center"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD5796f428-6173-4219-8a48-683c1afd2b05.jpg"></div>
                                    <div class="col-md-9 mt-3">
                                        <p class="gray80 body14">
                                            <b class="gray100">Canon RF 15-35mm  F2.8 L IS USM</b>
                                            Qty 1<br>
                                            + No Damage Waiver<br>
                                            Total $86.23
                                        </p>    
                                    </div>
                                </div>
                                <div class="row mb-4">
                                    <div class="col-md-3 text-center"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD40fd25f3-6455-43bb-977a-794e3930e6aa.jpg"></div>
                                    <div class="col-md-9 mt-3">
                                        <p class="gray80 body14">
                                            <b class="gray100">DJI Osmo Action 4K Camera</b>
                                            Qty 1<br>
                                            + Gear Guard Pro Damage Waiver<br>
                                            + Curved Sticky Mount<br>
                                            Total $86.23
                                        </p>    
                                    </div>
                                </div>
                            </div>
                            <div class="reviewCart">
                                <h5 class="mb-4">Delivery <a href="#" class="edit-cart lightteal float-end" data-bs-toggle="modal" data-bs-target="#editWarning">Edit</a></h5>
                                <div class="row mb-4">
                                    <div class="col-6">
                                        <p class="gray80 body14">
                                            <b class="gray100">Delivery Method</b>
                                            UPS Standard AM Round Trip
                                        </p>
                                    </div>
                                    <div class="col-6">
                                        <p class="gray80 body14">
                                            <b class="gray100">Sipping To</b>
                                            John Doe<br>
                                            123 Broadway<br>
                                            Suite #213<br>
                                            New York, NY 10012<br>
                                            555-456-7894
                                        </p>
                                    </div>
                                </div>
                                <div class="notification notification-tip truck">Returns are easy! <a href="#">Learn more</a> about how to give your gear back.</div>
                            </div>
                            <div class="reviewCart">
                                <h5 class="mb-4">Payment <a href="#" class="edit-cart lightteal float-end" data-bs-toggle="modal" data-bs-target="#editWarning">Edit</a></h5>
                                
                                <multi-checkout-paypal:paymentInfo cartData="${cartData}" paymentInfo="${cartData.paymentInfo}" brainTreePaymentInfo="${brainTreePaymentInfoData}" />
                            </div>
                                
                            <div class="reviewCart">
                                <h5 class="mb-4">Rental Terms Agreement</h5>
                                <div class="row mb-4">
                                    
                                    <textarea class="form-control" rows="4">THIS EQUIPMENT LEASE (&#034;Lease&#034;) is made and effective by clicking on the &#034;Place Your Order&#034; button, by and between BorrowLenses.com, (&#034;Lessor&#034;), and current user (&#034;Lessee&#034;).  By clicking on the &#034;Place Your Order&#034; button, Lessee agrees to be bound by these terms and conditions, whether or not Lessee has read them.  Lessor may at its sole discretion modify these terms and conditions at any time and any modifications shall become effective immediately as posted on this site.  By clicking on the &#034;Place Your Order&#034; button, Lessee indicates acceptance of the modified terms and conditions. 

                                    NOW, THEREFORE, in consideration of the mutual covenants and promises hereinafter set forth, the parties hereto agree as follows: 

                                    1.	Lease.  Lessor hereby leases to Lessee, and Lessee hereby leases from Lessor, the equipment shown in the order preview above (Ã¢ÂÂEquipmentÃ¢ÂÂ).  Lessor reserves the right to refuse or decline leasing Equipment to potential lessee at LessorÃ¢ÂÂs sole discretion.  Lessor may, at its sole discretion, gather information from third parties regarding potential lesseeÃ¢ÂÂs past rental history and credit worthiness.

                                    2.	Term.  The term of this Lease shall commence on the day of the first attempt by the parcel carrier to deliver the item, and expire on the last day of the rental period as show on the checkout page (Ã¢ÂÂRental PeriodÃ¢ÂÂ), not to exceed ninety (90) days.  The Equipment must be return shipped to BorrowLenses.com before shipping cutÃ¢ÂÂoff time with the courier on the last day of the Rental Period.  Should Lessee return the Equipment using a shipping label not provided by Lessor, and the Equipment is not returned on or before the expected return date, Lessor reserves the right to charge the Lessee additional Rent for the extra time the Equipment was in transit or late.  You hereby give Lessor permission to contact you via the US mail, email, telephone, or text message regarding information on or status of your rental. 

                                    3.	Renewal. Lessee may renew this Lease for additional Rental Periods, not to exceed ninety (90) days each, with the consent of Lessor. Renewal of this Lease will be linked to LesseeÃ¢ÂÂs original Order, but will be invoiced separately. All terms of this Lease apply with the same force during the renewal period.

                                    4.	Rent and Deposit.  All rent will be paid in advance, in full before the order ships.

                                    5.	Use.  Lessee shall use the Equipment in a careful, safe and appropriate manner and shall comply with and conform to all national, state, municipal, and other laws, and regulations in any way relating to the possession, use or maintenance of the Equipment including any manufacturerÃ¢ÂÂs recommendations, warnings and instructions as to the safe use of the Equipment. 

                                    6.	Representations, Warranties and Agreements.  Lessee has selected the Equipment without relying upon any suggestion or recommendations of Lessor or its employees and Lessee understands and agrees that Lessor assumes no responsibility for the Equipment as being fit for any particular purpose.    
                                    Lessor represents and warrants as follows: (1) the Equipment is free from known defects and is in good working order to the best of its knowledge at the inception of the rental; (2) Lessor is responsible for routine repair and maintenance of the Equipment prior to rental; (3) Lessor has the right to enter into the rental of the Equipment. 

                                    Lessee agrees as follows: (a) except as set forth in LessorÃ¢ÂÂs representations and warranties above, the Equipment is rented to Lessee without any warranty or guaranty of any kind, express or implied, and specifically, there is no warranty of merchantability or fitness for a particular purpose; (b) Lessor shall not be held responsible with respect to production downtime, loss of profits, extra expense, indirect, consequential, or punitive damages, production delays; and (c) except as set forth in LessorÃ¢ÂÂs representations and warranties above, Lessee is responsible for all costs associated with any repair or replacement (without deduction for depreciation) of the Equipment necessitated as a result of LesseeÃ¢ÂÂs usage, possession, transportation or failure to return the Equipment for any reason, including, without limitation, as a result of the negligence or willful misconduct of Lessee, its employees, agents, or contractors.  At all times LessorÃ¢ÂÂs maximum liability in connection with the Equipment is limited to the rent paid to Lessor by Lessee. 

                                    7.	Used Equipment.  Lessee acknowledges that the Equipment may be used gear and may be cosmetically flawed.  However, Lessor warrants that the Equipment, whether new or used, will be in proper working condition when leased to Lessee.  Should Lessee discover that the Equipment, as received, is not working properly, Lessee agrees to notify Lessor pursuant to Section 15 of this Agreement.   

                                    8.	Typographical Errors.  In the event a product rental rate is listed incorrectly due to errors in pricing information received from LessorÃ¢ÂÂs suppliers, Lessor has the right to refuse or cancel any orders placed for products listed at the incorrect rate whether or not LesseeÃ¢ÂÂs credit card has been charged.   Should Lessor cancel LesseeÃ¢ÂÂs order, Lessor will immediately credit LesseeÃ¢ÂÂs account for the incorrect amount paid. 

                                    9.	Order Acceptance Policy.  Your receipt of an electronic or other form of order confirmation does not signify LessorÃ¢ÂÂs acceptance of LesseeÃ¢ÂÂs order, nor does it constitute confirmation of LessorÃ¢ÂÂs offer to rent.  BorrowLenses.com reserves the right at any time after receipt of LesseeÃ¢ÂÂs order to accept or decline LesseeÃ¢ÂÂs order or to supply less than the quantity Lessee ordered of any item for any reason. 

                                    10.	OutÃ¢ÂÂofÃ¢ÂÂStock Products and Multiple Product Orders.  Lessor will ship the Equipment to Lessee as it becomes available.  There may be times when the Equipment Lessee ordered is outÃ¢ÂÂofÃ¢ÂÂstock which will delay fulfilling LesseeÃ¢ÂÂs order.  Lessor makes no guarantees as to availability of Equipment.  Any estimate of availability provided by Lessor is based on the assumption that each of LessorÃ¢ÂÂs customer returns Equipment within the prescribed Rental Period.  Lessor will keep Lessee informed of any Equipment that Lessee has ordered that are outÃ¢ÂÂofÃ¢ÂÂstock and unavailable for immediate shipment.  If Equipment is outÃ¢ÂÂ ofÃ¢ÂÂstock or unavailable, Lessee may cancel the order at any time prior to shipping.  For a multiple product order, Lessor will make every attempt to ship all products contained in the order at the same time.  Equipment that are unavailable at the time of shipment of other Equipment will be shipped as they become available unless Lessee notifies Lessor of their alternate wishes to this end.  Lessee will only be charged for Equipment contained in a given shipment, plus any applicable shipping charges.   Lessee will only be charged for shipping at the rate quoted on LesseeÃ¢ÂÂs purchase receipt.  The entirety of this shipping charge may be applied to the first Equipment shipped on an order requiring multiple shipments. 

                                    11.	Shipping.  All shipping choices (including returns) are the sole responsibility of the Lessee.  Lessor does not ship on federal holidays.  Lessor cannot guarantee the arrival date of the order as that is outside the control of Lessor.  Any shipping or transit time estimates provided by Lessor are estimates only.  Lessee is encouraged to order in a timely fashion to avoid delays caused by product unavailability or shipping.  Lessee agrees to obtain and retain the shipping receipt for all return shipping until Lessor notifies Lessee of LessorÃ¢ÂÂs receipt of such returned product. 

                                    12.	Cancellations.  Lessee may cancel a pending reservation prior to order shipment or pick up. Failure to do so will result in a 1 day rental (prorated off the 3 day rate) for all Equipment charged to your card. Lessee may cancel within 24 hours of placing an order with no fees or penalties, as long as order has not shipped. Canceling a shipped order will result in extra fees.  Lessee may cancel an order once it has shipped but such cancellation will be subject to full refund minus: 1 day rental rate + round trip shipping + cost of the damage waiver (if applicable).  Lessor reserves the right to cancel any order at any time before delivery occurs at its sole discretion. Lessor may recall the equipment in an event of default with LessorÃ¢ÂÂs lender or under a lease to which Lessor is party as lessee with five (5) business daysÃ¢ÂÂ notice. In the event of a recall before the end of the Rental Period, Lessor will provide a prorated refund corresponding to the amount remaining in the Rental Period. 

                                    13.	Insurance and Deposit.  Some orders may require that Lessee insure the Equipment for the duration of the Rental Period or provide an authorization hold to be placed on LesseeÃ¢ÂÂs credit card on file in an amount to be determined by Lessor until the Equipment has been returned to Lessor in good working order.  Should Lessee chose to insure the Equipment, Lessee shall add Shutterfly, LLC as Additional Insureds on LesseeÃ¢ÂÂs insurance policy and provide Lessor with a Certificate of Insurance at the time of the order. 

                                    14.	Age.  Due to the value of the Equipment Lessor will not rent Equipment to persons under the age of 18.  Lessee agrees not to allow any person to pick up, or return the Equipment who is under the age of 18. 

                                    15.	Repairs.  Lessee agrees not to attempt to repair or materially alter the physical or otherwise makeup of the Equipment under any circumstances regardless of fault.    

                                    16.	Loss and Damage.  Except as set forth in LessorÃ¢ÂÂs representations and warranties above, Lessee hereby assumes and shall bear the entire risk of loss and damage to the Equipment from any and every cause whatsoever. No loss or damage to the Equipment or any part thereof shall impair any obligation of Lessee under this Lease.  In the event of damage of any kind whatever to the Equipment, Lessor may: (a) charge LesseeÃ¢ÂÂs credit card for the full cost of repair; and (b) repair the Equipment using a vendor at LessorÃ¢ÂÂs sole discretion.  In addition to repair or replacement fees, Lessor may charge LesseeÃ¢ÂÂs credit card for a Ã¢ÂÂLoss of Use FeeÃ¢ÂÂ in the event Lessee did not purchase a damage waiver at the time of the initial rental order.  Ã¢ÂÂLoss of Use FeeÃ¢ÂÂ is equal to the daily rental fees Lessor would have been entitled to receive for the Equipment had it not been damaged or lost.  In the event the Equipment becomes damaged beyond repair, Lessee shall pay Lessor the full replacement value of the Equipment.  Lessee must ensure that the Equipment, when returned to Lessor, is clean.  Should the returned Equipment be deemed dirty in LessorÃ¢ÂÂs sole judgment, Lessor reserves the right to charge Lessee a Ã¢ÂÂCleaning FeeÃ¢ÂÂ. 

                                    17.	NonÃ¢ÂÂWorking Equipment.  Lessee shall notify Lessor within 3 hours of receipt of Equipment any malfunction and/or alleged damage of such Equipment. In the event Equipment is not functioning and/or damaged other than as a result of LesseeÃ¢ÂÂs negligence or willful acts, Lessee must not attempt to repair or modify the equipment himself/herself.  Lessee must return such nonÃ¢ÂÂworking Equipment to Lessor and Lessor will either replace the nonÃ¢ÂÂworking Equipment with a functioning equivalent 
                                    (Ã¢ÂÂReplacement EquipmentÃ¢ÂÂ) or issue Lessee a credit or full refund of all rental charges paid by Lessee at LesseeÃ¢ÂÂs option. The rental charges for all such nonÃ¢ÂÂworking Equipment so returned will commence upon LesseeÃ¢ÂÂs receipt of the replacement Equipment.  Once Lessor receives notification of suspected damage of Equipment in transit, Lessor shall send such damaged Equipment to the manufacturer for inspection and repair.  Lessee and Lessor hereby agree to be bound by the damage report provided by such manufacturer as to the cause and liability of such damage. 

                                    18.	Surrender.  Upon the expiration or earlier termination of this Lease, Lessee shall return the Equipment to Lessor in good repair, condition and working order, ordinary wear and tear resulting from proper use thereof excepted, by delivering the Equipment at Lessee&#039;s cost and expense via the shipping method specified by Lessor.  Lessee shall be responsible for proper packaging of the returned Equipment using shipping and packaging materials provided by Lessor in the order shipment.  LessorÃ¢ÂÂs acceptance of the Equipment upon return by Lessee shall not represent LessorÃ¢ÂÂs determination as to the condition of the returned Equipment.  Lessor reserves the right to inspect the Equipment within a reasonable time after the return of the Equipment and make a determination as to whether such returned Equipment was damaged during the Rental Period.  

                                    19.	Early Surrender.  Policy update: Ã¢ÂÂIf Lessee surrenders the Equipment with at least 1 day remaining in the Rental Period, Lessor will issue Lessee an Ã¢ÂÂEarly Return CreditÃ¢ÂÂ or Ã¢ÂÂEarly Return RefundÃ¢ÂÂ for the remaining unused time.  Early Return Credit is a discount code that may be applied towards future rentals and Early Return Refund will be issued to the credit card on file.   Early Return Refunds will be issued minus a 25% processing fee (calculated from the remaining dayÃ¢ÂÂs total). No Early Return Credit or Early Return Refund will be issued for return of Equipment with an original Rental Period under 4 days.  To receive an Ã¢ÂÂEarly Return CreditÃ¢ÂÂ or Ã¢ÂÂEarly Return RefundÃ¢ÂÂ, Lessee must request such credit at the time of early surrender via email at help@borrowlenses.com.Ã¢ÂÂ 

                                    20.	Damage Waiver.  Lessor offers Lessee the option to purchase a damage waiver (Ã¢ÂÂGearGuard 
                                    Damage WaiverÃ¢ÂÂ) for unintentional damage to the Equipment during the Rental Period or (Ã¢ÂÂGearGuard 
                                    Pro Full Value WaiverÃ¢ÂÂ) for theft, loss, or unintentional damage, including liquid damage, to the Equipment during the Rental Period. The determination of whether damage is unintentional and not abuse is at the sole discretion of Lessor.  

                                    Ã¢ÂÂ¢	In the event of damage covered by a GearGuard Damage Waiver, the Lessee, who has purchased the GearGuard Damage Waiver will pay Lessor a deductible amounting to 12% of the value of an item in similar condition to the Equipment rented to Lessee. Valuation of the Equipment is within the sole discretion of Lessor. A GearGuard Damage Waiver does not cover lost or stolen items. Equipment reported as damaged must be returned to Lessor for inspection. A GearGuard Damage Waiver does not cover liquid damage as that is considered to fall under the category of Lessee negligence.  

                                    Ã¢ÂÂ¢	In the event of damage or loss covered by a GearGuard Pro Full Value Waiver, the Lessee, who has purchased the GearGuard Pro Full Value Waiver will pay Lessor a deductible amounting to 12% of the value of an item in similar condition to the Equipment rented to Lessee. Valuation of the Equipment is within the sole discretion of Lessor. Equipment reported as damaged must be returned to Lessor for inspection. Equipment theft must be reported to the Lessor by the return date, or Lessee will be subject to late fees. Theft of equipment must be validated with an official police report submitted to Lessor within 30 days of the incident. Lessor may ask for additional documentation, up to an including a full investigation by Lessor or a third party acting on behalf of Lessor.  If the investigation reveals that the equipment can be returned, whatever the condition, then the Lessee must return or pay the full replacement value of the equipment. 

                                    Also, any peripheral items in Lessee&#039;s rental are not covered including lens hoods, battery chargers, front and rear caps, cables, etc. (Ã¢ÂÂPeripheral ItemsÃ¢ÂÂ).  If Lessee loses and/or damages Peripheral Items, Lessee is responsible for the cost of replacement of the Peripheral Items even if Lessee purchased a GearGuard Damage Waiver or GearGuard Pro Full Value Waiver.  

                                    21.	Taxes.  Lessee shall keep the Equipment free and clear of all levies, liens and encumbrances. Lessee, or Lessor at Lessee&#039;s expense, shall report, pay and discharge when due all license and registration fees, assessments, sales, use and property taxes, gross receipts, taxes arising out of receipts from use or operation of the Equipment, together with any penalties or interest thereon, imposed by any state, federal or local government or any agency, or department thereof, whether or not the same shall be assessed against or in the name of Lessor or Lessee.  Lessee shall indemnify and hold Lessor harmless from any taxes, fees, and penalties arising out of LesseeÃ¢ÂÂs lease, use and possession of the Equipment except for those taxes, fees and penalties based upon LessorÃ¢ÂÂs income.    

                                    22.	Indemnity.  Lessee shall indemnify Lessor against, and hold Lessor harmless from, any and all claims, actions, suits, proceedings, costs, expenses, damages and liabilities, including reasonable attorney&#039;s fees and costs, arising out of, connected with, or resulting from Lessee&#039;s use of the Equipment, including without limitation the selection, possession, use, operation, or return of the Equipment. 

                                    23.	Default.  If Lessee fails to pay any rent or other amount herein provided within ten (10) days after the same is due and payable, or if Lessee fails to observe, keep or perform any other provision of this Lease required to be observed, kept or performed by Lessee, Lessor shall have the right to exercise any one or more of the following remedies: 

                                    a.	To charge LesseeÃ¢ÂÂs credit card on file for all amounts due (including any late fees) and owing. 
                                    b.	To sue for and recover all rents, and other payments, then accrued or thereafter accruing. 
                                    c.	To take possession of the Equipment, without demand or notice, wherever same may be located, without any court order or other process of law. Lessee hereby waives any and all damages occasioned by such taking of possession. 
                                    d.	To terminate this Lease. 
                                    e.	To pursue any other remedy at law or in equity. 
                                    Notwithstanding any repossession or any other action which Lessor may take, Lessee shall be and remain liable for the full performance of all obligations on the part of the Lessee to be performed under this Lease. All of Lessor&#039;s remedies are cumulative, and may be exercised concurrently or separately. 

                                    24.	Bankruptcy.  Neither this Lease nor any interest therein is assignable or transferable by operation of law. If any proceeding under the Bankruptcy Act, as amended, is commenced by or against the Lessee, or if the Lessee is adjudged insolvent, or if Lessee makes any assignment for the benefit of his creditors, or if a writ of attachment or execution is levied on the Equipment and is not released or satisfied within ten (10) days thereafter, or if a receiver is appointed in any proceeding or action to which the Lessee is a party with authority to take possession or control of the Equipment, Lessor shall have and may exercise the option to, without notice, immediately terminate the Lease. The Lease shall not be treated as an asset of Lessee after the exercise of said option. 

                                    25.	Ownership.  The Equipment is, and shall at all times be and remain, the sole and exclusive property of Lessor; and the Lessee shall have no right, title or interest therein or thereto except as expressly set forth in this Lease. 

                                    26.	Additional Documents.  If Lessor shall so request, Lessee shall execute and deliver to Lessor such documents as Lessor shall deem necessary or desirable for purposes of recording or filing to protect the interest of Lessor in the Equipment including, but not limited to a UCC financing statement. 

                                    27.	Entire Agreement. This instrument constitutes the entire agreement between the parties on the subject matter hereof. 

                                    28.	Notices.  Service of all notices under this Agreement shall be sufficient if given personally or by certified mail, return receipt requested, postage prepaid, at the address hereinafter set forth, or to such address as such party may provide in writing from time to time. 

                                    29.	Governing Law.  This Lease shall be construed and enforced according to laws of the State of California. 

                                    Underwater Housing Equipment Addendum 

                                    All AquaTech Sport Housing Rental units undergo a thorough quality control and testing protocol. Due to the environments that our sport housing products are subjected to, the care and maintenance required, and the margin for user error, AquaTech and our affiliates stress that there is risk involved with taking your camera equipment into such conditions. 

                                    We strongly recommend that all care manuals be reviewed, understood and that all customers perform a preliminary water and control testing with and without their camera equipment before using their rented sport housing. We also strongly recommend you enter the water slowly, dipping your housing in and out of the water as you go. Constantly inspect the housing for signs of leakage and ensure that the installation protocol has been followed. If you notice any fogging or water drops inside the housing, we recommend that you leave the water immediately, remove your camera, dry the outside and inside of the housing and start again with the installation protocol. If this condition persists contact AquaTech immediately at (714) 968Ã¢ÂÂ6946. 

                                    AquaTech Sport Housings are designed for shallow water and surface use only. They have been tested and rated to 10 meters (33 feet). We do not recommend use in diving applications or depths greater than 10 meters (33 feet). 

                                    AquaTech or its dealers, including BorrowLenses.com, will assume no responsibility for any loss or damage to any camera equipment used with, around or in the housing product under any circumstances. Furthermore, AquaTech and our affiliates assume no responsibility and will not be held liable for any loss of income or any costs incurred by users from our housing products for any reason. 

                                    Agree To Purchasing Terms   

                                    CONDITION OF EQUIPMENT: All gear is individually reviewed and graded according to our used gear quality ratings. Our used gear is in good working order, in some cases with cosmetic flaws unless otherwise stated. We would never sell you equipment that is not in full working order. Each item is well tested and cleaned, upon return from previous rentals, to ensure you are receiving a quality used product from our inventory. We are unable to provide photos of the exact piece of gear you may be purchasing. We are also not able to list estimated or exact actuation counts for camera bodies.   

                                    Purchases Include: 
                                    Lenses include: Front &amp; rear caps, and a hood if it originally had one and still manufactured. 
                                    Cameras include: Body, front cap, strap, one battery and a charger. 
                                    Flashes include: Just the unit itself.   
                                    Video Equipment includes: Cameras, audio, and lighting all come with the accessories listed on the product page. 

                                    Purchases Exclude: 
                                    Lenses:  filters, pouch, boxes or a manual. 
                                    Cameras:  cables, boxes or a manual.   
                                    Flashes: pouches, batteries, filters, feet, 5th battery compartments, diffusers, boxes, or manuals.   
                                    Video equipment (cameras, audio, lighting): hard carrying cases. 
                                    Used Gear Quality Rating System

                                    Ratings are on a condition of 1-10 scale reflective of gear that has been used through normal wear and tear from past customers.

                                    WARRANTY:    
                                    We do not provide any warranty on our sale items. Please see our return policy (above) and condition specifications before placing your order. 

                                    RETURNS: Review your purchase with confidence from 3Ã¢ÂÂdays to 4Ã¢ÂÂweeks. We allow a 3Ã¢ÂÂday inspection period on all sale items from the date of delivery. If, for any reason, you want to return the item within those 3 days, simply return it using a 2Ã¢ÂÂday, trackable shipping method for a refund (minus the roundtrip shipping cost and a 3% restocking fee). The buyer is responsible for a 3% restocking fee and the roundtrip shipping cost of the equipment. If you are returning an item outside the 3Ã¢ÂÂday inspection window, we will issue a refund for the purchase price less the appropriate standard rental fees for the time you had the product in your possession and less the associated roundtrip shipping cost. No warranty is provided after the 4Ã¢ÂÂweek inspection period.    
                                    </textarea>

                                    <div class="notification notification-warning">Heads up: Placing your order holds your gear, but we wonÃ¢ÂÂt charge your card until itÃ¢ÂÂs time to ship your rental.</div>
                                    
                                </div>
                            </div>                            
                            
                            <div class="rental-terms">            
                                <b>By clicking Place Your Order, you agree to our Rental Terms</b>
                            </div>
                            <div class="cart-actions">
                                <form:form action="${placeOrderUrl}" id="placeOrderForm1" modelAttribute="placeOrderForm">
                                        <input type="hidden" id="shipsFromPostalCode" name="shipsFromPostalCode" value="${shipsFromPostalCode}">
                               <button id="placeOrder" type="submit" class="btn btn-sm btn-primary float-end">
                                   <spring:theme code="checkout.summary.placeOrder" text="Place Your Order"/>
                               </button>
                              </form:form>
                            </div>  

                        </div>
                        <div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top">
                           <cart:orderSummeryForReview cartData="${cartData}"
								emptyCart="${emptyCart}" />
										 <c:if test="${cartData.hasFreeShippingPromo}">
                        <div class="notification notification-tip truck"><spring:theme code="text.free.shipping.promo.applied.message"/></div>
                     </c:if>
                     <div class="notification notification-tip check"><spring:theme code="text.shipping.change.or.cancellation.message"/></div>

                            <div class="order-actions my-4"><a href="#" alt="Print Order"><i class="icon-print"></i></a></div>
                        </div>
                    </div>
                </div>
            </div>
        </div>    
    </section>
    
    <div class="modal fade" id="editWarning" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-sm">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title"><spring:theme code="checkout.summary.wait"/>!</h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
          <div class="modal-body"> 
              <p class="body14"><spring:theme code="checkout.summary.payment.message"/></p>
              <a href="${paymentAction}" class="btn btn-primary btn-block my-4"><spring:theme code="checkout.summary.payment.popup.continue"/></a>
              <p class="text-center mb-0"><a href="#" class="lightteal" data-bs-dismiss="modal" aria-label="Close"><spring:theme code="checkout.summary.payment.popup.Cancel"/></a></p>
          </div>
        </div>
      </div>
    </div>

	<div class="modal fade" id="editWarning" tabindex="-1"
		aria-hidden="true">
		<div class="modal-dialog modal-dialog-centered modal-sm">
			<div class="modal-content">
				<div class="modal-header">
					<h5 class="modal-title">
						<spring:theme
							code="shipping.interception.change.date.warning.wait" />
					</h5>
					<button type="button" class="btn-close" aria-label="Close"
						id="shippingCloseIconModal"></button>
				</div>
				<div class="modal-body">
					<input type="hidden" value="" id="rentalStartDate"> <input
						type="hidden" value="" id="rentalEndDate">
					<p class="body14">
						<spring:theme
							code="shipping.interception.change.date.warning.message" />
					</p>
					<a href="#" class="btn btn-primary btn-block my-4"
						id="shippingChangeRentalDate"><spring:theme
							code="shipping.interception.change.date.warning.continue" /></a>
					<p class="text-center mb-0">
						<a href="#" class="lightteal" aria-label="Close"
							id="shippingCloseModal"><spring:theme
								code="shipping.interception.change.date.warning.cancel" /></a>
					</p>
				</div>
			</div>
		</div>
	</div>
</template:page>
