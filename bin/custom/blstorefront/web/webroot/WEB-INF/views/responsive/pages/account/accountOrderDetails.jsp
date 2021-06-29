<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="common" tagdir="/WEB-INF/tags/responsive/common" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>



<spring:htmlEscape defaultHtmlEscape="true" />

 <div class="col-xl-10">
                    <div class="row">
                    ${orderData}
                        <div id="accountContent" class="col-lg-7">
                        <h1><spring:theme code="text.account.order.title.details"/></h1>
                            <hr>
                            <div class="reviewCart">
                                <h5 class="mb-4"><spring:theme code="text.myaccount.order.rental.dates"/></h5>
                                <div class="row">
                                    <div class="col-4">
                                        <p class="overline"><spring:theme code="text.myaccount.order.rental.starts"/></p>
                                        <p class="lightteal mb-0"><b>${orderData.rentalStartDate}</b></p>
                                        <p class="body14">UPS delivers to you</p>
                                    </div>
                                    <div class="col-2 text-center">
                                        <img class="rental-arrow" src="${themeResourcePath}/assets/icon-arrow.svg">
                                    </div>
                                    <div class="col-4">
                                        <p class="overline">Ends</p>
                                        <p class="lightteal mb-0"><b>${orderData.rentalEndDate}</b></p>
                                        <p class="body14">You drop at UPS</p>
                                    </div>
                                </div>
                            </div>
                            <div class="reviewCart">
                                <div class="row">
                                    <div class="col-6">
                                        <p class="body14">
                                        <spring:theme code="text.account.orderHistory.orderStatus"/> <br>
                                        <spring:theme code="text.account.orderHistory.datePlaced"/> <br>
                                        <spring:theme code="text.myaccount.order"/><br>
                                        <spring:theme code="text.myaccount.order.tracking"/>
                                        </p>
                                    </div>
                                    <div class="col-6">
                                        <p class="gray80 body14">
                                            Pending<br>
                                            Jan 28, 2021 11:16 AM<br>
                                            12345678<br>
                                            N/A
                                        </p>
                                    </div>
                                </div>
                            </div>
                            <div class="reviewCart">
                                <h5 class="mb-4">Your Rental </h5>
                                <div class="row mb-4">
                                    <div class="col-3 text-center"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></div>
                                    <div class="col-9 mt-3">
                                        <p class="gray80 body14">
                                            <b class="gray100">Canon EOS R5 Mirrorless Digital Camera</b>
                                            Qty 1<br>
                                            + Gear Guard Pro Damage Waiver<br>
                                            Total $86.23
                                        </p>
                                    </div>
                                </div>
                                <div class="row mb-4">
                                    <div class="col-3 text-center"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD5796f428-6173-4219-8a48-683c1afd2b05.jpg"></div>
                                    <div class="col-9 mt-3">
                                        <p class="gray80 body14">
                                            <b class="gray100">Canon RF 15-35mm  F2.8 L IS USM</b>
                                            Qty 1<br>
                                            + No Damage Waiver<br>
                                            Total $86.23
                                        </p>
                                    </div>
                                </div>
                                <div class="row mb-4">
                                    <div class="col-3 text-center"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD40fd25f3-6455-43bb-977a-794e3930e6aa.jpg"></div>
                                    <div class="col-9 mt-3">
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
                                <h5 class="mb-4">Delivery </h5>
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
                            </div>
                            <div class="reviewCart">
                                <h5 class="mb-4">Payment </h5>
                                <div class="row mb-4">
                                    <div class="col-2 text-center"><img src="assets/cc-mastercard.png" style="width: 49px;"></div>
                                    <div class="col-10 col-md-5">
                                        <p class="gray80 body14">
                                            <b class="gray100">Mastercard</b>
                                            ****1234. •. exp 11/24
                                        </p>
                                    </div>
                                    <div class="col-12 col-md-5">
                                        <p class="gray80 body14">
                                            <b class="gray100">Order Notes</b>
                                            JayZ Superbowl Shoot
                                        </p>
                                    </div>
                                </div>
                            </div>
                            <div class="reviewCart">
                                <h5 class="mb-4">Gift Card</h5>
                                <div class="row mb-4">
                                    <div class="col-2 text-center"><img src="assets/bl-logo@2x.png" style="width: 49px;"></div>
                                    <div class="col-5">
                                        <b class="body14 gray100">BL Gift Card</b>
                                        <div class="row">
                                            <div class="col-6">
                                                <p class="body14">
                                                Card #<br>
                                                Applied<br>
                                                Balance</p>
                                            </div>
                                            <div class="col-6">
                                                <p class="body14 gray80">
                                                12345<br>
                                                $100.00<br>
                                                $0</p>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                            <div class="cart-actions">
                                <a href="#" class="btn btn-sm btn-primary float-end">Rent Again</a>
                            </div>

                        </div>
                        <div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top mt-5 mt-md-0">
                            <div id="orderSummary" class="card">
                                <h5>Order Summary</h5>
                                <hr>
                                <p><b>Dates</b>&emsp;<span class="gray80">Apr 20 - Apr 23 (3 Days)</span></p>
                                <hr>
                                <table id="costSummary">
                                    <tbody>
                                        <tr>
                                            <td class="gray80">Rental Cost</td>
                                            <td class="text-end">$98.00</td>
                                        </tr>
                                        <tr>
                                            <td class="gray80">Damage Waiver <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></td>
                                            <td class="text-end">$98.00</td>
                                        </tr>
                                        <tr>
                                            <td class="gray80">Shipping*</td>
                                            <td class="text-end">$37.45</td>
                                        </tr>
                                        <tr>
                                            <td class="gray80">Est. Taxes*</td>
                                            <td class="text-end">$16.69</td>
                                        </tr>
                                        <tr class="discount">
                                            <td>Discount</td>
                                        </tr>
                                        <tr class="total">
                                            <td>Total</td>
                                            <td class="text-end">$160.23</td>
                                        </tr>
                                    </tbody>
                                </table>
                                <button class="btn btn-block btn-primary mt-4">Rent Again</button>
                            </div>
                        </div>
                    </div>
                </div>