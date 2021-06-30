<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<spring:htmlEscape defaultHtmlEscape="true" />
 <div class="col-xl-10">
                    <div class="row">
                        <div id="accountContent" class="col-lg-7">
                        <h1><spring:theme code="text.account.order.title.details"/></h1>
                            <hr>
                            <div class="reviewCart">
                                <h5 class="mb-4"><spring:theme code="text.myaccount.order.rental.dates"/></h5>
                                <div class="row">
                                    <div class="col-4">
                                        <p class="overline"><spring:theme code="text.myaccount.order.rental.starts"/></p>
                                        <p class="lightteal mb-0"><b>${orderData.rentalStartDate}</b></p>
                                        <p class="body14"><spring:theme code="text.myaccount.order.date.start.delivery" arguments="${orderData.deliveryMode.carrier}"/></p>
                                    </div>
                                    <div class="col-2 text-center">
                                        <img class="rental-arrow" src="${themeResourcePath}/assets/icon-arrow.svg">
                                    </div>
                                    <div class="col-4">
                                        <p class="overline"><spring:theme code="text.myaccount.order.rental.ends"/></p>
                                        <p class="lightteal mb-0"><b>${orderData.rentalEndDate}</b></p>
                                        <p class="body14"><spring:theme code="text.myaccount.order.date.end.delivery" arguments="${orderData.deliveryMode.carrier}"/></p>
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
                                            ${orderData.status}<br>
                                            ${orderData.orderedFormatDate}<br>
                                            ${fn:escapeXml(orderData.code)}<br>
                                            N/A
                                        </p>
                                    </div>
                                </div>
                            </div>
                            <div class="reviewCart">
                                <h5 class="mb-4"><spring:theme code="text.myaccount.order.your.rental"/></h5>
                               <c:forEach items="${orderData.entries}" var="cartEntry" >
                               		 <div class="row mb-4">
                               				<div class="col-md-3 text-center">
                               								<product:productPrimaryImage product="${cartEntry.product}" format="thumbnail"/>
                               				</div>
                               					<div class="col-md-9 mt-3">
                               							<p class="gray80 body14">
                               							 <b class="gray100">${cartEntry.product.name}</b>
                               							 <spring:theme code="text.myaccount.order.your.rental.qty"/> ${cartEntry.quantity}<br>
                               		        <c:choose>
                               								<c:when test="${cartEntry.gearGuardProFullWaiverSelected}">
                               									 <spring:theme code="text.myaccount.order.damage.waiver.gear.plus"/><br>
                               							  </c:when>
                               								<c:when test="${cartEntry.gearGuardWaiverSelected}">
                               								   <spring:theme code="text.myaccount.order.damage.waiver.gear"/><br>
                               								</c:when>
                               								<c:otherwise>
                               										 <spring:theme code="text.myaccount.order.damage.waiver.gear.no"/><br>
                               								</c:otherwise>
                               						</c:choose>
                               							<spring:theme code="text.review.page.your.rental.total"/>
                               							<format:price priceData="${cartEntry.totalPrice}" displayFreeForZero="true" /></p>
                               					</div>
                               				</div>
                                   </c:forEach>
                            </div>
                            <div class="reviewCart">
                                <h5 class="mb-4"><spring:theme code="text.myaccount.order.delivery"/></h5>
                                <div class="row mb-4">
                                    <div class="col-6">
                                        <p class="gray80 body14">
                                            <b class="gray100"><spring:theme code="text.myaccount.order.delivery.method"/></b>
                                           ${orderData.deliveryMode.name}
                                        </p>
                                    </div>
                                    <div class="col-6">
                                        <p class="gray80 body14">
                                            <b class="gray100"><spring:theme code="text.myaccount.order.shippingto"/></b>
                                            <order:addressItem address="${orderData.deliveryAddress}"/>
                                        </p>
                                    </div>
                                </div>
                            </div>
                            <div class="reviewCart">
                             <c:if test="${not empty orderData.paymentInfo}">
                                <h5 class="mb-4"><spring:theme code="text.myaccount.order.payment.title"/></h5>
                                <div class="row mb-4">
                                <c:set var= "cardName" value="${fn:escapeXml(orderData.paymentInfo.cardType)}"/>
                                <div class="col-2 text-center"><img src="${themeResourcePath}/assets/payment-${fn:replace(fn:toLowerCase(cardName),' ', '_')}.png"
                                style="width: 49px;"></div>
                                    <div class="col-10 col-md-5">
                                        <p class="gray80 body14">
                                           <b class="gray100"> ${fn:escapeXml(orderData.paymentInfo.cardType)}</b>${fn:escapeXml(orderData.paymentInfo.cardNumber)}. •.
                                          exp ${fn:escapeXml(orderData.paymentInfo.expiryMonth)}/${fn:escapeXml(orderData.paymentInfo.expiryYear)} <br/>
                                        </p>
                                    </div>
                                    <div class="col-12 col-md-5">
                                        <p class="gray80 body14">
                                            <b class="gray100"><spring:theme code="text.myaccount.order.notes"/></b>
                                            JayZ Superbowl Shoot
                                        </p>
                                    </div>
                                </div>
                               </c:if>
                            </div>
                             <c:if test="${not empty orderData.giftCardData}">
                            <div class="reviewCart">

                                <h5 class="mb-4"><spring:theme code="text.myaccount.order.gift.cart"/></h5>
                                <div class="row mb-4">
                                    <div class="col-2 text-center"><img src="${themeResourcePath}/assets/bl-logo@2x.png" style="width: 49px;"></div>
                                    <div class="col-5">
                                        <b class="body14 gray100"><spring:theme code="text.myaccount.order.gift.cart"/></b>
                                        <div class="row">
                                            <div class="col-6">
                                                <p class="body14">
                                                <spring:theme code="text.myaccount.order.gift.cart.card"/><br>
                                                <spring:theme code="text.myaccount.order.gift.cart.applied"/><br>
                                                <spring:theme code="text.myaccount.order.gift.cart.balance"/></p>
                                            </div>
                                            <div class="col-6">
                                                <p class="body14 gray80">
                                                <c:forEach items="${orderData.giftCardData}" var="giftCardEntry">
                                                  ${giftCardEntry.code}<br>
                                                 <format:price priceData="${giftCardEntry.redeemamount}"/><br>
                                                 <format:price priceData="${giftCardEntry.balanceamount}"/></p>
                                                </c:forEach>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
                             </c:if>
                            <div class="cart-actions">
                                <a href="#" class="btn btn-sm btn-primary float-end"><spring:theme code="text.myaccount.order.rent.again"/></a>
                            </div>

                        </div>
                        <div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top mt-5 mt-md-0">
                            <div id="orderSummary" class="card">
                                <h5><spring:theme code="text.myaccount.order.summary"/></h5>
                                <hr>
                                <p><b><spring:theme code="text.myaccount.order.dates"/></b>&emsp;<span class="gray80">${orderData.rentalFormattedStartDate} - ${orderData.rentalFormattedEndDate} (${orderData.totalRentalDays} Days)</span></p>
                                <hr>
                                <table id="costSummary">
                                    <tbody>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.myaccount.order.rental.cost"/></td>
                                            <td class="text-end"> <format:price priceData="${orderData.subTotal}"/></td>
                                        </tr>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.myaccount.order.rental.damege.waiver"/> <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></td>
                                            <td class="text-end"><format:price priceData="${orderData.totalDamageWaiverCost}"/></td>
                                        </tr>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.myaccount.order.shipping"/></td>
                                            <td class="text-end"><format:price priceData="${orderData.deliveryCost}"/></td>
                                        </tr>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.myaccount.order.tax"/></td>
                                            <td class="text-end"><format:blPrice priceData="${orderData.taxAvalaraCalculated}" /></td>
                                        </tr>
                                         <c:if test="${orderData.totalDiscounts.value > 0}">
                                        <tr class="discount">
                                            <td><spring:theme code="text.myaccount.order.discount"/></td>
                                            <td class="text-end"> -<format:blPrice priceData="${orderData.totalDiscounts}"/></td>
                                        </tr>
                                          </c:if>
                                        <tr class="total">
                                            <td><spring:theme code="text.myaccount.order.total"/></td>
                                            <td class="text-end"><format:price priceData="${orderData.totalPriceWithTax}" /></td>
                                        </tr>
                                    </tbody>
                                </table>
                                <button class="btn btn-block btn-primary mt-4"><spring:theme code="text.myaccount.order.rent.again"/></button>
                            </div>
                        </div>
                    </div>
                </div>

<!-- DamageWaivers Modal -->
<div class="modal fade" id="damageWaivers" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-lg">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title"><spring:theme code="text.damage.Waiver.model.title"/></h5>
                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
            <div class="modal-body">
                <div class="row mb-4">
                    <div class="text-center col-md-3 col-lg-2">
                        <img src="${themeResourcePath}/assets/gear-guard-plus.png">
                    </div>
                    <div class="col-md-9 col-lg-10">
                        <p><b><spring:theme code="text.damage.Waiver.model.option.pro"/></b></p>
                        <p class="body14"><spring:theme code="text.damage.Waiver.model.option.pro.description"/></p>
                        <hr>
                    </div>
                </div>
                <div class="row mb-4">
                    <div class="text-center col-md-3 col-lg-2">
                        <img src="${themeResourcePath}/assets/gear-guard.png">
                    </div>
                    <div class="col-md-9 col-lg-10">
                        <p><b><spring:theme code="text.damage.Waiver.model.option.gear"/></b></p>
                        <p class="body14"><spring:theme code="text.damage.Waiver.model.option.gear.description"/></p>
                        <hr>
                    </div>
                </div>

                <div class="row">
                    <div class="text-center col-md-3 col-lg-2">
                        <img src="${themeResourcePath}/assets/gear-guard-none.png">
                    </div>
                    <div class="col-md-9 col-lg-10">
                        <p><b><spring:theme code="text.damage.Waiver.model.option"/></b></p>
                        <p class="body14"><spring:theme code="text.damage.Waiver.model.option.description"/></p>
                    </div>
                </div>
            </div>
          </div>
      </div>
  </div>