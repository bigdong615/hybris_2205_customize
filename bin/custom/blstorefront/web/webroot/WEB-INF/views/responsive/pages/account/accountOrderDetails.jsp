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
                          <c:if test="${orderData.isRentalCart}">
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
                                        <p class="overline"><spring:theme code="text.myaccount.order.rental.end"/></p>
                                        <p class="lightteal mb-0"><b>${orderData.rentalEndDate}</b></p>
                                        <p class="body14"><spring:theme code="text.myaccount.order.date.end.delivery" arguments="${orderData.deliveryMode.carrier}"/></p>
                                    </div>
                                </div>
                            </div>
                          </c:if>
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

                             <c:if test="${not empty orderData.extendOrderEntrie}">
                               <div class="col-12 mt-4">
                                 <b><spring:theme code="text.myaccount.extend.order.details"/></b>
                                   <c:forEach items="${orderData.extendOrderEntrie}" var="extendOrder">
                                        <div class="row">
                                           <div class="col-5 col-md-4">
                                              <p class="body14"><spring:theme code="text.order.extend.text"/><br>
                                                    <spring:theme code="text.myaccount.extend.order.added.time"/><br>
                                                    <spring:theme code="text.myaccount.extend.order.cost"/><br>
                                                    <spring:theme code="text.myaccount.order.rental.damege.waiver"/></p>
                                           </div>
                                           <div class="col-7 col-md-8">
                                               <p class="body14 gray60">
                                                 ${extendOrder.extendOrderEndDate}<br>
                                                 ${extendOrder.extendOrderDaysWithoutPrevOrder}<br>
                                                 <format:blPrice priceData="${extendOrder.extendOrderCost}"/></br>
                                                <c:choose>
                                                <c:when test="${extendOrder.extendOrderDamageWaiverCost.value > 0}">
                                                 <format:blPrice priceData="${extendOrder.extendOrderDamageWaiverCost}"/>
                                                </c:when>
                                                <c:otherwise>
                                                $0.00
                                                </c:otherwise>
                                                </c:choose>
                                               </p>
                                           </div>
                                        </div>
                                   </c:forEach>
                               </div>
                             </c:if>
                             </div>
                            <div class="reviewCart">
                                <h5 class="mb-4">
                                 <c:if test="${orderData.isRentalCart}">
                                <spring:theme code="text.myaccount.order.your.rental"/>
                                </c:if>
                                <c:if test="${!orderData.isRentalCart}">
                                Your Order
                                </c:if>
                                </h5>
                               <c:forEach items="${orderData.entries}" var="cartEntry" >
                               		 <div class="row mb-4">
                               				<div class="col-md-3 text-center">
                               								<product:productPrimaryImage product="${cartEntry.product}" format="thumbnail"/>
                               				</div>
                               					<div class="col-md-9 mt-3">
                               							<p class="gray80 body14">
                               							 <b class="gray100">${cartEntry.product.name}</b>
                               							 <spring:theme code="text.myaccount.order.your.rental.qty"/> ${cartEntry.quantity}<br>
                               							  <c:if test="${orderData.isRentalCart}">
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
                               						</c:if>
                               							<spring:theme code="text.review.page.your.rental.total"/>
                               							<format:price priceData="${cartEntry.totalPrice}" displayFreeForZero="true" /></p>
                               					</div>
                               				</div>
                                   </c:forEach>
                            </div>
                            <div class="reviewCart">
                            							<c:choose>
                            								<c:when
                            									test="${fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'SHIP_UPS_OFFICE') == true or fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == true}">
                            								<h5 class="mb-4">
                                            <spring:theme code="text.review.page.delivery.pickup.title" />
                                            </h5>
                            									<div class="row mb-4">
                            										<%-- <div class="col-6">
                            											<p class="gray80 body14">
                            												<b class="gray100"><spring:theme
                            														code="text.review.page.delivery.mode.pickup" /></b>
                            												${orderData.pickUpPersonFirstName}&nbsp;${orderData.pickUpPersonLastName}
                            												<br /> ${orderData.pickUpPersonEmail} <br />
                            												${orderData.pickUpPersonPhone} <br />
                            											</p>
                            										</div> --%>
                            										<c:if test="${not empty orderData.deliveryAddress}">
                            											<div class="col-6">
                            												<p class="gray80 body14">
                            													<b class="gray100"><spring:theme
                            															code="text.review.page.delivery.pickup.from" /></b></br>
                            													<order:addressItem address="${orderData.deliveryAddress}" />
                            												</p>
                            											</div>
                            										</c:if>
                            									</div>
                            								</c:when>
                            								<c:otherwise>
                            									<div class="row mb-4">
                            										<div class="col-6">
                            											<p class="gray80 body14">
                            												<b class="gray100"><spring:theme
                            														code="text.review.page.delivery.mode" /></b>
                            												${orderData.deliveryMode.name}
                            											</p>
                            										</div>
                            										<c:if test="${not empty orderData.deliveryAddress}">
                            											<div class="col-6">
                            												<p class="gray80 body14">
                            													<b class="gray100"><spring:theme
                            															code="text.review.page.delivery.shipping.to" /></b>
                            													<order:addressItem address="${orderData.deliveryAddress}" />
                            												</p>
                            											</div>
                            										</c:if>
                            									</div>
                            								</c:otherwise>
                            							</c:choose>
                            						</div>

                            <div class="reviewCart">
                            <c:choose>
                             <c:when test="${not empty orderData.paymentInfo}">
                                <h5 class="mb-4"><spring:theme code="text.myaccount.order.payment.title"/></h5>
                                <div class="row mb-4">
                               <order:accountPaymentDetails orderData="${orderData}" paymentInfo="${orderData.paymentInfo}"/>
                                </div>
                               </c:when>
                               <c:otherwise>
                               <div class="row">
                                   	<div class="col-2 text-center">
                                   		<img
                                   			src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-po.png"
                                   			style="width: 50px;">
                                   	</div>
                                   	<div class="col-10 col-md-5">
                                   		<b class="body14 gray100"><spring:theme code="text.review.page.payment.po" /></b>
                                   		<div class="row">
                                   			<div class="col-6">
                                   				<p class="body14">
                                   					<spring:theme code="text.review.page.payment.amount" />
                                   				</p>
                                   			</div>
                                   			<div class="col-6">
                                   				<p class="body14 gray80">
                                   					<format:price priceData="${orderData.totalPriceWithTax}" />
                                   				</p>
                                   			</div>
                                   		</div>
                                   	</div>
                                   	<div class="col-12 col-md-5">
                                   	  <div class="po-order-notes">
                                   		  <p class="gray80 body14">
                                   			  <b class="gray100"><spring:theme code="text.order.confirmation.print.page.po.notes"/></b>
                                   			  <c:choose>
                                   				  <c:when test="${orderData.poNotes == ''}">
                                                <spring:theme code="text.review.page.payment.notes.na"/>
                                   				  </c:when>
                                   				  <c:otherwise>
                                               ${orderData.poNotes}
                                   				  </c:otherwise>
                                   			  </c:choose>
                                   		  </p>
                                   	  </div>
                                   	</div>
                                   </div>
                                  </c:otherwise>
                                </c:choose>
                            </div>
                             <c:if test="${not empty orderData.giftCardData}">
                              <order:accountGiftCardDetails orderData="${orderData}"/>
                             </c:if>
                               <c:if test="${orderData.isRentalCart}">
                            <div class="cart-actions">

                            <c:choose>
                            <c:when test="${isUsedGearCartActive eq true}">
                                     <a href="#" class="btn btn-sm btn-primary float-end" data-bs-toggle="modal" data-bs-target="#rentAgainPopUp">
                                     <spring:theme code="text.myaccount.order.rent.again"/> </a>
                            </c:when>
                            <c:otherwise>
                            <c:url value="/my-account/rentAgain/${orderData.code}" var="rentOrderAction" />
                               <a href="${rentOrderAction}" class="btn btn-sm btn-primary float-end" data-order-id="${orderData.code}">
                                   <spring:theme code="text.myaccount.order.rent.again"/> </a>

                            </c:otherwise>
                           </c:choose>
                          </div>
                            </c:if>

                        </div>
                        <div class="col-lg-4 offset-lg-1 d-lg-block sticky-lg-top mt-5 mt-md-0">
                            <div id="orderSummary" class="card">
                                <h5><spring:theme code="text.myaccount.order.summary"/></h5>
                                <hr>
                                 <c:if test="${orderData.isRentalCart}">
                                <p><b><spring:theme code="text.myaccount.order.dates"/></b>&emsp;<span class="gray80">${orderData.rentalFormattedStartDate} - ${orderData.rentalFormattedEndDate} (${orderData.totalRentalDays} Days)</span></p>
                                <hr>
                                  </c:if>
                                <table id="costSummary">
                                    <tbody>
                                        <tr>
                                            <td class="gray80">
                                              <c:if test="${orderData.isRentalCart}">
                                            <spring:theme code="text.myaccount.order.rental.cost"/>
                                             </c:if>
                                              <c:if test="${!orderData.isRentalCart}">
                                                               Item Cost
                                              </c:if>
                                            </td>
                                            <td class="text-end"> <format:price priceData="${orderData.subTotal}"/></td>
                                        </tr>
                                          <c:if test="${orderData.isRentalCart}">
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.myaccount.order.rental.damege.waiver"/> <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></td>
                                            <td class="text-end"><format:price priceData="${orderData.totalDamageWaiverCost}"/></td>
                                        </tr>
                                        </c:if>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.myaccount.order.shipping.text"/></td>
                                            <td class="text-end"><format:price priceData="${orderData.deliveryCost}"/></td>
                                        </tr>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.myaccount.order.tax.text"/></td>
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
                                  <c:if test="${orderData.isRentalCart}">
                                      <c:choose>
                                          <c:when test="${isUsedGearCartActive eq true}">
                                                   <a href="#" class="btn btn-sm btn-primary float-end" data-bs-toggle="modal" data-bs-target="#rentAgainPopUp">
                                                         <spring:theme code="text.myaccount.order.rent.again"/>
                                                   </a>
                                          </c:when>
                                          <c:otherwise>
                                               <c:url value="/my-account/rentAgain/${orderData.code}" var="rentOrderAction" />
                                                    <a href="${rentOrderAction}" class="btn btn-block btn-primary mt-4" data-order-id="${orderData.code}">
                                                        <spring:theme code="text.myaccount.order.rent.again"/>
                                                    </a>
                                          </c:otherwise>
                                      </c:choose>
                                </c:if>
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

 <!--Used Gear Cart Error  -->

<div class="modal fade" id="rentAgainPopUp" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-sm">
      <div class="modal-content">
      <div class="modal-header">
      <h5>Wait!</h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
      <div class="modal-body">
         <p>
                 Wait!
      </p>
      <div class="modal-actions">
                      <p class="text-center mb-0">
                     	<a href="#" class="lightteal" aria-label="Close" data-bs-dismiss="modal" aria-label="Close">
                     											<spring:theme code="basket.save.cart.action.cancel" /> </a></p>
          </div>

      </div>
      </div>
      </div>
</div>

