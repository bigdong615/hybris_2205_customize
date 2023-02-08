<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<%@ taglib prefix="account" tagdir="/WEB-INF/tags/addons/blassistedservicestorefront/order" %>
<spring:htmlEscape defaultHtmlEscape="true" />
<spring:url value="/my-account/order" var="orderDetailsUrl" htmlEscape="false"/>

<c:choose>
<c:when test="${!orderData.hasGiftCart}">
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
                                          <c:if test="${orderData.isRentalActive eq true}">
                                               <p class="lightteal mb-0">
                                                        <b>${orderData.rentalStartDate}</b>
                                               </p>
                                           </c:if>
                                           <c:if test="${orderData.isRentalActive eq false}">
                                                  <p class="mb-0">
                                                    <b>${orderData.rentalStartDate}</b>
                                                  </p>
                                           </c:if>
									<c:choose>
										<c:when
											test="${fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'SHIP_UPS_OFFICE') == true or fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == true}">
											<p class="body14">
												<spring:theme
													code="text.review.page.date.start.delivery.pickup.order.history" />
											</p>
										</c:when>
										<c:otherwise>
											<p class="body14">
												<spring:theme
													code="text.myaccount.order.date.start.delivery"
													arguments="${orderData.deliveryMode.carrier}" />
											</p>
										</c:otherwise>
									</c:choose>
								</div>
                                    <div class="col-2 text-center">
                                        <img class="rental-arrow" src="${themeResourcePath}/assets/icon-arrow.svg">
                                    </div>
                                    <div class="col-4">
                                        <p class="overline"><spring:theme code="text.myaccount.order.rental.end"/></p>
                                         <c:if test="${orderData.isRentalActive eq true}">
                                        <p class="lightteal mb-0"><b>${orderData.rentalEndDate}</b></p>
                                        </c:if>
                                        <c:if test="${orderData.isRentalActive eq false}">
                                          <p class="mb-0"><b>${orderData.rentalEndDate}</b></p>
                                        </c:if>

									<c:choose>
										<c:when
											test="${fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'SHIP_UPS_OFFICE') == true or fn:containsIgnoreCase(orderData.deliveryMode.shippingGroup, 'BL_PARTNER_PICKUP') == true}">
											<p class="body14">
												<spring:theme
													code="text.review.page.date.end.delivery.pickup.order.history" />
											</p>
										</c:when>
										<c:otherwise>
											<p class="body14">
												<spring:theme code="text.myaccount.order.date.end.delivery"
													arguments="${orderData.deliveryMode.carrier}" />
											</p>
										</c:otherwise>
									</c:choose>
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
                                        
                                        <c:if test="${orderData.isReplacementOrder eq true}">
                                        <b>	<spring:theme code="text.myaccount.order.replacementFor"/> </b><br>
                                        </c:if>
                                        <spring:theme code="text.myaccount.order.tracking"/>
                                        </p>
                                    </div>
                                    <div class="col-6">
                                        <p class="gray80 body14">
                                            ${orderData.status}<br>
                                            ${orderData.orderedFormatDate}<br>
                                            ${fn:escapeXml(orderData.code)}<br>

                                            <c:if test="${orderData.isReplacementOrder eq true}">
                                            <b>	${fn:escapeXml(orderData.replacementFor)} </b> <br>
                                            </c:if>
                                       <c:choose>
                                         <c:when test="${not empty orderData.trackingNumber && fn:containsIgnoreCase(orderData.status.code ,'Completed') == 'false'}">
                                           <c:forEach items="${orderData.trackingNumber}" var="trackingInfo">
                                             <c:if test="${trackingInfo.key ne null}">
                                               <c:url value="${trackingInfo.value}" var="trackingUrl" />
                                                 <c:if test="${fn:containsIgnoreCase(trackingInfo.value ,'ups') == 'true'}">
                                                     <a class="tracking-info" href="https://www.ups.com/track?loc=en_US&tracknum=${trackingInfo.key}" target="_new"> ${trackingInfo.key}</a></br>
                                                 </c:if>
                                                 <c:if test="${fn:containsIgnoreCase(trackingInfo.value ,'fedex') == 'true'}">
                                                     <a class="tracking-info" href="https://www.fedex.com/fedextrack/?trknbr=${trackingInfo.key}" target="_new"> ${trackingInfo.key}</a></br>
                                                 </c:if>
                                             </c:if>
                                            </c:forEach>
                                          </c:when>
                                          <c:otherwise>
                                           <spring:theme code="text.myaccount.order.na"/>
                                          </c:otherwise>
                                       </c:choose>
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
                               		 <c:url var="productUrl" value="/rent/product/${cartEntry.product.code}"/>
                                              <c:if test="${!orderData.isRentalCart}">
                                                <c:url var="productUrl" value="/buy/product/${cartEntry.product.code}"/>
                                              </c:if>

                               				<div class="col-md-3 text-center">
                               									<a href="${productUrl}"> <product:productPrimaryImage product="${cartEntry.product}" format="thumbnail"/> </a>
                               				</div>
                               					<div class="col-md-9 mt-3">
                               							<p class="gray80 body14">
                               							 <b class="gray100">
                               							<a href="${productUrl}" style="text-decoration: none"> ${cartEntry.product.name}</b></a> <br>
                               							 <spring:theme code="text.myaccount.order.your.rental.qty"/>&nbsp;${cartEntry.quantity}<br>
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
                               						<c:if test="${not empty cartEntry.selectedOptions}">
														+ ${cartEntry.selectedOptions} <br>
													</c:if>
                               						</c:if>
                               							<c:choose>
                               								<c:when test="${cartEntry.quantity <= 0 and cartEntry.totalPrice.value <= 0}">
                               									<p class="refund-msg"><spring:theme code="order.details.refunded.msg"/></p>
                               								</c:when>
                               								<c:otherwise>
                               									<spring:theme code="text.review.page.your.rental.total"/>
                               									<format:price priceData="${cartEntry.totalPrice}" displayFreeForZero="true" />
                               								</c:otherwise>
                               							</c:choose>
                               							
                               							<c:if test="${not empty cartEntry.product.bundleProductReference}">
                                             	<ul class="checklist mt-4">
                                                  <c:forEach items="${cartEntry.product.bundleProductReference}" var="bundleItems">
                                                  <li>${bundleItems.productReferenceName}</li>
                                                  </c:forEach>
                                              </ul>
                                           </c:if>
                               							</p>
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
                            										 <div class="col-6">
                            											<p class="gray80 body14">
                            												<b class="gray100"><spring:theme
                            														code="text.review.page.delivery.mode.pickup" /></b> </br>
                            												${orderData.pickUpPersonFirstName}&nbsp;${orderData.pickUpPersonLastName}
                            												<br /> ${orderData.pickUpPersonEmail} <br />
                            												${orderData.pickUpPersonPhone} <br />
                            											</p>
                            										</div>
                            										<c:if test="${not empty orderData.deliveryAddress}">
                            											<div class="col-6">
                            												<p class="gray80 body14">
                            													<b class="gray100"><spring:theme
                            															code="text.review.page.delivery.pickup.from" /></b> <br />
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
                            														code="text.review.page.delivery.mode" /></b> <br />
                            												${orderData.deliveryMode.name}
                            											</p>
                            										</div>
                            										<c:if test="${not empty orderData.deliveryAddress}">
                            											<div class="col-6">
                            												<p class="gray80 body14">
                            													<b class="gray100"><spring:theme
                            															code="text.review.page.delivery.shipping.to" /></b> <br />
                            													<order:addressItem address="${orderData.deliveryAddress}" />
                            												</p>
                            											</div>
                            										</c:if>
                            									</div>
                            								</c:otherwise>
                            							</c:choose>
                            						</div>

                            <c:if test="${orderData.isReplacementOrder ne true}">
                            <div class="reviewCart">
                            <h5 class="mb-4"><spring:theme code="text.myaccount.order.payment.title"/></h5>
                            <c:choose>
                             <c:when test="${not empty orderData.paymentInfo}">
                                
                                <div class="row mb-4">
                               <order:accountPaymentDetails orderData="${orderData}" paymentInfo="${orderData.paymentInfo}" displayOrderNote="true"/>
                               <c:if test="${orderData.modifiedOrderPaymentInfos.size() > 0 or not empty orderData.modifiedOrderPoNumber}">
                               			<h5 class="mb-4"><spring:theme code="order.details.modified.order.payment.msg"/></h5>
                               		</c:if>
                               <c:if test="${orderData.modifiedOrderPaymentInfos.size() > 0 }">
	                               <c:forEach var="modifiedPaymentInfo" items="${orderData.modifiedOrderPaymentInfos}">
	                               		<order:accountPaymentDetails orderData="${orderData}" paymentInfo="${modifiedPaymentInfo}" displayOrderNote="false"/>
	                               </c:forEach>
                               </c:if>
                               <c:if test="${not empty orderData.modifiedOrderPoNumber}">
                               			<order:accountOrderPoPayment orderData="${orderData}" displayOrderNote="false" poNote="${orderData.modifiedOrderPoNotes}"/>	
                               		</c:if>
                                </div>
                               </c:when>
                               <c:otherwise>
                               <div class="row">
                               		<order:accountOrderPoPayment orderData="${orderData}" displayOrderNote="true" poNote="${orderData.poNotes}"/>
                               		<c:if test="${orderData.modifiedOrderPaymentInfos.size() > 0 or not empty orderData.modifiedOrderPoNumber}">
                               			<h5 class="mb-4"><spring:theme code="order.details.modified.order.payment.msg"/></h5>
                               		</c:if>
                               		<c:if test="${orderData.modifiedOrderPaymentInfos.size() > 0 }">
	                               <c:forEach var="modifiedPaymentInfo" items="${orderData.modifiedOrderPaymentInfos}">
	                               		<order:accountPaymentDetails orderData="${orderData}" paymentInfo="${modifiedPaymentInfo}" displayOrderNote="false"/>
	                               </c:forEach>
                               </c:if>
                               		<c:if test="${not empty orderData.modifiedOrderPoNumber}">
                               			<order:accountOrderPoPayment orderData="${orderData}" displayOrderNote="false" poNote="${orderData.modifiedOrderPoNotes}"/>	
                               		</c:if>
                               </div>
                                  </c:otherwise>
                                </c:choose>
                               </div>
                            </c:if>
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
                           
                           <c:if test="${asmUser}"> 
                             	<c:set var="orderAction" value="/returnOrder" ></c:set>
							 	<a id="replaceProduct" href="${orderDetailsUrl}${orderAction}/${orderData.code}" class="btn btn-sm btn-primary" data-order-id="${orderData.code}">
							 		<spring:theme code="text.myaccount.order.return.request"/>
							 	</a>
							</c:if>
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
                                            <c:choose>
                                             <c:when  test="${orderData.isRetailGearOrder eq true}">
                                                <spring:theme code="text.checkout.multi.newgear.order.summary.cost"/>
                                             </c:when>
                                             <c:when test="${orderData.isRentalCart}">
                                               <spring:theme code="text.myaccount.order.rental.cost"/>
                                              </c:when>
                                             <c:when  test="${!orderData.isRentalCart}">
                                                 Item Cost
                                              </c:when>
                                              <c:otherwise>
                                              </c:otherwise>
                                              </c:choose>
                                            </td>
                                            <td class="text-end"> <format:price priceData="${orderData.subTotal}"/></td>
                                        </tr>
                                          <c:if test="${orderData.isRentalCart}">
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.myaccount.order.rental.damege.waiver"/> <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers"><i class="icon-support"></i></a></td>
                                            <td class="text-end"><format:price priceData="${orderData.totalDamageWaiverCost}"/></td>
                                        </tr>
                                        <c:if test="${orderData.totalOptionsCost.value gt 0}">
				<tr>
					<td class="gray80"><spring:theme
							code="text.cart.rental.options" /> </td>
					<td class="text-end"><format:blPrice
							priceData="${orderData.totalOptionsCost}" /></td>
				</tr>
				</c:if>
                                            </c:if>
                                        <tr>
                                            <td class="gray80"><spring:theme code="text.myaccount.order.shipping.text"/></td>
                                            <td class="text-end"><format:price priceData="${orderData.deliveryCost}"/></td>
                                        </tr>
                                        <tr>
                                            <td class="gray80">
                                            <c:if test="${orderData.isRentalCart}">
                                            <spring:theme code="text.myaccount.order.tax.text"/>
                                            </c:if>
                                            <c:if test="${orderData.isRentalCart eq false}">
                                            <spring:theme code="text.myaccount.order.tax.used"/>
                                            </c:if>
                                            </td>
                                            <c:choose>
                                            <c:when test="${orderData.isReplacementOrder eq true || orderData.taxAvalaraCalculated.value == '0.0'}">
                                            <td class="text-end"> $0.00 </td>
                                            </c:when>
                                            <c:otherwise>
                                            <td class="text-end"><format:blPrice priceData="${orderData.taxAvalaraCalculated}" /></td>
                                            </c:otherwise>
                                            </c:choose>
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
                            <c:if test="${not empty orderData.customerOwnedOrderNote}">
	                            <p class="customer-item-note"><spring:theme code="text.myaccount.order.level.note"/></p>
	                            <div class="notification-order-note notification-order-note-warning">${orderData.customerOwnedOrderNote} </div>
                            </c:if>
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

</c:when>
<c:otherwise>
 <order:accountGiftCardPurchaseOrderHistoryPage />
</c:otherwise>
</c:choose>