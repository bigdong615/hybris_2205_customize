<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>

<spring:htmlEscape defaultHtmlEscape="true" />
								<c:set var="searchUrl" value="/my-account/orders?sort=${ycommerce:encodeUrl(searchPageData.pagination.sort)}" />
								<c:url var="homepageUrl" value="/" />
                <c:if test="${empty searchPageData.results}">
                	<div id="accountContent" class="col-lg-8 offset-lg-1">
                		<h1><spring:theme code="text.myaccount.recent.order"/></h1>
                		<hr>
                		<div class="notification no-orders">
                			<p><strong>
                				<spring:theme code="text.myaccount.empty.order" /></strong></p>
                			<p>
                				<p><spring:theme code="text.myaccount.empty.order.instruction.first"/>
                				<a href="${homepageUrl}"><spring:theme code="text.myaccount.empty.order.instruction.middle"/></a>
                				<spring:theme code="text.myaccount.empty.order.instruction.last"/></p>

                				</p>
                		</div>
                		<c:url value="/contactus" var="link"/>
                		<c:set var ="notificationDate" value="${jalosession.tenant.config.getParameter('bl.order.history.notification.message.date')}"/>
            <div class="mt-2 notification notification-warning"><spring:theme code="text.myaccount.notification.message.order.history" arguments="${fn:escapeXml(notificationDate)}" htmlEscape="false"/> <a href="${link}"> <spring:theme code="text.myaccount.contactus.message.order.history"/></a></div>
                	</div>
                </c:if>
                <c:if test="${not empty searchPageData.results}">
                	<div id="accountContent" class="col-lg-8 offset-lg-1">
                		<h1><spring:theme code="text.myaccount.recent.order"/></h1>
                		<c:forEach items="${searchPageData.results}" var="order">
                		<div class="order-block">
                				<div class="row">
                				     <c:choose>
                                        <c:when test="${order.isGiftCard}">
                                            <div class="col-12 col-md-7">
							                <p class="mb-0">
								               <b>${order.orderDate}</b>
							                </p>
							               <p class="body14">
								               ${fn:escapeXml(order.total.formattedValue)} &nbsp;
								<spring:theme
									code="order.gift.card.myaccount.review.page.gift.certificate" />
							</p>
						</div>
						<div class="col-6 col-md-3 offset-md-1 text-start text-md-end">
							<p class="my-2">
								<spring:theme code="order.gift.card.myaccount.order.completed" />
							</p>
						</div>
						                					<div class="col-6 col-md-1">
                						<div class="btn-group"> <a id="btn-rental-001" class="dropdown-toggle" data-bs-toggle="dropdown" aria-expanded="false" href="#"><i class="icon-dots"></i></a>
                							<ul class="dropdown-menu" aria-labeledby="btn-rental-001">
                								<li>
                									<c:url value="/my-account/order/${order.code}" var="viewOrderAction" />
                									<a href="${viewOrderAction}">
                										<spring:theme code="text.myaccount.order.view" /> </a>
                								</li>
                								<li>
                								<c:if test="${order.rentalCart}">
                								
                								 <c:url value="/rent/product/${order.productCode}" var="rentOrderAction" />
                                 <a href="${rentOrderAction}">
                                                        <spring:theme code="order.gift.card.myaccount.order.another"/> </a>
                							  </c:if>
                								</li>
                								<li>
                						
                								<c:if test="${not empty agent.uid && (order.orderStatus eq 'Pending') && (order.isCaptured eq false) }">
                								 <c:url value="/my-account/modifyPayment/${order.code}" var="modifyPaymentAction" />
                                 <a href="${modifyPaymentAction}">
                                                        <spring:theme code="order.myaccount.modify.payment"/> </a>
                							  </c:if>
                								</li>
                								<li>
                									<c:if test="${not empty agent.uid && order.orderStatus eq 'Pending' }">
                								 <c:url value="/my-account/${order.code}/depositPayment" var="depositPaymentAction" />
                                 <a href="${depositPaymentAction}">
                                                        <spring:theme code="order.myaccount.deposit.payment"/> </a>
                							  </c:if>
                								</li>
                								<li>
                									<c:if test="${not empty agent.uid && order.orderStatus eq 'Shipped' }">
                								 <c:url value="/my-account/${order.code}/modifiedOrderPayment" var="modifiedOrderPayment" />
                                 <a href="${modifiedOrderPayment}">
                                                        <spring:theme code="order.myaccount.modified.order.payment"/> </a>
                							  </c:if>
                								</li>
                							</ul>
                						</div>
                					</div>
						<div class="col-12 mt-4">
							<div class="row">
								<div class="col-4 col-md-2">
									<p class="body14">

										<spring:theme code="text.myaccount.order.rental.total.cost" />
										<br>
										<spring:theme code="text.myaccount.order" />
									</p>
									</p>
								</div>
								<div class="col-8 col-md-10">
									<p class="body14 gray60">${fn:escapeXml(order.total.formattedValue)}
										<br> ${fn:escapeXml(order.code)}
									</p>
								</div>
							</div>
						</div>
                      </c:when>
                          <c:otherwise>
  
                				<c:if test="${!order.rentalCart}">
                                 <div class="col-12 col-md-7">
                                      <p class="mb-0"><b>${order.orderDate}</b></p>
                                       <p class="body14">
                                       <c:forEach items="${order.productNameAndQuantity}" var="cartEntry">
                                             ${cartEntry}<br>
                                       </c:forEach>
                                      </p>
                     						</div>
                                 <div class="col-6 col-md-3 offset-md-1 text-start text-md-end">
                                  <c:choose>
                                    <c:when test="${order.retailGearOrder eq true}">
                                      <p class="my-2"> ${order.status.code}</p>
                                    </c:when>
                                    <c:otherwise>
                                               <p class="my-2">${order.orderStatus}</p>
                                    </c:otherwise>
                                  </c:choose>
                                 </div>
                        </c:if>
                					<c:if test="${order.rentalCart}">
                						<div class="col-5 col-md-3">
                						<c:if test="${order.isRentalActive eq true}">
                							<p class="lightteal mb-0"><b>${order.rentalStartDate}</b></p>
                							<p class="body14">
                								<spring:theme code="text.myaccount.order.rental.Starts" /> </p>
                						</c:if>
                						<c:if test="${order.isRentalActive eq false}">
                                            							<p class="mb-0"><b>${order.rentalStartDate}</b></p>
                                            							<p class="body14">
                                            								<spring:theme code="text.myaccount.order.rental.history.Started"/> </p>
                            </c:if>
                						</div>
                						<div class="col-2 col-md-1 text-center"> <img class="rental-arrow" src="${themeResourcePath}/assets/icon-arrow.svg"> </div>
                						<div class="col-5 col-md-3">
                						<c:if test="${order.isRentalActive eq true}">
                							<p class="lightteal mb-0"><b>${order.rentalEndDate}</b></p>
                							<p class="body14">
                								<spring:theme code="text.myaccount.order.rental.ends" /> </p>
                						</c:if>
                						   <c:if test="${order.isRentalActive eq false}">
                                        <p class="mb-0"><b>${order.rentalEndDate}</b></p>
                                           <p class="body14">
                                            	<spring:theme code="text.myaccount.order.rental.history.ended" />
                                           </p>
                                </c:if>
                						</div>
                						  <div class="col-6 col-md-3 offset-md-1 text-start text-md-end three">
                						  <c:choose>

                							<c:when test="${(order.isRentalActive eq true && order.isRentalStartDateActive eq true && order.orderReturnedToWarehouse eq false && order.orderStatus ne 'Canceled') || (not empty agent.uid && order.orderReturnedToWarehouse eq false && order.orderStatus ne 'Canceled')}">
                							<c:url value="/my-account/extendRent/${order.code}" var="extendRentAction" />
                							<a href="${extendRentAction}" class="btn btn-primary">
                								<spring:theme code="text.myaccount.order.extend.rent" /> </a>
                								<c:if test="${order.orderStatus ne null && order.orderStatus eq 'Shipped'}">
                								 <p class="my-2">${order.orderStatus}</p>
                								 </c:if>
                					  </c:when>

                           <c:otherwise>
                					   <p class="my-2">${order.orderStatus}</p>
                					 </c:otherwise>
                					   </c:choose>
                            </div>
                					</c:if>
                					<div class="col-6 col-md-1">
                					
                						<div class="btn-group view-links"> <a id="btn-rental-001" class="dropdown-toggle" data-bs-toggle="dropdown" aria-expanded="false" href="#"><i class="icon-dots"></i></a>
                							<ul class="dropdown-menu" aria-labeledby="btn-rental-001">
                								<li>
                									<c:url value="/my-account/order/${order.code}" var="viewOrderAction" />
                									<a href="${viewOrderAction}">
                										<spring:theme code="text.myaccount.order.view" /> </a>
                								</li>
                								<li>
                								<c:if test="${order.rentalCart}">
                								 <c:url value="/my-account/order/${order.code}" var="rentOrderAction" />
                                                   <a href="${rentOrderAction}">
                                                        <spring:theme code="text.myaccount.order.rent.again"/> </a>
                							  </c:if>
                								</li>
                								
                								
                						
                								<c:if test="${not empty agent.uid && (order.orderStatus eq 'Pending') && (order.isCaptured eq false) }">
													<li></li>
                								 <c:url value="/my-account/modifyPayment/${order.code}" var="modifyPaymentAction" />
                                                      <a href="${modifyPaymentAction}">
                                                        <spring:theme code="order.myaccount.modify.payment"/> </a>
													</li>
                							  </c:if>
                								
                								
                									<c:if test="${not empty agent.uid && (order.orderStatus eq 'Pending') }">
														<li>
                								 <c:url value="/my-account/${order.code}/depositPayment" var="depositPaymentAction" />
                                              <a href="${depositPaymentAction}">
                                                        <spring:theme code="order.myaccount.deposit.payment"/> </a>
													</li>
													</c:if>
													<li>
                									<c:if test="${not empty agent.uid && order.orderStatus eq 'Shipped' }">
                								 <c:url value="/my-account/${order.code}/modifiedOrderPayment" var="modifiedOrderPayment" />
                                 <a href="${modifiedOrderPayment}">
                                                        <spring:theme code="order.myaccount.modified.order.payment"/> </a>
                							  </c:if>
                								</li>
                								 
                							</ul>
                						</div>
                					</div>
                					<div class="col-12 mt-4">
                						<div class="row">
                							<div class="col-4 col-md-2">
                								<p class="body14">
                									<c:if test="${order.rentalCart}"><spring:theme code="text.myaccount.order.rental.total"/></c:if>
                									<c:if test="${!order.rentalCart}"><spring:theme code="text.myaccount.order.rental.total.cost"/></c:if>
                									<br><spring:theme code="text.myaccount.order"/>
                									 <c:if test="${order.isReplacementOrder eq true}">
                                       <spring:theme code="text.myaccount.order.replacementFor"/>
                                   </c:if>
                                </p>
                							</div>
                							<div class="col-8 col-md-10">
                								<p class="body14 gray60">${fn:escapeXml(order.total.formattedValue)}
                									<br> ${fn:escapeXml(order.code)}
                									 <c:if test="${order.isReplacementOrder eq true}">
                                        <br> ${order.replacementFor}
                                   </c:if>
                									</p>
                							</div>
                							<c:url value="/my-account/${order.code}/payBill" var="payBillAction" />
                						  
                							<c:choose>
                							<c:when test="${(order.payBillingCost.value gt 0) and (not empty (order.code))}">
                									<div class="mt-2 notification notification-error"><spring:theme code="text.myaccount.order.unpaidbill"/> (#${fn:escapeXml(order.code)}). <a href="${payBillAction}">Pay Invoice</a></div>
                							</c:when>
                							<c:when test="${(order.payBillingCost.value gt 0) and (empty order.code)}">
                							         <div class="mt-2 notification notification-error"><spring:theme code="text.myaccount.order.unpaidbill"/>. <a href="${payBillAction}">Pay Invoice</a></div>
                							</c:when>
                							<c:otherwise>
                							      
                							</c:otherwise>	
                							</c:choose>
                						</div>
                					</div>
                					</c:otherwise>
                					</c:choose>
                				</div>
                			</div>
                		</c:forEach>
                		<div class="account-orderhistory-pagination">
                			<nav:pagination searchPageData="${searchPageData}" searchUrl="${searchUrl}" /> </div>
                			<c:url value="/contactus" var="link"/>
                		<c:set var ="notificationDate" value="${jalosession.tenant.config.getParameter('bl.order.history.notification.message.date')}"/>
            <div class="mt-2 notification notification-warning"><spring:theme code="text.myaccount.notification.message.order.history" arguments="${fn:escapeXml(notificationDate)}" htmlEscape="false"/> <a href="${link}"> <spring:theme code="text.myaccount.contactus.message.order.history"/></a></div>
                              	</div>
                	</c:if>