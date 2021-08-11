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
                	</div>
                </c:if>
                <c:if test="${not empty searchPageData.results}">
                	<div id="accountContent" class="col-lg-8 offset-lg-1">
                		<h1><spring:theme code="text.myaccount.recent.order"/></h1>
                		<c:forEach items="${searchPageData.results}" var="order">
                			<div class="order-block">
                				<div class="row">
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
                                   <p class="my-2"><spring:theme code="text.myaccount.order.completed"/></p>
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
                						  <div class="col-6 col-md-3 offset-md-1 text-start text-md-end">
                							<c:if test="${(order.isRentalActive eq true && order.isRentalStartDateActive eq true) || (not empty agent.uid && order.status.code ne 'COMPLETED')}">
                							<c:url value="/my-account/extendRent/${order.code}" var="extendRentAction" />
                							<a href="${extendRentAction}" class="btn btn-primary">
                								<spring:theme code="text.myaccount.order.extend.rent" /> </a>
                					  </c:if>
                					 <!-- <c:if test="${order.isRentalActive eq false}">
                                <p class="my-2"><spring:theme code="text.myaccount.order.completed"/></p>
                             </c:if>-->
                            </div>
                					</c:if>
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
                								 <c:url value="/my-account/order/${order.code}" var="rentOrderAction" />
                                                   <a href="${rentOrderAction}">
                                                        <spring:theme code="text.myaccount.order.rent.again"/> </a>
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
                									<br><spring:theme code="text.myaccount.order"/> </p>
                							</div>
                							<div class="col-8 col-md-10">
                								<p class="body14 gray60">${fn:escapeXml(order.total.formattedValue)}
                									<br> ${fn:escapeXml(order.code)}</p>
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
                				</div>
                			</div>
                		</c:forEach>
                		<div class="account-orderhistory-pagination">
                			<nav:pagination searchPageData="${searchPageData}" searchUrl="${searchUrl}" /> </div>
                	</div>
                	</div>
								</c:if>