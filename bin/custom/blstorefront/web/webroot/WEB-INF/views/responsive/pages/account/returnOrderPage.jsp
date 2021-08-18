<%@ page trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="order" tagdir="/WEB-INF/tags/responsive/order" %>
<%@ taglib prefix="account" tagdir="/WEB-INF/tags/addons/blassistedservicestorefront/order" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<spring:htmlEscape defaultHtmlEscape="true" />
<spring:url value="/my-account/order" var="orderDetailsUrl" htmlEscape="false"/>
<spring:url value="/my-account/returnOrderRequest" var="returnOrderRequest" htmlEscape="false"/>

<c:choose>
<c:when test="${!orderData.hasGiftCart}">
 <div class="col-xl-10">
                    <div class="row">
                        <div id="accountContent" class="col-lg-7">
                        <h1>Return Order</h1>
                            <hr>
                          <c:if test="${orderData.isRentalCart}">
                            
                          </c:if>
                           <div class="reviewCart">
                                <h5 class="mb-4">
                                 <c:if test="${orderData.isRentalCart}">
                                <spring:theme code="text.myaccount.order.your.rental"/>
                                </c:if>
                                <c:if test="${!orderData.isRentalCart}">
                                Your Order
                                </c:if>
                                </h5>
                                <form:form action="${returnOrderRequest}" id="returnOrderForm" modelAttribute="returnOrderForm">
                               
                               <c:forEach items="${orderData.entries}" var="cartEntry" varStatus="loop"> 
                                                            
                               <c:set var="productCode" value ="${cartEntry.product.code}" />
                               
                               		 <div class="row mb-4">
                               		 <c:url var="productUrl" value="/rent/product/${cartEntry.product.code}"/>
                                              <c:if test="${!orderData.isRentalCart}">
                                                <c:url var="productUrl" value="/buy/product/${cartEntry.product.code}"/>
                                              </c:if>

                               				<div class="col-md-3 text-center">
                               					<a href="${productUrl}"> <product:productPrimaryImage product="${cartEntry.product}" format="thumbnail"/> </a>
                               				</div>
                               				<div class="col-md-6 mt-3">
                               					<p class="gray80 body14">
													   <p class="myLabel" style="display: none;"> ${cartEntry.product.code} </p>
                               						<b class="gray100">
                               							<%-- <a href="${productUrl}" style="text-decoration: none"> ${cartEntry.product.name}</a></b> --%>
                               							<a href="${productUrl}" style="text-decoration: none"> ${cartEntry.product.name}</a></b>
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
                               							
                               							<input type="hidden" id="ReturnProdCode" name="prodCode"  class="form-control return-prod-code" value="${cartEntry.product.code}">
                               							
                               							</div> 
														<!-- =================== QTY ======================== -->
														<div class="col-md-3 mt-4">
															<div class="quantity">
											             	    <div class="input-group">
											             		    <span class="input-group-btn">
											             			  <button type="button" class="btn btn-default btn-number"
											             				  data-type="minus" data-field="quant[1]${cartEntry.entryNumber}" entryNumber="${cartEntry.entryNumber}">
											             				  <span class="glyphicon glyphicon-minus"></span>
											             			  </button>
											             		    </span> <input type="text" name="quant[1]${cartEntry.entryNumber}" class="form-control input-number Myqtynumber"
											             			    value="${cartEntry.quantity}" min="1" max="99" entryNumber="${cartEntry.entryNumber}"> <span class="input-group-btn">
											             			    <button type="button" class="btn btn-default btn-number"
											             				    data-type="plus" data-field="quant[1]${cartEntry.entryNumber}" entryNumber="${cartEntry.entryNumber}">
											             				    <span class="glyphicon glyphicon-plus"></span>
											             			    </button>
											             		    </span>
											             	    </div>
										                	</div>
														</div>
												</div>
                                 </c:forEach>
                                 
                                 <form:hidden path="orderCode" value="${orderData.code}" />
                            		<form:hidden path="returnOrderEntries" value="1,2" id="returnOrderEntries${loop.index}"/>                      	
                            	
									<!-- <input type="submit" id="returnOrder" > -->
                            		<button type="submit" id="returnButton" class="return-button-cls btn btn-sm btn-primary float-end">
                            			<spring:theme code="text.myaccount.order.return.order"/>
                            		</button>
                            	</form:form>
                            </div>
                           
                            	
                          	
                        </div>                        
                    </div>
                </div>
</c:when>
<c:otherwise>
 <order:accountGiftCardPurchaseOrderHistoryPage />
</c:otherwise>
</c:choose>