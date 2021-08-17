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
                                                 <format:blPrice priceData="${extendOrder.extendOrderCost}"/>
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
                               				<div class="col-md-9 mt-3">
                               					<p class="gray80 body14">
                               						<b class="gray100 myLabel">
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
                               							
                               							<!-- =================== QTY ======================== -->
                               							<div class="quantity">
             	    										<div class="input-group">
             		    										<span class="input-group-btn">
             			  											<button type="button" class="btn btn-default btn-number" data-type="minus" data-field="quant[1]${cartEntry.entryNumber}">
             				  											<span class="glyphicon glyphicon-minus"></span>
             			  											</button>
             		    										</span> 
             		    										
             		    									<input type="text" id="Myqty" name="titles"  class="form-control input-number" value="1" min="0" max="99"> 
             		    									
             		    									<span class="input-group-btn">
             			    									<button type="button" class="btn btn-default btn-number" data-type="plus" data-field="quant[1]${cartEntry.entryNumber}">
             				    									<span class="glyphicon glyphicon-plus"></span>
             			    									</button>
             		    									</span>
             	    										</div>
             	    										
                										</div>				</div>  	</div>
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