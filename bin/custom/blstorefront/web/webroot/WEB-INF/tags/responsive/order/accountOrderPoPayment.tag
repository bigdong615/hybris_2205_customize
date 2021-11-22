<%@ tag language="java" pageEncoding="ISO-8859-1"%>
<%@ attribute name="orderData" required="true" type="de.hybris.platform.commercefacades.order.data.OrderData" %>
<%@ attribute name="displayOrderNote" required="false" type="java.lang.Boolean" %>
<%@ attribute name="poNote" required="false" type="java.lang.String" %>

<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

                                   	<div class="col-2 text-center">
                                   		<img
                                   			src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-po.png"
                                   			style="width: 50px;">
                                   	</div>
                                   	<div class="col-10 col-md-5">
                                   		<b class="body14 gray100"><spring:theme code="text.review.page.payment.po" /></b>
                                   		
                                   		<!--Commented below code to resolved BL-1107 -->
                                   		<%-- <div class="row">
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
                                   		</div> --%>
                                   	</div>
                                    <div class="col-12 col-md-5">
                                   	  <div class="po-order-notes">
                                   	    <c:if test="${displayOrderNote == true and not empty orderData.orderNotes}">
                                          <p class="gray80 body14">
                                             <b class="gray100"><spring:theme code="text.review.page.payment.notes"/></b> ${orderData.orderNotes}
                                          </p>
                                        </c:if>
                                   		  <p class="gray80 body14">
                                   			  <b class="gray100"><spring:theme code="text.order.confirmation.print.page.po.notes"/></b>
                                   			  <c:choose>
                                   				  <c:when test="${poNote == ''}">
                                                <spring:theme code="text.review.page.payment.notes.na"/>
                                   				  </c:when>
                                   				  <c:otherwise>
                                               ${poNote}
                                   				  </c:otherwise>
                                   			  </c:choose>
                                   		  </p>
                                      </div>
                                    </div>
