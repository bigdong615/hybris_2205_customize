<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="checkout" tagdir="/WEB-INF/tags/responsive/checkout/multi" %>

<spring:htmlEscape defaultHtmlEscape="true" />
<div class="row">
    <div class="col-12">
        <p class="overline"><spring:theme code="text.checkout.multi.order.delivery.fastest"/></p>
    </div>
    <div class="col-1 text-center">
        <button class="btn-checkbox" type="button" data-bs-toggle="collapse" data-bs-target="#sameday-expand" aria-controls="sameday-expand"
            aria-expanded="false">
        <input type="radio" id="sameday" name="shipProduct"><label for="sameday"></label>
        </button>
    </div>
    <div class="col-11">
        <b><spring:theme code="text.checkout.multi.order.same.day.delivery"/></b>
        <span class="gray80"><spring:theme code="text.checkout.multi.order.sf.nyc.courier"/></span>
        <div class="collapse" id="sameday-expand" data-bs-parent="#shippingOptions">
            <div class="tab-container">
                <div class="tab-navigation">
                    <select id="same-day-select-box" class="btn btn-block btn-outline text-start mt-4 mb-2" onChange="onChangeOfSameDayShippingMethod()">
                        <c:forEach items="${shippingGroup}" var="entry" varStatus="loop">
                            <c:if test="${entry.shippingType eq 'FASTEST'}">
                                <c:choose>
                                    <c:when test="${entry.defaultShippingGroup}">
                                        <option value="${entry.code}" selected="selected"> ${entry.name} </option>
                                    </c:when>
                                    <c:otherwise>
                                        <option value="${entry.code}"> ${entry.name} </option>
                                    </c:otherwise>
                                </c:choose>
                            </c:if>
                        </c:forEach>
                    </select>
                </div>

                <span class="gray80">
                    <spring:theme code="text.checkout.multi.order.same.day.delivery.to.door"/>
                </span>

                <b class="mt-4"><spring:theme code="text.checkout.multi.order.same.day.delivery.check.eligibility"/></b>
                <div class="input-group mt-2 mb-4">
                    <input type="text" class="form-control" placeholder="Zip code" id="sameDayZipCheckText">
                    <div class="input-group-append">
                        <button class="btn btn-secondary" type="button" onClick="sameDayZipCheck()">
                            <spring:theme code="text.checkout.multi.order.same.day.delivery.check.btn"/>
                        </button>
                    </div>
                </div>

                <div id="sameDayShippingMethods">

                </div>

                <div class="notification notification-warning mb-4" id="sameDayShippingMethodsNotification" style="display:none">
                	<spring:theme code="text.checkout.multi.order.same.day.delivery.notification"/>
                	<ul>
                		<li><spring:theme code="text.checkout.multi.order.same.day.delivery.notification.one"/></li>
                		<li><spring:theme code="text.checkout.multi.order.same.day.delivery.notification.two"/></li>
                		<li><spring:theme code="text.checkout.multi.order.same.day.delivery.notification.three"/></li>
                		<li><spring:theme code="text.checkout.multi.order.same.day.delivery.notification.four"/></li>
                	</ul>
                </div>

                <div id="same-day-address-div">
                    <b><spring:theme code="text.checkout.multi.order.same.day.delivery.add.delivery.notes"/></b>
                    <textarea class="form-control mt-2 mb-4" id="sameDayDeliveryNote" placeholder="Optional (i.e. deliver to back door.)"></textarea>
                    <checkout:addressForm />
                    <div id="same-day-save-address-div">
                        <input type="checkbox" id="same-day-save-address">
                        <label for="same-day-save-address">
                            <span class="gray80"><spring:theme code="text.add.new.shipping.save.address"/></span>
                        </label>
                    </div>
                    <div id="same-day-status-updates-div">
                        <input type="checkbox" id="same-day-status-updates" onChange="onChangeOfStatusUpdate()">
                        <label for="same-day-status-updates">
                            <span class="gray80"><spring:theme code="text.checkout.multi.order.same.day.delivery.get.text.msg.status"/></span>
                        </label>
                    </div>
                </div>
                <div id="same-day-notification"></div>
            </div>
        </div>
    </div>
</div>