<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="order" required="true" type="de.hybris.platform.commercefacades.order.data.OrderData" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="grid" tagdir="/WEB-INF/tags/responsive/grid" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="ordercancel" tagdir="/WEB-INF/tags/addons/blorderselfserviceaddon/responsive/order/cancel" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<form:form action="${request.contextPath}/my-account/order/${fn:escapeXml(order.code)}/cancel/cancelconfirmation"
           id="confirmcancelorderForm"
           modelAttribute="orderEntryCancelForm"
           class="account-cancel-order-form">


    <ordercancel:cancelCompleteOrder/>

    <ul class="item__list cart__list cancel-order__list">

        <li class="hidden-xs hidden-sm">
            <ul class="item__list--header">
                <li class="item__image"></li>
                <li class="item__info"><spring:theme code="basket.page.item"/></li>
                <li class="item__delivery"><spring:theme code="text.account.cancel.delivery"/></li>
                <li class="item__price"><spring:theme code="basket.page.price"/></li>
                <li class="item__quantity"><spring:theme code="basket.page.qty"/></li>
            </ul>
        </li>

        <c:forEach items="${order.entries}" var="entry" varStatus="loop">
            <c:if test="${entry.entryNumber != -1 && entry.cancellableQty > 0 }">
                <c:if test="${not empty entry.statusSummaryMap}">
                    <c:set var="errorCount" value="${entry.statusSummaryMap.get(errorStatus)}"/>
                    <c:if test="${not empty errorCount && errorCount > 0}">
                        <div class="notification has-error">
                            <spring:theme code="basket.error.invalid.configuration" arguments="${errorCount}"/>
                            <spring:theme code="basket.error.invalid.configuration.edit"/>
                        </div>
                    </c:if>
                </c:if>
                <li class="item__list--item">
                        <%-- chevron for multi-d products --%>
                    <div class="hidden-xs hidden-sm item__toggle">
                        <c:if test="${entry.product.multidimensional}">
                            <div class="js-show-editable-grid" data-index="${loop.index}"
                                 data-read-only-multid-grid="${not entry.updateable}">
                                <ycommerce:testId code="cancel_product_updateQuantity">
                                    <span class="glyphicon glyphicon-chevron-down"></span>
                                </ycommerce:testId>

                            </div>
                        </c:if>
                    </div>

                        <%-- product image --%>
                    <div class="item__image">
                        <product:productPrimaryImage product="${entry.product}" format="thumbnail"/>
                    </div>

                        <%-- product name, code, promotions --%>
                    <div class="item__info">
                        <ycommerce:testId code="cancel_product_name">
                            <span class="item__name">${fn:escapeXml(entry.product.name)}</span>
                        </ycommerce:testId>

                        <div class="item__code">${fn:escapeXml(entry.product.code)}</div>

                        <c:if test="${fn:escapeXml(entry.product.configurable)}">
                            <div class="item__configurations">
                                <c:forEach var="config" items="${entry.configurationInfos}">
                                    <c:set var="style" value=""/>
                                    <c:if test="${config.status eq errorStatus}">
                                        <c:set var="style" value="color:red"/>
                                    </c:if>
                                    <div class="item__configuration-entry row" style="${style}">
                                        <div class="item__configuration-name col-md-4">${fn:escapeXml(config.configurationLabel)}:</div>
                                        <div class="item__configuration-value col-md-8">${fn:escapeXml(config.configurationValue)}</div>
                                    </div>
                                </c:forEach>
                            </div>
                            <c:if test="${not empty entry.configurationInfos}">
                                <div class="item__configurations-edit">
                                    <spring:theme code="basket.page.change.configuration"/>
                                </div>
                            </c:if>
                        </c:if>
                    </div>
                        <%-- delivery mode --%>
                    <div class="item__delivery">
                        <span class="visible-xs visible-sm"><spring:theme code="text.account.cancel.delivery"/>: </span>
                        <c:choose>
                            <c:when test="${entry.deliveryPointOfService eq null}">
                                <spring:theme code="text.account.cancel.standard.shipping"/>
                            </c:when>
                            <c:otherwise>
                                        <span><spring:theme
                                                code="text.account.cancel.pickup.from"/><span></span>${fn:escapeXml(entry.deliveryPointOfService.name)}</span>
                            </c:otherwise>
                        </c:choose>
                    </div>
                        <%-- price --%>
                    <div class="item__price">
                        <span class="visible-xs visible-sm"><spring:theme code="basket.page.itemPrice"/>: </span>
                        <format:price priceData="${entry.basePrice}" displayFreeForZero="true"/>
                    </div>

                    

                    <div class="item__quantity visible-xs visible-sm">
                        <div class="item__quantity-wrapper">
                            <label class="visible-xs visible-sm"><spring:theme code="basket.page.qty"/>:</label>
                            <span class="qtyValue"><c:out value="${entry.cancellableQty}"/></span>
                            <input type="hidden" id="item_quantity_${entry.entryNumber}" value="${entry.cancellableQty}"/>
                        </div>
                    </div>

                    <!-- desktop -->
                    <div class="item__quantity hidden-xs hidden-sm">
                        <div class="js-qty-form${loop.index}" action="">
                            <c:set var="key" value="${entry.entryNumber}"/>
                            
                        </div>
                    </div>

                    <!-- mobile -->
                    <div class="item__quantity visible-xs visible-sm">
                        <div class="item__quantity-wrapper">
                            <div class="js-qty-form${loop.index}" action="">
                                <label class="visible-xs visible-sm item__quantity__input-label" path="quantity"
                                       for="quantity${entry.entryNumber}"> <spring:theme code="text.account.cancel.qty"/></label>
                                <c:set var="key" value="${entry.entryNumber}"/>
                                <form:input class="form-control js-update-entry-quantity-input" type="text" size="1"
                                            path="cancelEntryQuantityMap[${key}]"/>
                            </div>
                        </div>
                    </div>
                </li>
                <li>
                    <div class="add-to-cart-order-form-wrap display-none"></div>
                </li>
            </c:if>
           
        </c:forEach>

    </ul>

    <ordercancel:cancelCompleteOrder/>

    <!--Button begins-->
    <div class="row">
        <div class="col-md-6 col-md-offset-6">
            <div class="cancel-actions">
                <div class="row">
                    <div class="col-sm-6 col-sm-offset-6">

                        <button type="submit" class="btn btn-primary btn-block" id="cancelOrderButtonConfirmation"
                                disabled="disabled">
                            <spring:theme code="text.account.confirm.cancelOrder"/>
                        </button>


                    </div>
                </div>
            </div>
        </div>
    </div>

</form:form>
