<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="order" required="true" type="de.hybris.platform.commercefacades.order.data.AbstractOrderData"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="theme" tagdir="/WEB-INF/tags/shared/theme" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="grid" tagdir="/WEB-INF/tags/responsive/grid" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="common" tagdir="/WEB-INF/tags/responsive/common" %>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<spring:url value="/cart/voucher/apply" var="applyVoucherAction"/>
<spring:url value="/cart/voucher/remove" var="removeVoucherAction"/>


<spring:theme code="new.checkout.text.gift.card" text="Gift card or discount code" var="giftCard"/>
<div class="row coupon_newcheckout">
    <hr>
    <form class="form-inline" >
        <div class="form-group col-xs-8">
            <input type="text" class="form-control inputCode-d coupan-box-text-align"
                   placeholder="${giftCard}">
        </div>
        <div class="col-xs-4">
            <button type="submit" class="btn btn-primary col-xs-3 coupon-btn-text-align"
                    id="applyCode-d"><spring:theme code="text.voucher.apply.button.label" text="APPLY"/>
            </button>
        </div>
    </form>
</div>

<c:if test="${not empty coupon_applied_msg}">
    <div  class="promotion_message">${coupon_applied_msg}</div>
</c:if>

<div>

<ul id="js-applied-vouchers" class="selected_product_ids clearfix voucher-list">

    <c:forEach items="${cartData1.appliedVouchers}" var="voucher" varStatus="loop">
        <li class="voucher-list__item">
            <form:form id="removeVoucherForm${loop.index}" action="${removeVoucherAction}" method="post"
                       commandName="voucherForm">
                <span class="js-release-voucher voucher-list__item-box" id="voucher-code-${voucher}">
                     ${voucher}
                     <form:input id="${voucher}" value="${voucher}" path="voucherCode"/>
                    <span id="${voucher}"
                          class="glyphicon glyphicon-remove js-release-voucher-remove-btn voucher-list__item-remove"/>
                </span>
            </form:form>
        </li>
    </c:forEach>

    <c:forEach items="${cartData.giftCardData}" var="gift" varStatus="loop">
        <li class="voucher-list__item">
            <form:form id="removeVoucherForm${loop.index}" action="${removeVoucherAction}" method="post"
                       commandName="voucherForm">
                    <span class="js-release-voucher voucher-list__item-box" id="voucher-code-${gift.code}">
                         ${gift.code}
                         <form:input id="${gift.code}" value="${gift.code}" path="voucherCode"/>
                        <span id="${gift.code}"
                              class="glyphicon glyphicon-remove js-release-voucher-remove-btn voucher-list__item-remove"/>
                    </span>
            </form:form>
        </li>
    </c:forEach>
</ul>
