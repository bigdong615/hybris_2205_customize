<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="cartData" required="true" type="de.hybris.platform.commercefacades.order.data.CartData" %>
<%@ attribute name="emptyCart" required="true" type="String" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring"  uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>

<spring:htmlEscape defaultHtmlEscape="true" />
<div id="orderSummary" class="card">
      <h5>
        <spring:theme code="checkout.multi.order.summary"/>
      </h5>
      <hr>
      <p>
        <b><spring:theme code="text.rental.cart.date"/></b>&emsp;
        <input type="text" class="form-control cart-picker" id="summary-litepicker"
            placeholder="<spring:theme code="text.rental.cart.select.date"/>">
        </p>
      <hr>
      <table id="costSummary">
          <tbody>
              <tr>
                  <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.cost"/></td>
                  <td class="text-end"><format:blPrice priceData="${cartData.subTotal}"/></td>
              </tr>
              <tr>
                  <td class="gray80"><spring:theme code="text.cart.damage.waiver"/>
                    <a href="#" data-bs-toggle="modal" data-bs-target="#damageWaivers">
                        <i class="icon-support"></i>
                    </a>
                  </td>
                  <td class="text-end"><format:blPrice priceData="${cartData.totalDamageWaiverCost}"/></td>
              </tr>
              <tr>
                  <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.shipping"/></td>
                  <td class="text-end" id="cart-shipping-cost"><format:blPrice priceData="${cartData.deliveryCost}"/></td>
                  <input type="hidden" class="cart-cost" id="${cartData.deliveryCost.formattedValue}">
              </tr>
              <tr>
                  <td class="gray80"><spring:theme code="text.checkout.multi.order.summary.tax"/></td>
                  <td class="text-end"><format:price priceData="${cartData.taxAvalaraCalculated}"/></td>

              </tr>
              <tr class="total">
                  <td><spring:theme code="basket.page.total"/></td>
                  <td class="text-end"><format:blPrice priceData="${cartData.totalPriceWithTax}"/></td>
              </tr>
          </tbody>
      </table>
      <div class="input-group my-3">
        <input type="text" class="form-control" placeholder="<spring:theme code="text.checkout.multi.order.summary.promocode.placeholder"/>">
        <div class="input-group-append">
          <button class="btn btn-secondary" type="button"><spring:theme code="text.voucher.apply.button.label"/></button>
        </div>
      </div>
      <small class="gray60"><spring:theme code="text.checkout.multi.order.summary.msg"/></small>
</div>
