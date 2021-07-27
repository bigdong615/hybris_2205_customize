<%@ attribute name="orderData" required="true" type="de.hybris.platform.commercefacades.order.data.OrderData" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="format" tagdir="/WEB-INF/tags/shared/format"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>

<div class="reviewCart">
	<h5 class="mb-4">
		<spring:theme code="text.gift.review.page" text="Gift Card" />
	</h5>
	<c:forEach items="${orderData.giftCardData}" var="gift">
		<div class="row">
			<div class="col-2 text-center">
				<img
					src="${request.contextPath}/_ui/responsive/theme-bltheme/assets/payment-giftcard.png"
					style="width: 50px;">
			</div>
			<div class="col-5">
				<b class="body14 gray100"><spring:theme
						code="text.gift.bl.review.page" text="BL Gift Card" /></b>
				<div class="row">
					<div class="col-6">
						<p class="body14">
							<spring:theme code="text.gift.card.review.page" text="Card #" />
							<br>
							<spring:theme code="text.gift.applied.review.page" text="Applied" />
							<br>
							<spring:theme code="text.gift.balance.review.page" text="Balance" />
						</p>
					</div>
					<div class="col-6">
						<p class="body14 gray80">
						  <c:set var="redeemedAmount"><format:price priceData="${gift.redeemamount}"/></c:set>
							${gift.code}<br> ${fn:replace(redeemedAmount, "-", "")}<br>
							<format:price priceData="${gift.balanceamount}"/>
						</p>
					</div>
				</div>
			</div>
		</div>
	</c:forEach>
</div>