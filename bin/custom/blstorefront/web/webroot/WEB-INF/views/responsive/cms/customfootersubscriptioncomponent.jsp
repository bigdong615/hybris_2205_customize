<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<div class="row">
	<div class="col-12 order-2 order-md-1 scroll-hide">
		<div class="input-group">
			<input type="text" id="emailSubscr_txt" class="form-control"
				placeholder="${feature.placeHolder}">
			<div class="input-group-append">
				<button class="btn btn-submit emailSubscr_btn" type="button"><spring:theme code="text.footer.subscription.button.search"/></button>
				
			</div>
		</div>
	</div>
	<div class="col-12 order-1 order-md-2">
		${feature.footerAddress}
	</div>
</div>