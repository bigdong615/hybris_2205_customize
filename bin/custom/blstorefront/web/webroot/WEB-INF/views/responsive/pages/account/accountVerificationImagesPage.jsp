<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="nav" tagdir="/WEB-INF/tags/responsive/nav"%>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form"%>

<spring:url value="/my-account/uploadDocument" var="uploadDocument" />
<spring:htmlEscape defaultHtmlEscape="true" />
<div id="accountContent" class="col-lg-8 offset-lg-1">
	<h1>Verification Documents</h1>
	<hr>
	<p>For certain orders we need to get to know you better, there is
		an extra step that needs to be completed before we can ship the order
		out. This is done for everyone's protection and to make sure that
		someone is not using your credit card fraudulently or trying to steal
		your identity and run off with a bunch of expensive camera gear. We
		will require the following items:</p>
	<div class="notification notification-tip link mt-4">Max. file
		size is 2mb, JPG's and PDF's work best. Upload your images one at a
		time.</div>
	<h5 class="mt-5">Required Docs</h5>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-license"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b>Your driver's license or state-issued ID</b><br> <span
					class="gray60">Make sure the information is legible.</span>
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">
				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="this.form.submit();"
					id="drivingLicenseUpload" /> Choose File
				</label>
				<input type="hidden" name="documentType" value="DRIVING_LICENSE" />
			</form:form>
		</div>
	</div>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-doc"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b>Your utility bill, phone bill, or car registration</b><br> <span
					class="gray60">Or any other document that clearly lists your
					name and billing address.</span>
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">
				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="this.form.submit();"
					id="utilityBillUpload" /> Choose File
				</label>
				<input type="hidden" name="documentType" value="UTILITY_BILL">
			</form:form>
		</div>
	</div>
	<hr>
	<h5 class="mt-5">Insurance Certificate</h5>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-doc"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b>Your photographer's insurance</b><br> <span class="gray60">Make
					sure it clearly shows coverage for miscellaneous rented equipment
					and it lists <b>Shutterfly, Inc. and its wholly owned
						subsidiary BorrowLenses.com</b>, as ‘loss payee’ and ‘additionally
					insured’.
				</span>
			</p>

			<p class="gray60">
				Our address is: <b>1664 Industrial rd. San Carlos, CA 94070</b>. The
				Insurance will need to provide enough coverage for the retail
				replacement cost of the item(s) you wish to rent.
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">
				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="this.form.submit();"
					id="insuranceCertificateUpload" /> Choose File
				</label>
				<input type="hidden" name="documentType"
					value="INSURANCE_CERTIFICATE">
			</form:form>
		</div>
	</div>
	<hr>