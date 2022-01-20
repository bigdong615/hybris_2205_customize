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
<c:url value="/my-account/removeDocumentEntry" var="removeDocument" />
        <div class="page-loader-new-layout">
          <img src="${themeResourcePath}/assets/bl-loader.gif" alt="Loading.." title="Loading.." id="new_loading_Img"/>
        </div>
<div id="accountContent" class="col-lg-8 offset-lg-1">

	<h1>
		<spring:theme code="bl.verification.document.page.title" />
	</h1>
	<hr>
	<p>
		<spring:theme code="bl.verification.document.page.text" />
	</p>
	<div class="notification notification-tip link mt-4">
		<spring:theme code="bl.verification.document.page.msg" />

	</div>
	<h5 class="mt-5">
		<spring:theme code="bl.verification.document.required.doc" />
	</h5>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-license"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b><spring:theme code="bl.verification.document.license.title" /></b><br>
				<span class="gray60"><spring:theme
						code="bl.verification.document.license.title.msg" /></span>
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">
				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="$('.page-loader-new-layout').show();this.form.submit();"
					id="drivingLicenseUpload" accept=".PDF,.JPG,.JPEG,.PNG,.HEIC" /> <spring:theme code="bl.verification.documents.choose.file"/>
				</label>
				<input type="hidden" name="documentType" value="DRIVING_LICENSE" />
			</form:form>

			<c:forEach items="${UploadedDocument['DRIVING_LICENSE']}"
				var="uploadedDocument" varStatus="loopindex">
				<c:if test="${!uploadedDocument.removedByCustomer}">
				${uploadedDocument.realFileName} <br/> ${uploadedDocument.expiryDate}
				<%--  ${uploadedDocument.code} --%>
				<div class="uploaded-doc">
					<form action="${removeDocument}" method="get"
						id="removeDocumentForm_DRIVING_LICENSE_${loopindex.index}">
						<input type="hidden" name="removeDocumentEntry"
							value="${uploadedDocument.code}"
							id="removeDocumentEntry_${loopindex.index}" /> <a href="#"
							class="lightteal remove-doc" data-bs-toggle="modal"
							data-bs-target="#exampleModal"
							data-code="removeDocumentForm_DRIVING_LICENSE_${loopindex.index}">
							Remove </a>
					</form>

				</div>
				</c:if>
			</c:forEach>
		</div>
		<!-- THIS IS THE CODE SNIPPITS -->
		<!-- modal for remove -->
		<!-- Modal -->
		<div class="modal fade" id="exampleModal" tabindex="-1"
			aria-labelledby="exampleModalLabel" aria-hidden="true">

			<div class="modal-dialog modal-sm">
				<div class="modal-content">
					<div class="modal-header">
						<h5 class="modal-title" id="exampleModalLabel"><spring:theme code="bl.verification.documents.popup.wait" /></h5>
						<button type="button" class="btn-close" data-bs-dismiss="modal"
							aria-label="Close"></button>
					</div>
					<div class="modal-body"><spring:theme code="bl.verification.documents.popup.msg" />
						<p></p>
						<input type="hidden" value="" id="clickedForm">
					
					  
							<a href="#" class="btn btn-primary btn-block my-4" id="remove-doc-submit-button" data-bs-dismiss="modal">Continue</a>
							
					<p class="text-center mb-0"><a href="#"  class="lightteal" aria-label="Close" data-bs-dismiss="modal">Cancel</a></p>		
					</div> 
				</div>
			</div>
		</div>

		<!-- modal for remove -->


	</div>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-doc"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b><spring:theme code="bl.verification.document.utility.title" /></b><br>
				<span class="gray60"><spring:theme
						code="bl.verification.document.utility.title.msg" /></span>
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">

				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="$('.page-loader-new-layout').show();this.form.submit();"
					id="utilityBillUpload" accept=".PDF,.JPG,.JPEG,.PNG,.HEIC"/> <spring:theme code="bl.verification.documents.choose.file"/>
				</label>
				<input type="hidden" name="documentType" value="UTILITY_BILL">
			</form:form>


			<c:forEach items="${UploadedDocument['UTILITY_BILL']}"
				var="uploadedDocument">
				<c:if test="${!uploadedDocument.removedByCustomer}">
		${uploadedDocument.realFileName} <br/> ${uploadedDocument.expiryDate}
				
					<form action="${removeDocument}" method="get"
					id="removeDocumentForm_UTILITY_BILL_${loopindex.index}">
					<input type="hidden" name="${_csrf.parameterName}"
						value="${_csrf.token}" /> <input type="hidden"
						name="removeDocumentEntry" value="${uploadedDocument.code}"
						id="removeDocumentEntry_${loopindex.index}" /> <a href="#"
						class="lightteal remove-doc"
						data-code="removeDocumentForm_UTILITY_BILL_${loopindex.index}"
						data-bs-toggle="modal" data-bs-target="#exampleModal"> Remove
					</a>
				</form>
</c:if>
			</c:forEach>

		</div>
	</div>
	<hr>
	<h5 class="mt-5">
		<spring:theme code="bl.verification.document.Insurance.Certificate" />
	</h5>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-doc"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b><spring:theme
						code="bl.verification.document.Insurance.Certificate.title" /></b><br>
				<span class="gray60"><spring:theme
						code="bl.verification.document.Insurance.Certificate.title.msg" />
				</span>
			</p>

			<p class="gray60">
				<spring:theme
					code="bl.verification.document.Insurance.Certificate.title.second.msg" />
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">

				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="$('.page-loader-new-layout').show();this.form.submit();"
					id="insuranceCertificateUpload" accept=".PDF,.JPG,.JPEG,.PNG,.HEIC"/> <spring:theme code="bl.verification.documents.choose.file"/>
				</label>
				<input type="hidden" name="documentType"
					value="INSURANCE_CERTIFICATE">
			</form:form>


			<c:forEach items="${UploadedDocument['INSURANCE_CERTIFICATE']}"
				var="uploadedDocument" varStatus="loopindex">
				<c:if test="${!uploadedDocument.removedByCustomer}">
				<div class="uploaded-doc">
		${uploadedDocument.realFileName} <br/> ${uploadedDocument.expiryDate}
					<%-- <fmt:formatDate value="${uploadedDocument.expiryDate}" pattern="dd-MM-yy" /> --%>
					 <input type="hidden" class="existing-doc">
				</div>

				<form action="${removeDocument}" method="get"
					id="removeDocumentForm_INSURANCE_CERTIFICATE_${loopindex.index}">
					<input type="hidden" name="${_csrf.parameterName}"
						value="${_csrf.token}" /> <input type="hidden"
						name="removeDocumentEntry" value="${uploadedDocument.code}"
						id="removeDocumentEntry_${loopindex.index}" /><a href="#"
						class="lightteal remove-doc" data-bs-toggle="modal"
						data-bs-target="#exampleModal"
						data-code="removeDocumentForm_INSURANCE_CERTIFICATE_${loopindex.index}">
						Remove </a>
				</form>
				</c:if>
			</c:forEach>


		</div>
	</div>
	<hr>

	<h5 class="mt-5">
		<spring:theme code="bl.verification.document.additional.doc" />
	</h5>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-doc"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b><spring:theme code="bl.verification.document.first" /></b><br>
				<span class="gray60"><spring:theme
						code="bl.verification.documents.message" /></span>
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">

				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="$('.page-loader-new-layout').show();this.form.submit();"
					id="insuranceCertificateUpload" accept=".PDF,.JPG,.JPEG,.PNG,.HEIC"/> <spring:theme code="bl.verification.documents.choose.file"/>
				</label>
				<input type="hidden" name="documentType" value="EXTRA_DOCUMENT1">
			</form:form>

			<c:forEach items="${UploadedDocument['EXTRA_DOCUMENT1']}"
				var="uploadedDocument" varStatus="loopindex">
				<c:if test="${!uploadedDocument.removedByCustomer}">
				<div class="uploaded-doc">
			${uploadedDocument.realFileName} <br/> ${uploadedDocument.expiryDate}
			<input type="hidden" class="existing-doc">
				</div>

				<form action="${removeDocument}" method="get"
					id="removeDocumentForm_EXTRA_DOCUMENT1_${loopindex.index}">
					<input type="hidden" name="${_csrf.parameterName}"
						value="${_csrf.token}" /> <input type="hidden"
						name="removeDocumentEntry" value="${uploadedDocument.code}"
						id="removeDocumentEntry_${loopindex.index}" /> <a href="#"
						class="lightteal remove-document" data-bs-toggle="modal"
						data-bs-target="#exampleModalDocument"
						data-code="removeDocumentForm_EXTRA_DOCUMENT1_${loopindex.index}">
						Remove </a>
				</form>
				</c:if>
			</c:forEach>

		</div>
	</div>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-doc"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b><spring:theme code="bl.verification.document.second" /></b><br>
				<span class="gray60"><spring:theme
						code="bl.verification.documents.message" /></span>
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">

				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="$('.page-loader-new-layout').show();this.form.submit();"
					id="insuranceCertificateUpload" accept=".PDF,.JPG,.JPEG,.PNG,.HEIC"/> <spring:theme code="bl.verification.documents.choose.file"/>
				</label>
				<input type="hidden" name="documentType" value="EXTRA_DOCUMENT2">
			</form:form>


			<c:forEach items="${UploadedDocument['EXTRA_DOCUMENT2']}"
				var="uploadedDocument" varStatus="loopindex">
				<c:if test="${!uploadedDocument.removedByCustomer}">
				<div class="uploaded-doc">
		${uploadedDocument.realFileName} <br/> ${uploadedDocument.expiryDate}
		<input type="hidden" class="existing-doc">
				</div>

				<form action="${removeDocument}" method="get"
					id="removeDocumentForm_EXTRA_DOCUMENT2_${loopindex.index}">
					<input type="hidden" name="${_csrf.parameterName}"
						value="${_csrf.token}" /> <input type="hidden"
						name="removeDocumentEntry" value="${uploadedDocument.code}"
						id="removeDocumentEntry_${loopindex.index}" /> <a href="#"
						class="lightteal remove-document"
						data-code="removeDocumentForm_EXTRA_DOCUMENT2_${loopindex.index}"
						data-bs-toggle="modal" data-bs-target="#exampleModalDocument"> Remove
					</a>
				</form>
				</c:if>
			</c:forEach>


		</div>
	</div>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-doc"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b><spring:theme code="bl.verification.document.third" /></b><br>
				<span class="gray60"><spring:theme
						code="bl.verification.documents.message" /></span>
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">

			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">

				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="$('.page-loader-new-layout').show();this.form.submit();"
					id="insuranceCertificateUpload" accept=".PDF,.JPG,.JPEG,.PNG,.HEIC"/> <spring:theme code="bl.verification.documents.choose.file"/>
				</label>
				<input type="hidden" name="documentType" value="EXTRA_DOCUMENT3">
			</form:form>

			<c:forEach items="${UploadedDocument['EXTRA_DOCUMENT3']}"
				var="uploadedDocument" varStatus="loopindex">
				<c:if test="${!uploadedDocument.removedByCustomer}">
				<div class="uploaded-doc">
			${uploadedDocument.realFileName} <br/> ${uploadedDocument.expiryDate}
			<input type="hidden" class="existing-doc">
				</div>

				<form action="${removeDocument}" method="get"
					id="removeDocumentForm_EXTRA_DOCUMENT3_${loopindex.index}">
					<input type="hidden" name="${_csrf.parameterName}"
						value="${_csrf.token}" /> <input type="hidden"
						name="removeDocumentEntry" value="${uploadedDocument.code}"
						id="removeDocumentEntry_${loopindex.index}" /> <a href="#"
						class="lightteal remove-document" data-bs-toggle="modal"
						data-code="removeDocumentForm_EXTRA_DOCUMENT3_${loopindex.index}"
						data-bs-target="#exampleModalDocument"> Remove </a>
				</form></c:if>
			</c:forEach>
		</div>
	</div>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-doc"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b><spring:theme code="bl.verification.document.fourth" /></b><br>
				<span class="gray60"><spring:theme
						code="bl.verification.documents.message" /></span>
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">

				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="$('.page-loader-new-layout').show();this.form.submit();"
					id="insuranceCertificateUpload" accept=".PDF,.JPG,.JPEG,.PNG,.HEIC"/> <spring:theme code="bl.verification.documents.choose.file"/>
				</label>
				<input type="hidden" name="documentType" value="EXTRA_DOCUMENT4">
			</form:form>

			<c:forEach items="${UploadedDocument['EXTRA_DOCUMENT4']}"
				var="uploadedDocument" varStatus="loopindex">
				<c:if test="${!uploadedDocument.removedByCustomer}">
				<div class="uploaded-doc">
		     ${uploadedDocument.realFileName} <br/> ${uploadedDocument.expiryDate}
		     <input type="hidden" class="existing-doc">
				</div>

				<form action="${removeDocument}" method="get"
					id="removeDocumentForm_EXTRA_DOCUMENT4_${loopindex.index}">
					<input type="hidden" name="${_csrf.parameterName}"
						value="${_csrf.token}" /> <input type="hidden"
						name="removeDocumentEntry" value="${uploadedDocument.code}"
						id="removeDocumentEntry_${loopindex.index}" /> <a href="#"
						class="lightteal remove-document"
						data-code="removeDocumentForm_EXTRA_DOCUMENT4_${loopindex.index}"
						data-bs-toggle="modal" data-bs-target="#exampleModalDocument"> Remove
					</a>
				</form></c:if>
			</c:forEach>

		</div>
	</div>
	<div class="file-upload row mt-4">
		<div class="col-lg-1">
			<i class="icon-doc"></i>
		</div>
		<div class="col-lg-7">
			<p>
				<b><spring:theme code="bl.verification.document.fifth" /></b><br>
				<span class="gray60"><spring:theme
						code="bl.verification.documents.message" /></span>
			</p>
		</div>
		<div class="col-lg-4 text-lg-end">
			<form:form id="documentUploadForm"
				action="${uploadDocument}?${CSRFToken.parameterName}=${CSRFToken.token}"
				method="post" modelAttribute="verificationDocumentForm"
				enctype="multipart/form-data">

				<label class="custom-file-upload"> <input type="file"
					name="document" onchange="$('.page-loader-new-layout').show();this.form.submit();"
					id="insuranceCertificateUpload" accept=".PDF,.JPG,.JPEG,.PNG,.HEIC"/> <spring:theme code="bl.verification.documents.choose.file"/>
				</label>
				<input type="hidden" name="documentType" value="EXTRA_DOCUMENT5">
			</form:form>

			<c:forEach items="${UploadedDocument['EXTRA_DOCUMENT5']}"
				var="uploadedDocument" varStatus="loopindex">
				<c:if test="${!uploadedDocument.removedByCustomer}">
				<div class="uploaded-doc">
	    	${uploadedDocument.realFileName} <br/> ${uploadedDocument.expiryDate}
	    	<input type="hidden" class="existing-doc">
				</div>

				<form action="${removeDocument}" method="get"
					id="removeDocumentForm_EXTRA_DOCUMENT5_${loopindex.index}">
					<input type="hidden" name="${_csrf.parameterName}"
						value="${_csrf.token}" /> <input type="hidden"
						name="removeDocumentEntry" value="${uploadedDocument.code}"
						id="removeDocumentEntry_${loopindex.index}" /> <a href="#"
						class="lightteal remove-document"
						data-code="removeDocumentForm_EXTRA_DOCUMENT5_${loopindex.index}"
						data-bs-toggle="modal" data-bs-target="#exampleModalDocument"> Remove
					</a>
				</form></c:if>
			</c:forEach>

		</div>
	</div>
	
	<!-- THIS IS THE CODE SNIPPITS -->
		<!-- modal for remove -->
		<!-- Modal -->
		<div class="modal fade" id="exampleModalDocument" tabindex="-1"
			aria-labelledby="exampleModalLabel" aria-hidden="true">

			<div class="modal-dialog modal-sm">
				<div class="modal-content">
					<div class="modal-header">
						<h5 class="modal-title" id="exampleModalLabel"><spring:theme code="bl.verification.documents.popup.title" /></h5>
						<button type="button" class="btn-close" data-bs-dismiss="modal"
							aria-label="Close"></button>
					</div>
					<div class="modal-body"><spring:theme code="bl.verification.documents.popup.additional" />
						<p></p>
						<input type="hidden" value="" id="clickedFormDocument">
					
					  
							<a href="#" class="btn btn-primary btn-block my-4" id="remove-document-submit-button" data-bs-dismiss="modal"><spring:theme code="bl.verification.documents.popup.continue"/></a>
							
					<p class="text-center mb-0"><a href="#"  class="lightteal" aria-label="Close" data-bs-dismiss="modal"><spring:theme code="bl.verification.documents.popup.cancel"/></a></p>		
					</div> 
				</div>
			</div>
		</div>

		<!-- modal for remove -->
<div class="modal fade" id="editWarning" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-sm">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title"><spring:theme code="bl.verification.documents.popup.wait" /></h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
          <div class="modal-body">
          		<input type="hidden" id="enablePopup" value="${enablePopup}">
          		<c:if test="${not empty fileFormatError }">
          			<p class="body14"><spring:theme code="${fileFormatError }" /></p>
          		</c:if>
              	<c:if test="${not empty fileSizeError }">
          			<p class="body14"><spring:theme code="${fileSizeError }" /></p>
          		</c:if>
              <p class="body14"><spring:theme code="${code }" /></p>
              <p class="text-center mb-0"><a href="#" class="lightteal" data-bs-dismiss="modal" aria-label="Close">
              <spring:theme code="bl.verification.documents.popup.cancel"/></a></p>
          </div>
        </div>
      </div>
    </div>