<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>

<h5>${feature.headline}</h5>
  <div class="row mt-5">
      <c:forEach items="${feature.mediaData}" var="medias">
     <div class="col-6 col-md-3 text-center">
            <img src="${medias.url}" />
            <h6>${medias.description}</h6>
     </div>
     </c:forEach>
  </div>