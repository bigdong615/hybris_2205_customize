<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>

<script type="application/ld+json">
{
  "@context": "https://schema.org/",
    "@type": "Product",
	  "name": "${product.name}",<c:forEach items="${galleryImages}" var="container" varStatus="varStatus">
	  "image": 
	       [
	       "${container.product.url}",</c:forEach>
	       ],
	  "brand": {
	    "@type": "Brand",
	    "name": "${product.manufacturer}"
	  },
	  "offers": {
	    "@type": "Offer",
	    <c:choose>
            <c:when test="${IsRentalPage eq 'true' && product.forRent eq 'true'}">
                <c:url var="offerURL" value="${jalosession.tenant.config.getParameter('website.bl.https')}/rent${product.url}"/>
                "url": "${offerURL}",
                "priceCurrency": "USD",
                "price": "${product.price.value}"
            </c:when>
            <c:when test="${IsRentalPage eq 'false' && product.forSale eq 'true' && not empty product.serialproducts}">
                <c:url var="offerURL" value="${jalosession.tenant.config.getParameter('website.bl.https')}/buy${product.url}"/>
                "url": "${offerURL}",
                "priceCurrency": "USD",
                "price": "<fmt:formatNumber type="number" maxFractionDigits="2" value="${product.serialproducts[0].finalSalePrice.value}"/>"
            </c:when>
            <c:otherwise>
                <c:url var="offerURL" value=""/>
                "url": "${offerURL}",
                "priceCurrency": "USD",
                "price": ""
            </c:otherwise>
	    </c:choose>
	  },
    "breadcrumbList": {
  	"@type": "BreadcrumbList",
		  "itemListElement": [{
		    "@type": "ListItem",
		    "position": 1,
		    "name": "Home",
		    "item": "${jalosession.tenant.config.getParameter('website.bl.https')}"
		  },{
		    "@type": "ListItem",
		    "position": 2,
		    "name": "<c:choose><c:when test="${IsRentalPage eq 'true' && product.forRent eq 'true'}">Rental Gear</c:when><c:when test="${IsRentalPage eq 'false' && product.forSale eq 'true'}">Used Gear</c:when><c:otherwise></c:otherwise></c:choose>",
		    "item": "<c:choose><c:when test="${IsRentalPage eq 'true' && product.forRent eq 'true'}">${jalosession.tenant.config.getParameter('website.bl.https')}/rent/category/rentalgear</c:when><c:when test="${IsRentalPage eq 'false' && product.forSale eq 'true'}">${jalosession.tenant.config.getParameter('website.bl.https')}/buy/category/usedgear</c:when><c:otherwise></c:otherwise></c:choose>
		  }
		  <c:if test="${not empty product.categories and not empty breadcrumbs}">,{<c:url var="categoryURL" value="${jalosession.tenant.config.getParameter('website.bl.https')}${breadcrumbs[1].url}"/>
		    "@type": "ListItem",
		    "position": 3,
		    "name": "${breadcrumbs[1].name}",
		    "item": "${categoryURL}"
		  }</c:if>
		  
		  <c:if test="${not empty product.categories and not empty breadcrumbs and fn:length(breadcrumbs) > 3}">,{<c:url var="subCategoryURL" value="${jalosession.tenant.config.getParameter('website.bl.https')}${breadcrumbs[2].url}"/>
		    "@type": "ListItem",
		    "position": 4,
		    "name": "${breadcrumbs[2].name}",
		    "item": "${subCategoryURL}"
		  }
		  </c:if>
  }
}
</script>
