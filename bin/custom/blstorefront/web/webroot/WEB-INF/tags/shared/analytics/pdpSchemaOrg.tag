<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>

<script type="application/ld+json">
{
  "@context": https://schema.org/,
    "@type": "Product",
	  "name": "${product.name}",<c:forEach items="${galleryImages}" var="container" varStatus="varStatus">
	  "image": "${container.product.url}",</c:forEach>
	  "brand": {
	    "@type": "Brand",
	    "name": "${product.manufacturer}"
	  },
	  "offers": {
	    "@type": "Offer",<c:choose><c:when test="${product.forRent}"><c:url var="offerURL" value="/rent${product.url}"/></c:when><c:otherwise><c:url var="offerURL" value="/buy${product.url}"/></c:otherwise></c:choose>
	    "url": "${offerURL}",
	    "priceCurrency": "USD",
	    "price": "${product.price.value}"
	  },
	  "aggregateRating": {
      "@type": "AggregateRating",
      "ratingValue": "<c:choose><c:when test="${product.averageRating > 0}">${product.reviewRating}</c:when><c:otherwise>0</c:otherwise></c:choose>",
      "reviewCount": "<c:choose><c:when test="${product.numberOfReviews > 0}">${product.reviewRating}</c:when><c:otherwise>0</c:otherwise></c:choose>"
    },
    "breadcrumbList": {
  	"@type": "BreadcrumbList",
		  "itemListElement": [{
		    "@type": "ListItem",
		    "position": 1,
		    "name": "Home",
		    "item": https://www.borrowlenses.com/
		  },{
		    "@type": "ListItem",
		    "position": 2,
		    "name": "<c:choose><c:when test="${product.forRent}">Rental Gear</c:when><c:otherwise>Used Gear</c:otherwise></c:choose>",
		    "item": "<c:choose><c:when test="${product.forRent}">https://www.borrowlenses.com/rent/category/rentalgear</c:when><c:otherwise> https://www.borrowlenses.com/rent/category/usedgear</c:otherwise></c:choose>
		  }<c:if test="${not empty product.categories}">,{<c:url var="categoryURL" value="${product.categories[0].url}"/>
		    "@type": "ListItem",
		    "position": 3,
		    "name": "${product.categories[0].name}",
		    "item": "${categoryURL}"
		  }</c:if><c:if test="${not empty product.categories and fn:length(product.categories) > 1}">,{<c:url var="subCategoryURL" value="${product.categories[fn:length(product.categories) - 1].url}"/>
		    "@type": "ListItem",
		    "position": 4,
		    "name": "${product.categories[fn:length(product.categories) - 1].name}",
		    "item": "${subCategoryURL}"
		  }
		  </c:if>
  }
}
</script>
