<script type="application/ld+json">
{
  "@context": https://schema.org/,
    "@type": "Product",
	  "name": "${product.name}",
	  "image": "${imagePath}",
	  "brand": {
	    "@type": "Brand",
	    "name": "${product.manufacturer}"
	  },
	  "offers": {
	    "@type": "Offer",
	    "url": "${c:url}",
	    "priceCurrency": "USD",
	    "price": "<product:productListerItemPrice product="${product}"/>"
	  },
	  "aggregateRating": {
      "@type": "AggregateRating",
      "ratingValue": 4.4,
      "reviewCount": 89
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
		    "name": "Rental Gear",
		    "item": https://www.borrowlenses.com/rent/category/rentalgear
		  },{
		    "@type": "ListItem",
		    "position": 3,
		    "name": "Production",
		    "item": https://www.borrowlenses.com/rent/category/rentalgear/production
		  },{
		    "@type": "ListItem",
		    "position": 4,
		    "name": "Projectors & Scanners",
		    "item": https://www.borrowlenses.com/rent/category/production/projectors-scanners
		  }
  }
}
</script>
