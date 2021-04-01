<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<template:page pageTitle="${pageTitle}">
   <c:choose>
       <c:when test="${product.forRent}">
              <product:blRentalProductDetailsPanel />
      </c:when>
      <c:when test="${product.forSale}">
         <product:blUsedProductDetailsPanel/>
      </c:when>
   </c:choose>
</template:page>