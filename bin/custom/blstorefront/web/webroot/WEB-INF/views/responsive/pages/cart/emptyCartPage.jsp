<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%@ taglib prefix="template" tagdir="/WEB-INF/tags/responsive/template"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="cart" tagdir="/WEB-INF/tags/responsive/cart" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<template:page pageTitle="${pageTitle}">

<div class="screen"></div>
    <section id="confirmationWindow">
        <div class="container">
            <div class="row justify-content-center">
                <div class="col-12 col-lg-11 col-xl-10">
                    <div id="orderConfirmation" class="text-center">
                        <h1>Your cart is empty</h1>
                        <div class="confirmation-actions my-4">
                            <a href="/blstorefront/bl/en/" class="btn btn-outline mx-3">Continue Shopping</a>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </section>

    <section>
        <div class="container">
            <div id="featured" class="row justify-content-center mt-5">
                <div class="col-lg-11 col-xl-9">
                    <cms:pageSlot position="HomePageFeaturedGearSectionSlot" var="feature">
                    		<cms:component component="${feature}" />
                    </cms:pageSlot>
                </div>
            </div>
        </div>
    </section>

</template:page>