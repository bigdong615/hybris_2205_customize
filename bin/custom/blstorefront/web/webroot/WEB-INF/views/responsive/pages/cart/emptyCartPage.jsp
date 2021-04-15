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
                    <h5>Featured Gear</h5>
                    <div id="gear-slider" class="splide mt-4">
                        <div class="splide__track">
                            <ul class="splide__list">
                                <li class="splide__slide">
                                    <div class="card">
                                        <span class="badge badge-new">New</span>
                                        <span class="bookmark"></span>
                                        <div class="card-slider splide">
                                            <div class="splide__track">
                                                <ul class="splide__list">
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg"></li>
                                                </ul>
                                            </div>
                                        </div>
                                        <p class="overline"><a href="#">Red</a></p>
                                        <h6 class="product"><a href="#">Komodo ST 6K Limited</a></h6>
                                        <h6 class="price">$464 <span class="period">7 day rental</span></h6>
                                        <a href="#" class="btn btn-primary">Add to Rental</a>
                                    </div>
                                </li>
                                <li class="splide__slide">
                                    <div class="card">
                                        <span class="badge badge-limited-stock">Only 2 Left</span>
                                        <span class="bookmark set"></span>
                                        <div class="card-slider splide">
                                            <div class="splide__track">
                                                <ul class="splide__list">
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></li>
                                                </ul>
                                            </div>
                                        </div>
                                        <p class="overline"><a href="#">Sony</a></p>
                                        <h6 class="product"><a href="#">Sony Alpha a7C Mirrorless</a></h6>
                                        <h6 class="price">$78 <span class="period">7 day rental</span></h6>
                                        <a href="#" class="btn btn-primary">Add to Rental</a>
                                    </div>
                                </li>
                                <li class="splide__slide">
                                    <div class="card">
                                        <span class="badge badge-out-of-stock">Out of Stock</span>
                                        <span class="bookmark"></span>
                                        <div class="card-slider splide">
                                            <div class="splide__track">
                                                <ul class="splide__list">
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                                </ul>
                                            </div>
                                        </div>
                                        <p class="overline"><a href="#">Canon</a></p>
                                        <h6 class="product"><a href="#">EF 24-70mm f/2.8L USM II Lens</a></h6>
                                        <h6 class="price">$46.75 <span class="period">7 day rental</span></h6>
                                        <a href="#" class="btn btn-outline btn-disabled">Add to Rental</a>
                                    </div>
                                </li>
                                <li class="splide__slide">
                                    <div class="card">
                                        <span class="badge badge-new">New</span>
                                        <span class="bookmark"></span>
                                        <div class="card-slider splide">
                                            <div class="splide__track">
                                                <ul class="splide__list">
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg"></li>
                                                </ul>
                                            </div>
                                        </div>
                                        <p class="overline"><a href="#">Red</a></p>
                                        <h6 class="product"><a href="#">Komodo ST 6K Limited</a></h6>
                                        <h6 class="price">$464 <span class="period">7 day rental</span></h6>
                                        <a href="#" class="btn btn-primary">Add to Rental</a>
                                    </div>
                                </li>
                                <li class="splide__slide">
                                    <div class="card">
                                        <span class="badge badge-limited-stock">Only 2 Left</span>
                                        <span class="bookmark set"></span>
                                        <div class="card-slider splide">
                                            <div class="splide__track">
                                                <ul class="splide__list">
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></li>
                                                </ul>
                                            </div>
                                        </div>
                                        <p class="overline"><a href="#">Sony</a></p>
                                        <h6 class="product"><a href="#">Sony Alpha a7C Mirrorless</a></h6>
                                        <h6 class="price">$78 <span class="period">7 day rental</span></h6>
                                        <a href="#" class="btn btn-primary">Add to Rental</a>
                                    </div>
                                </li>
                                <li class="splide__slide">
                                    <div class="card">
                                        <span class="badge badge-out-of-stock">Out of Stock</span>
                                        <span class="bookmark"></span>
                                        <div class="card-slider splide">
                                            <div class="splide__track">
                                                <ul class="splide__list">
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                                    <li class="splide__slide"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                                </ul>
                                            </div>
                                        </div>
                                        <p class="overline"><a href="#">Canon</a></p>
                                        <h6 class="product"><a href="#">EF 24-70mm f/2.8L USM II Lens</a></h6>
                                        <h6 class="price">$46.75 <span class="period">7 day rental</span></h6>
                                        <a href="#" class="btn btn-outline btn-disabled">Add to Rental</a>
                                    </div>
                                </li>
                            </ul>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </section>



</template:page>