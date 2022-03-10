<%@ tag body-content="empty" trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product"%>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>

<c:url value="/cart/add" var="addToCartUrl"/>

<h5><spring:theme code= "pdp.rental.product.recommendation.section.text" /></h5>
<div id="gear-slider" class="splide mt-4 splide--slide splide--ltr splide--draggable is-active" style="visibility: visible;">
                                    <div class="splide__arrows"><button class="splide__arrow splide__arrow--prev" type="button" aria-controls="gear-slider-track" aria-label="Go to last slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button><button class="splide__arrow splide__arrow--next" type="button" aria-controls="gear-slider-track" aria-label="Next slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button></div><div class="splide__track" id="gear-slider-track" style="padding-left: 10px; padding-right: 10px;">
                                        <ul class="splide__list" id="gear-slider-list" style="transform: translateX(0px);">
                                            <li class="splide__slide is-active is-visible" id="gear-slider-slide01" style="margin-right: 20px; width: 205.25px;" aria-hidden="false" tabindex="0">
                                                <div class="card">
                                                    <span class="badge badge-new">New</span>
                                                    <span class="bookmark"></span>
                                                    <div class="card-slider splide splide--loop splide--ltr is-active" id="splide01" style="visibility: visible;">
                                                        <div class="splide__arrows"><button class="splide__arrow splide__arrow--prev" type="button" aria-controls="splide01-track" aria-label="Go to last slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button><button class="splide__arrow splide__arrow--next" type="button" aria-controls="splide01-track" aria-label="Next slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button></div><div class="splide__track" id="splide01-track">
                                                            <ul class="splide__list" id="splide01-list" style="transform: translateX(-174px);">
                                                                <li class="splide__slide splide__slide--clone" style="width: 174px;" aria-hidden="true" tabindex="-1"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li><li class="splide__slide is-active is-visible" id="splide01-slide01" style="width: 174px;" aria-hidden="false" tabindex="0"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide01-slide02" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide01-slide03" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                            <li class="splide__slide splide__slide--clone" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li></ul>
                                                        </div>
                                                    <ul class="splide__pagination"><li><button class="splide__pagination__page is-active" type="button" aria-current="true" aria-controls="splide01-slide01" aria-label="Go to slide 1"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide01-slide02" aria-label="Go to slide 2"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide01-slide03" aria-label="Go to slide 3"></button></li></ul></div>
                                                    <p class="overline">Canon</p>
                                                    <h3 class="product">BG-10 Battery Grip</h3>
                                                    <h6 class="price">$44 <span class="period">Jan 31 - Feb 2</span></h6>
                                                    <!-- Form added for BL-456 -->
                                                    <form id="addToCartForm" class="add_to_cart_form" action="${addToCartUrl}" method="post">
                                                       <input type="hidden" maxlength="3" size="1" id="qty" name="qty" class="qty js-qty-selector-input" value="1">
                                                       <input type="hidden" name="productCodePost" value="${product.code}">
                                                       <button id="addToCartButton" type="submit" class="btn btn-primary js-add-to-cart js-enable-btn">
                                                    <spring:theme code= "basket.add.to.rental.cart.button.text" /></button>
                                                    </form>
                                                </div>
                                            </li>
                                            <li class="splide__slide is-visible" id="gear-slider-slide02" style="margin-right: 20px; width: 205.25px;" aria-hidden="false" tabindex="0">
                                                <div class="card">
                                                    <span class="badge badge-new">New</span>
                                                    <span class="bookmark"></span>
                                                    <div class="card-slider splide splide--loop splide--ltr is-active" id="splide02" style="visibility: visible;">
                                                        <div class="splide__arrows"><button class="splide__arrow splide__arrow--prev" type="button" aria-controls="splide02-track" aria-label="Go to last slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button><button class="splide__arrow splide__arrow--next" type="button" aria-controls="splide02-track" aria-label="Next slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button></div><div class="splide__track" id="splide02-track">
                                                            <ul class="splide__list" id="splide02-list" style="transform: translateX(-174px);">
                                                                <li class="splide__slide splide__slide--clone" style="width: 174px;" aria-hidden="true" tabindex="-1"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li><li class="splide__slide is-active is-visible" id="splide02-slide01" style="width: 174px;" aria-hidden="false" tabindex="0"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide02-slide02" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide02-slide03" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                            <li class="splide__slide splide__slide--clone" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li></ul>
                                                        </div>
                                                    <ul class="splide__pagination"><li><button class="splide__pagination__page is-active" type="button" aria-current="true" aria-controls="splide02-slide01" aria-label="Go to slide 1"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide02-slide02" aria-label="Go to slide 2"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide02-slide03" aria-label="Go to slide 3"></button></li></ul></div>
                                                    <p class="overline">Canon</p>
                                                    <h3 class="product">BG-10 Battery Grip</h3>
                                                    <h6 class="price">$44 <span class="period">Jan 31 - Feb 2</span></h6>
                                                    <a href="#" class="btn btn-primary">Add to Rental</a>
                                                </div>
                                            </li>
                                            <li class="splide__slide is-visible" id="gear-slider-slide03" style="margin-right: 20px; width: 205.25px;" aria-hidden="false" tabindex="0">
                                                <div class="card">
                                                    <span class="badge badge-new">New</span>
                                                    <span class="bookmark"></span>
                                                    <div class="card-slider splide splide--loop splide--ltr is-active" id="splide03" style="visibility: visible;">
                                                        <div class="splide__arrows"><button class="splide__arrow splide__arrow--prev" type="button" aria-controls="splide03-track" aria-label="Go to last slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button><button class="splide__arrow splide__arrow--next" type="button" aria-controls="splide03-track" aria-label="Next slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button></div><div class="splide__track" id="splide03-track">
                                                            <ul class="splide__list" id="splide03-list" style="transform: translateX(-174px);">
                                                                <li class="splide__slide splide__slide--clone" style="width: 174px;" aria-hidden="true" tabindex="-1"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li><li class="splide__slide is-active is-visible" id="splide03-slide01" style="width: 174px;" aria-hidden="false" tabindex="0"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide03-slide02" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide03-slide03" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                            <li class="splide__slide splide__slide--clone" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li></ul>
                                                        </div>
                                                    <ul class="splide__pagination"><li><button class="splide__pagination__page is-active" type="button" aria-current="true" aria-controls="splide03-slide01" aria-label="Go to slide 1"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide03-slide02" aria-label="Go to slide 2"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide03-slide03" aria-label="Go to slide 3"></button></li></ul></div>
                                                    <p class="overline">Canon</p>
                                                    <h3 class="product">BG-10 Battery Grip</h3>
                                                    <h6 class="price">$44 <span class="period">Jan 31 - Feb 2</span></h6>
                                                    <a href="#" class="btn btn-primary">Add to Rental</a>
                                                </div>
                                            </li>
                                            <li class="splide__slide is-visible" id="gear-slider-slide04" style="margin-right: 20px; width: 205.25px;" aria-hidden="false" tabindex="0">
                                                <div class="card">
                                                    <span class="badge badge-new">New</span>
                                                    <span class="bookmark"></span>
                                                    <div class="card-slider splide splide--loop splide--ltr is-active" id="splide04" style="visibility: visible;">
                                                        <div class="splide__arrows"><button class="splide__arrow splide__arrow--prev" type="button" aria-controls="splide04-track" aria-label="Go to last slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button><button class="splide__arrow splide__arrow--next" type="button" aria-controls="splide04-track" aria-label="Next slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button></div><div class="splide__track" id="splide04-track">
                                                            <ul class="splide__list" id="splide04-list" style="transform: translateX(-174px);">
                                                                <li class="splide__slide splide__slide--clone" style="width: 174px;" aria-hidden="true" tabindex="-1"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li><li class="splide__slide is-active is-visible" id="splide04-slide01" style="width: 174px;" aria-hidden="false" tabindex="0"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide04-slide02" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide04-slide03" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                            <li class="splide__slide splide__slide--clone" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li></ul>
                                                        </div>
                                                    <ul class="splide__pagination"><li><button class="splide__pagination__page is-active" type="button" aria-current="true" aria-controls="splide04-slide01" aria-label="Go to slide 1"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide04-slide02" aria-label="Go to slide 2"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide04-slide03" aria-label="Go to slide 3"></button></li></ul></div>
                                                    <p class="overline">Canon</p>
                                                    <h3 class="product">BG-10 Battery Grip</h3>
                                                    <h6 class="price">$44 <span class="period">Jan 31 - Feb 2</span></h6>
                                                    <a href="#" class="btn btn-primary">Add to Rental</a>
                                                </div>
                                            </li>
                                            <li class="splide__slide" id="gear-slider-slide05" style="margin-right: 20px; width: 205.25px;">
                                                <div class="card">
                                                    <span class="badge badge-new">New</span>
                                                    <span class="bookmark"></span>
                                                    <div class="card-slider splide splide--loop splide--ltr is-active" id="splide05" style="visibility: visible;">
                                                        <div class="splide__arrows"><button class="splide__arrow splide__arrow--prev" type="button" aria-controls="splide05-track" aria-label="Go to last slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button><button class="splide__arrow splide__arrow--next" type="button" aria-controls="splide05-track" aria-label="Next slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button></div><div class="splide__track" id="splide05-track">
                                                            <ul class="splide__list" id="splide05-list" style="transform: translateX(-174px);">
                                                                <li class="splide__slide splide__slide--clone" style="width: 174px;" aria-hidden="true" tabindex="-1"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li><li class="splide__slide is-active is-visible" id="splide05-slide01" style="width: 174px;" aria-hidden="false" tabindex="0"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide05-slide02" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide05-slide03" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                            <li class="splide__slide splide__slide--clone" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li></ul>
                                                        </div>
                                                    <ul class="splide__pagination"><li><button class="splide__pagination__page is-active" type="button" aria-current="true" aria-controls="splide05-slide01" aria-label="Go to slide 1"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide05-slide02" aria-label="Go to slide 2"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide05-slide03" aria-label="Go to slide 3"></button></li></ul></div>
                                                    <p class="overline">Canon</p>
                                                    <h3 class="product">BG-10 Battery Grip</h3>
                                                    <h6 class="price">$44 <span class="period">Jan 31 - Feb 2</span></h6>
                                                    <a href="#" class="btn btn-primary">Add to Rental</a>
                                                </div>
                                            </li>
                                            <li class="splide__slide" id="gear-slider-slide06" style="margin-right: 20px; width: 205.25px;">
                                                <div class="card">
                                                    <span class="badge badge-new">New</span>
                                                    <span class="bookmark"></span>
                                                    <div class="card-slider splide splide--loop splide--ltr is-active" id="splide06" style="visibility: visible;">
                                                        <div class="splide__arrows"><button class="splide__arrow splide__arrow--prev" type="button" aria-controls="splide06-track" aria-label="Go to last slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button><button class="splide__arrow splide__arrow--next" type="button" aria-controls="splide06-track" aria-label="Next slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button></div><div class="splide__track" id="splide06-track">
                                                            <ul class="splide__list" id="splide06-list" style="transform: translateX(-174px);">
                                                                <li class="splide__slide splide__slide--clone" style="width: 174px;" aria-hidden="true" tabindex="-1"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li><li class="splide__slide is-active is-visible" id="splide06-slide01" style="width: 174px;" aria-hidden="false" tabindex="0"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide06-slide02" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                                <li class="splide__slide" id="splide06-slide03" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li>
                                                            <li class="splide__slide splide__slide--clone" style="width: 174px;"><img src="https://d2ieyhi8galmxj.cloudfront.net/product/MD6d750368-073c-4a5f-a9e9-1d818dac2685.jpg"></li></ul>
                                                        </div>
                                                    <ul class="splide__pagination"><li><button class="splide__pagination__page is-active" type="button" aria-current="true" aria-controls="splide06-slide01" aria-label="Go to slide 1"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide06-slide02" aria-label="Go to slide 2"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="splide06-slide03" aria-label="Go to slide 3"></button></li></ul></div>
                                                    <p class="overline">Canon</p>
                                                    <h3 class="product">BG-10 Battery Grip</h3>
                                                    <h6 class="price">$44 <span class="period">Jan 31 - Feb 2</span></h6>
                                                    <a href="#" class="btn btn-primary">Add to Rental</a>
                                                </div>
                                            </li>
                                        </ul>
                                    </div>