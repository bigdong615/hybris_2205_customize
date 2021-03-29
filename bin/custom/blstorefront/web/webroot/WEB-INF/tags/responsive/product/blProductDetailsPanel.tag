
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>

<section id="globalSearch">
        <div class="container">
            <div class="row justify-content-center">
                <div class="col-12">
                    <div class="input-group">
                        <input type="text" class="d-none d-md-inline-block form-control" placeholder="Search photo &amp; video rentals…">
                        <input type="text" class="d-inline-block d-md-none form-control" placeholder="Search…">
                        <span class="rental-dates d-none d-md-inline"><i class="icon-calendar"></i> Rental Dates</span>
                        <input type="text" id="litepicker" class="form-control d-none d-md-inline-block" placeholder="Select rental dates…">
                        <input type="text" id="mobile-litepicker" class="form-control d-inline-block d-md-none" placeholder="Dates…">
                        <div class="input-group-append d-none d-md-block">
                            <button class="btn btn-search" type="button">Search</button>
                        </div>
                    <div class="litepicker-backdrop"></div></div>
                </div>
            </div>
        </div>
    </section>


    <section id="theProduct">
            <div class="container">
                <div class="row justify-content-center">

                    <div class="col-12">
                     <cms:pageSlot position="BreadcrumbSection" var="feature">
                								<cms:component component="${feature}" />
                						</cms:pageSlot>
                    </div>
                </div>
                <div class="row justify-content-center">
                    <div class="col-12 col-lg-10 col-xl-9">
                        <div class="row">
                            <div id="productImage" class="col-lg-6 text-center">
                                <div id="product-slider" class="splide splide--fade splide--ltr splide--draggable is-active" style="visibility: visible;">
                                    <div class="splide__track" id="product-slider-track">
                                        <ul class="splide__list" id="product-slider-list">
                                            <li class="splide__slide is-active is-visible" id="product-slider-slide01" style="width: 429px; transition: opacity 400ms cubic-bezier(0.42, 0.65, 0.27, 0.99) 0s;" aria-hidden="false" tabindex="0"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></li>
                                            <li class="splide__slide" id="product-slider-slide02" style="width: 429px;"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                            <li class="splide__slide" id="product-slider-slide03" style="width: 429px;"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg"></li>
                                            <li class="splide__slide" id="product-slider-slide04" style="width: 429px;"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                            <li class="splide__slide" id="product-slider-slide05" style="width: 429px;"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg"></li>
                                            <li class="splide__slide" id="product-slider-slide06" style="width: 429px;"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                            <li class="splide__slide" id="product-slider-slide07" style="width: 429px;"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg"></li>
                                            <li class="splide__slide" id="product-slider-slide08" style="width: 429px;"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg"></li>
                                        </ul>
                                    </div>
                                </div>

                                <div id="product-thumbnails" class="splide splide--slide splide--ltr splide--draggable splide--nav is-active" style="visibility: visible;">
                                    <div class="splide__arrows"><button class="splide__arrow splide__arrow--prev" type="button" aria-controls="product-thumbnails-track" aria-label="Go to last slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button><button class="splide__arrow splide__arrow--next" type="button" aria-controls="product-thumbnails-track" aria-label="Next slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button></div><div class="splide__track" id="product-thumbnails-track">
                                        <ul class="splide__list" id="product-thumbnails-list" style="transform: translateX(0px);">
                                            <li class="splide__slide is-active is-visible" id="product-thumbnails-slide01" style="margin-right: 10px; width: 115px; height: 115px; background: rgba(0, 0, 0, 0) url(&quot;https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg&quot;) no-repeat scroll center center / cover;" aria-current="true" aria-hidden="false" tabindex="0" role="button" aria-label="Go to slide 1" aria-controls="product-slider-slide01"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg" style="display: none;"></li>
                                            <li class="splide__slide is-visible" id="product-thumbnails-slide02" style="margin-right: 10px; width: 115px; height: 115px; background: rgba(0, 0, 0, 0) url(&quot;https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg&quot;) no-repeat scroll center center / cover;" aria-hidden="false" tabindex="0" role="button" aria-label="Go to slide 2" aria-controls="product-slider-slide02"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg" style="display: none;"></li>
                                            <li class="splide__slide is-visible" id="product-thumbnails-slide03" style="margin-right: 10px; width: 115px; height: 115px; background: rgba(0, 0, 0, 0) url(&quot;https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg&quot;) no-repeat scroll center center / cover;" aria-hidden="false" tabindex="0" role="button" aria-label="Go to slide 3" aria-controls="product-slider-slide03"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg" style="display: none;"></li>
                                            <li class="splide__slide" id="product-thumbnails-slide04" style="margin-right: 10px; width: 115px; height: 115px; background: rgba(0, 0, 0, 0) url(&quot;https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg&quot;) no-repeat scroll center center / cover;" role="button" aria-label="Go to slide 4" aria-controls="product-slider-slide04"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg" style="display: none;"></li>
                                            <li class="splide__slide" id="product-thumbnails-slide05" style="margin-right: 10px; width: 115px; height: 115px; background: rgba(0, 0, 0, 0) url(&quot;https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg&quot;) no-repeat scroll center center / cover;" role="button" aria-label="Go to slide 5" aria-controls="product-slider-slide05"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/Sony-a7R-IV.jpg" style="display: none;"></li>
                                            <li class="splide__slide" id="product-thumbnails-slide06" style="margin-right: 10px; width: 115px; height: 115px; background: rgba(0, 0, 0, 0) url(&quot;https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg&quot;) no-repeat scroll center center / cover;" role="button" aria-label="Go to slide 6" aria-controls="product-slider-slide06"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg" style="display: none;"></li>
                                            <li class="splide__slide" id="product-thumbnails-slide07" style="margin-right: 10px; width: 115px; height: 115px; background: rgba(0, 0, 0, 0) url(&quot;https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg&quot;) no-repeat scroll center center / cover;" role="button" aria-label="Go to slide 7" aria-controls="product-slider-slide07"><img src="https://clients.veneerstudio.com/borrowlenses/lp/cameras/RED-KOMODO-ST-6K-Limited-Edition.jpg" style="display: none;"></li>
                                            <li class="splide__slide" id="product-thumbnails-slide08" style="margin-right: 10px; width: 115px; height: 115px; background: rgba(0, 0, 0, 0) url(&quot;https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg&quot;) no-repeat scroll center center / cover;" role="button" aria-label="Go to slide 8" aria-controls="product-slider-slide08"><img src="https://clients.veneerstudio.com/borrowlenses/lp/lenses/Canon-USM-II-Lens.jpg" style="display: none;"></li>
                                        </ul>
                                    </div>
                                </div>
                            </div>
                            <div id="productInfo" class="col-lg-5 offset-lg-1">
                                <p class="overline">${product.manufacturer}</p>
                                <h1 class="mb-4">${product.name}</h1>
                                <span class="badge badge-limited-stock">Only 2 Left</span> <div class="stars"><span class="stars-filled" style="width: 80%;"></span><img src="assets/stars-empty.svg"></div> <span class="review-count">(138)</span>
                                <ul class="checklist mt-4">
                                    <li>45MP Full Frame CMOS Sensor</li>
                                    <li>8K30 Row &amp; 4K120 10-Bit Video</li>
                                    <li>50 –&nbsp;102,400 Expanded ISO</li>
                                </ul>
                                <div id="productDates">
                                    <div class="input-group">
                                        <span class="rental-dates d-md-inline"><i class="icon-calendar"></i> Rental Dates</span>
                                        <input type="text" id="product-litepicker" class="form-control d-none d-md-inline-block" placeholder="Select dates…">
                                        <input type="text" id="mobile-product-litepicker" class="form-control d-inline-block d-md-none" placeholder="Dates…">
                                    <div class="litepicker-backdrop"></div></div>
                                </div>
                                <div id="pickupDelivery">
                                    <p><span class="arrival">Get it on Jan 31</span> <a href="#" class="pickupDeliveryLink">Pickup or Delivery</a></p>
                                </div>
                                <div class="priceSummary"><span class="productPrice">$215</span> <span class="rentalDates">7 day rental</span></div>
                                <a href="#" class="btn btn-primary btn-block mt-4 mb-0 mb-md-5">Add to Rental</a>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </section>


        <section id="theProcess">
                <div class="container">
                    <div class="row justify-content-center">
                        <div class="col-lg-11 col-xl-9">
                            <h5>
                           <cms:pageSlot position="HomePageRentingGearIsEasyTitleSlot"
                           							var="feature">
                           							<cms:component component="${feature}" />
                           						</cms:pageSlot>
                            </h5>
                            <div class="row mt-5">
                               <cms:pageSlot position="HomePageRentingGearSectionSlot"
                               							var="feature">
                               							<cms:component component="${feature}" />
                               						</cms:pageSlot>
                            </div>
                        </div>
                    </div>
                </div>
            </section>


            <section id="productExtras">
                    <div class="container">
                        <div class="row justify-content-center">
                            <div class="col-lg-11 col-xl-9">
                                <hr>
                                <a class="filter-expand" data-bs-toggle="collapse" href="#overview" role="button" aria-expanded="true" aria-controls="overview"><h5>Overview</h5></a>
                                <div class="collapse show" id="overview">
                                    <p>The Canon EOS R5 Mirrorless Digital Camera follows the innovative Canon EOS R Mirrorless Digital Camera with a huge expansion of features that customers have requested since the launch of the EOS line in 2018. This next-generation EOS camera continues to take advantage of the high resolving power of Canon's RF-mount lenses while improving a host of in-camera specs.</p>
                                    <div id="overview-slider" class="splide mt-5 splide--slide splide--ltr splide--draggable is-active" style="visibility: visible;">
                                        <div class="splide__arrows"><button class="splide__arrow splide__arrow--prev" type="button" aria-controls="overview-slider-track" disabled="" aria-label="Previous slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button><button class="splide__arrow splide__arrow--next" type="button" aria-controls="overview-slider-track" aria-label="Next slide"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="40" height="40"><path d="m15.5 0.932-4.3 4.38 14.5 14.6-14.5 14.5 4.3 4.4 14.6-14.6 4.4-4.3-4.4-4.4-14.6-14.6z"></path></svg></button></div><div class="splide__track" id="overview-slider-track">
                                            <ul class="splide__list" id="overview-slider-list" style="transform: translateX(0px);">
                                                <li class="splide__slide is-active is-visible" id="overview-slider-slide01" style="margin-right: 30px; width: 415.5px;" aria-hidden="false" tabindex="0">
                                                    <div class="embed-responsive embed-responsive-16by9">
                                                      <iframe class="embed-responsive-item" src="https://www.youtube.com/embed/8jtaappbWtE" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen=""></iframe>
                                                    </div>
                                                    <p class="text-start mt-1">Canon EOS R5 features <span class="gray80 float-end">2:15</span></p>
                                                </li>
                                                <li class="splide__slide is-visible" id="overview-slider-slide02" style="margin-right: 30px; width: 415.5px;" aria-hidden="false" tabindex="0">
                                                    <div class="embed-responsive embed-responsive-16by9">
                                                      <iframe class="embed-responsive-item" src="https://www.youtube.com/embed/4EBEwxWxCK8" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen=""></iframe>
                                                    </div>
                                                    <p class="text-start mt-1">Canon EOS R5 features <span class="gray80 float-end">2:15</span></p>
                                                </li>
                                                <li class="splide__slide" id="overview-slider-slide03" style="margin-right: 30px; width: 415.5px;">
                                                    <div class="embed-responsive embed-responsive-16by9">
                                                      <iframe class="embed-responsive-item" src="https://www.youtube.com/embed/-EZO9RbATDk" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen=""></iframe>
                                                    </div>
                                                    <p class="text-start mt-1">Canon EOS R5 features <span class="gray80 float-end">2:15</span></p>
                                                </li>
                                                <li class="splide__slide" id="overview-slider-slide04" style="margin-right: 30px; width: 415.5px;">
                                                    <div class="embed-responsive embed-responsive-16by9">
                                                      <iframe class="embed-responsive-item" src="https://www.youtube.com/embed/f1peSTA8tRk" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen=""></iframe>
                                                    </div>
                                                    <p class="text-start mt-1">Canon EOS R5 features <span class="gray80 float-end">2:15</span></p>
                                                </li>
                                            </ul>
                                        </div>
                                    <ul class="splide__pagination"><li><button class="splide__pagination__page is-active" type="button" aria-current="true" aria-controls="overview-slider-slide01 overview-slider-slide02" aria-label="Go to page 1"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="overview-slider-slide03 overview-slider-slide04" aria-label="Go to page 2"></button></li></ul></div>
                                </div>
                                <hr>
                                <a class="filter-expand" data-bs-toggle="collapse" href="#specs" role="button" aria-expanded="false" aria-controls="specs"><h5>Specs</h5></a>
                                <div class="collapse" id="specs">
                                    <p>The Canon EOS R5 Mirrorless Digital Camera follows the innovative Canon EOS R Mirrorless Digital Camera with a huge expansion of features that customers have requested since the launch of the EOS line in 2018. This next-generation EOS camera continues to take advantage of the high resolving power of Canon's RF-mount lenses while improving a host of in-camera specs.</p>
                                </div>
                                <hr>
                                <a class="filter-expand" data-bs-toggle="collapse" href="#rental-includes" role="button" aria-expanded="false" aria-controls="includes"><h5>Rental Includes</h5></a>
                                <div class="collapse" id="rental-includes">
                                    <p>The Canon EOS R5 Mirrorless Digital Camera follows the innovative Canon EOS R Mirrorless Digital Camera with a huge expansion of features that customers have requested since the launch of the EOS line in 2018. This next-generation EOS camera continues to take advantage of the high resolving power of Canon's RF-mount lenses while improving a host of in-camera specs.</p>
                                </div>
                                <hr>
                                <a class="filter-expand" data-bs-toggle="collapse" href="#rental-notes" role="button" aria-expanded="false" aria-controls="notes"><h5>Rental Notes</h5></a>
                                <div class="collapse" id="rental-notes">
                                    <p>The Canon EOS R5 Mirrorless Digital Camera follows the innovative Canon EOS R Mirrorless Digital Camera with a huge expansion of features that customers have requested since the launch of the EOS line in 2018. This next-generation EOS camera continues to take advantage of the high resolving power of Canon's RF-mount lenses while improving a host of in-camera specs.</p>
                                </div>
                                <hr>
                                <a class="filter-expand" data-bs-toggle="collapse" href="#manual" role="button" aria-expanded="false" aria-controls="notes"><h5>Manual</h5></a>
                                <div class="collapse" id="manual">
                                    <p>The Canon EOS R5 Mirrorless Digital Camera follows the innovative Canon EOS R Mirrorless Digital Camera with a huge expansion of features that customers have requested since the launch of the EOS line in 2018. This next-generation EOS camera continues to take advantage of the high resolving power of Canon's RF-mount lenses while improving a host of in-camera specs.</p>
                                </div>
                                <hr>
                                <!-- Additional Gear Slider -->
                                <h5>Don't forget</h5>
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
                                                    <h6 class="product">BG-10 Battery Grip</h6>
                                                    <h6 class="price">$44 <span class="period">Jan 31 - Feb 2</span></h6>
                                                    <a href="#" class="btn btn-primary">Add to Rental</a>
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
                                                    <h6 class="product">BG-10 Battery Grip</h6>
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
                                                    <h6 class="product">BG-10 Battery Grip</h6>
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
                                                    <h6 class="product">BG-10 Battery Grip</h6>
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
                                                    <h6 class="product">BG-10 Battery Grip</h6>
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
                                                    <h6 class="product">BG-10 Battery Grip</h6>
                                                    <h6 class="price">$44 <span class="period">Jan 31 - Feb 2</span></h6>
                                                    <a href="#" class="btn btn-primary">Add to Rental</a>
                                                </div>
                                            </li>
                                        </ul>
                                    </div>
                                <ul class="splide__pagination"><li><button class="splide__pagination__page is-active" type="button" aria-current="true" aria-controls="gear-slider-slide01 gear-slider-slide02 gear-slider-slide03 gear-slider-slide04" aria-label="Go to page 1"></button></li><li><button class="splide__pagination__page" type="button" aria-controls="gear-slider-slide03 gear-slider-slide04 gear-slider-slide05 gear-slider-slide06" aria-label="Go to page 2"></button></li></ul></div>
                                <!-- Start Reviews -->
                                <div id="reviews" class="mb-5">
                                    <h5 class="mb-4">Reviews</h5><div class="stars"><span class="stars-filled" style="width: 80%;"></span><img src="assets/stars-empty.svg"></div> <span class="review-count">(138)</span>
                                    <div class="reviewBlock">
                                        <div class="reviewTitle">
                                            <div class="stars"><span class="stars-filled" style="width: 90%;"></span><img src="assets/stars-empty.svg"></div> <b>A superb camera with a remarkable kit lens.</b>
                                            <p class="my-4">I received this camera just over a week ago and have shot about 1000 pictures in various conditions with the kit lens and an RF f/4 24 to 105 mm L series lens I got with the Canon R camera I also own. I mostly shoot JPG and use RAW for those difficult pictures that in camera processing may not produce the best results. The R6 was purchased to replace the R.</p>
                                            <p><b>–&nbsp;Kate</b></p>
                                        </div>
                                    </div>
                                    <div class="reviewBlock">
                                        <div class="reviewTitle">
                                            <div class="stars"><span class="stars-filled" style="width: 80%;"></span><img src="assets/stars-empty.svg"></div> <b>A superb camera with a remarkable kit lens.</b>
                                            <p class="my-4">I received this camera just over a week ago and have shot about 1000 pictures in various conditions with the kit lens and an RF f/4 24 to 105 mm L series lens I got with the Canon R camera I also own. I mostly shoot JPG and use RAW for those difficult pictures that in camera processing may not produce the best results. The R6 was purchased to replace the R.</p>
                                            <p><b>–&nbsp;Kate</b></p>
                                        </div>
                                    </div>
                                    <div class="reviewActions">
                                        <a href="#" class="btn btn-outline"><img src="assets/icon-filter-open.svg"> Write Review</a> <a href="#" class="btn btn-outline">More Reviews</a>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </section>