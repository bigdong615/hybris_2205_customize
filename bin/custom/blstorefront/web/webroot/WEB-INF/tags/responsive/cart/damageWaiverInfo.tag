<%@ tag language="java" pageEncoding="ISO-8859-1"%>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags"%>
<%-- Damage Waivers Modal --%>
    <div class="modal fade" id="damageWaivers" tabindex="-1" aria-hidden="true">
      <div class="modal-dialog modal-dialog-centered modal-lg">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title"><spring:theme code="text.damage.Waiver.model.title"/></h5>
                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
            <div class="modal-body">
                <div class="row mb-4">
                    <div class="text-center col-md-3 col-lg-2">
                        <img src="${themeResourcePath}/assets/gear-guard-plus.png">
                    </div>
                    <div class="col-md-9 col-lg-10">
                        <p><b><spring:theme code="text.damage.Waiver.model.option.pro"/></b></p>
                        <p class="body14"><spring:theme code="text.damage.Waiver.model.option.pro.description"/></p>
                        <hr>
                    </div>
                </div>
                <div class="row mb-4">
                    <div class="text-center col-md-3 col-lg-2">
                        <img src="${themeResourcePath}/assets/gear-guard.png">
                    </div>
                    <div class="col-md-9 col-lg-10">
                        <p><b><spring:theme code="text.damage.Waiver.model.option.gear"/></b></p>
                        <p class="body14"><spring:theme code="text.damage.Waiver.model.option.gear.description"/></p>
                        <hr>
                    </div>
                </div>

                <div class="row">
                    <div class="text-center col-md-3 col-lg-2">
                        <img src="${themeResourcePath}/assets/gear-guard-none.png">
                    </div>
                    <div class="col-md-9 col-lg-10">
                        <p><b><spring:theme code="text.damage.Waiver.model.option"/></b></p>
                        <p class="body14"><spring:theme code="text.damage.Waiver.model.option.description"/></p>
                    </div>
                </div>
            </div>
          </div>
      </div>
  </div>