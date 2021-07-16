package com.bl.storefront.controllers.pages;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@RequestMapping("/extendOrder")
public class ExtendOrderController {

  protected static final Logger LOG = Logger.getLogger(ExtendOrderController.class);

  /**
   * To place the extend rental order
   */

  @PostMapping(value = "/placeOrder")
  public void placeExtendOrder(	@RequestParam(value = "orderCode", defaultValue = "") final String orderCode ,
      final HttpServletRequest request, final HttpServletResponse response, final Model model,
      final RedirectAttributes redirectModel) {
  }



}
