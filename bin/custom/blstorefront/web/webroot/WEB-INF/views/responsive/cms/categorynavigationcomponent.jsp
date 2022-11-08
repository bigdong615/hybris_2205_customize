<%@ page trimDirectiveWhitespaces="true"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>
<%@ taglib prefix="cms" uri="http://hybris.com/tld/cmstags"%>

<!-- Mobile Category Navigation Menu -->
<c:if test="${positionAttribute == 'NavigationBarMobileSlot'}">
	<c:if test="${component.visible}">
		<c:forEach items="${component.navigationNode.children}"	var="childLevel1">
            	<c:forEach items="${childLevel1.entries}" var="childlink1">
            		<li><span><i class="icon-${fn:toLowerCase(childlink1.item.category.code)}"></i> ${childlink1.item.linkName}</span>
            			<ul>
		            		<c:if test="${childlink1.item.type  eq 'Link'}">
		            			<c:if test="${childlink1.item.visible}">
			            			<c:if test="${not empty childLevel1.children}">
				            			<c:set var="hasChildren" value="false"/>
				            			<c:forEach items="${childLevel1.children}" var="childLevel2" varStatus="loopStatus">
					            			<c:if test="${not empty childLevel2.children }">
												<c:set var="hasChildren" value="true"/>
											</c:if>
					            			<c:forEach items="${childLevel2.entries}" var="childlink2">							
												<c:url value="${childlink2.item.url}" var="linkNode"/>
												<li><a href="${linkNode }">${childlink2.item.linkName}</a></li> 
											</c:forEach>
											<c:if test="${loopStatus.isLast() and hasChildren}">	
												<c:forEach items="${childLevel1.children}" var="childLevel2" varStatus="childLevel2loopStatus">
													<c:forEach items="${childLevel2.children}" var="childLevel3" varStatus="childLevel3loopStatus">
														<c:forEach items="${childLevel3.entries}" var="childlink3">							
															<c:url value="${childlink3.item.url}" var="linkNode"/>
															<li><a href="${linkNode }">${childlink3.item.linkName}</a></li> 
														</c:forEach>
													</c:forEach>
												</c:forEach>
											</c:if>													
				            			</c:forEach>
			            			</c:if>
		            			</c:if>
		            		</c:if>
            			</ul>
            		</li>
            	</c:forEach>
            </c:forEach>            
	</c:if>
</c:if>

<!-- Category Navigation Menu -->
<c:if test="${positionAttribute == 'NavigationBarSlot'}">
<c:if test="${component.visible}">
	<c:set var="numberOfRows" value="${component.wrapAfter }"/>
	<c:forEach items="${component.navigationNode.children}"	var="childLevel1">
		<c:forEach items="${childLevel1.entries}" var="childlink1">
			<c:if test="${childlink1.item.type  eq 'Link'}">
				<c:url value="${childlink1.item.url}" var="MainCatUrl" />
	           	<c:if test="${childlink1.item.visible}">
					<li class="nav-item dropdown menu-large">

					<c:choose>
						<c:when test="${childlink1.item.linkName eq 'Used Gear'}">
							<a class="nav-link" href="/buy/category/usedgear" id="${fn:toLowerCase(childlink1.item.linkName)}dropdown" aria-expanded="false">${childlink1.item.linkName}</a>
					    </c:when>
					<c:when test="${childlink1.item.linkName eq 'Retail Gear'}">
          <c:if test="${not empty agent.uid}">

 <a class="nav-link dropdown-toggle" href="#" id="${fn:toLowerCase(childlink1.item.linkName)}dropdown" data-bs-toggle="dropdown" aria-expanded="false">${childlink1.item.linkName}</a>
		              	<div class="dropdown-menu megamenu" aria-labelledby="${fn:toLowerCase(childlink1.item.linkName)}dropdown">
							<div class="container">
								<div class="row">
									<div class="col-md-6 offset-md-1 submenu">
										<h5><i class="icon-${fn:toLowerCase(childlink1.item.category.code)}"></i> ${childlink1.item.linkName}</h5>
										<c:if test="${not empty childLevel1.children}">
										<div class="row">
										<c:set var="breakRow" value="0" />
										<c:set var="hasChildren" value="false"/>
										<c:forEach items="${childLevel1.children}" var="childLevel2" varStatus="loopStatus">
											<c:if test="${not empty childLevel2.children }">
												<c:set var="hasChildren" value="true"/>
											</c:if>
											<c:if test="${not empty childLevel2.entries}">
												<c:set var="breakRow" value="${breakRow + 1 }" />
												<c:if test="${breakRow == 1 }">
													<div class="col-md-4">
														<ul>
												</c:if>
															<c:forEach items="${childLevel2.entries}" var="childlink2">
																<c:url value="${childlink2.item.url}" var="linkNode"/>
																<li><a href="${linkNode }">${childlink2.item.linkName}</a></li>
															</c:forEach>
															<c:if test="${breakRow == numberOfRows or loopStatus.isLast()}">
																<c:choose>
																	<c:when test="${loopStatus.isLast() and hasChildren}">
																		<c:set var="breakRow" value="${breakRow}" />
																		<c:forEach items="${childLevel1.children}" var="level2">
																			<c:if test="${not empty level2.children}">
																				<c:forEach items="${level2.children}" var="level3" varStatus="childStatus">
																					<c:if test="${not empty level3.entries}">
																						<c:set var="breakRow" value="${breakRow + 1 }" />
																						<c:if test="${breakRow == 1 }">
																							<div class="col-md-4">
																								<ul>
																						</c:if>
																							<c:forEach items="${level3.entries}" var="level4link">
																								<c:url value="${level4link.item.url}" var="linkNode"/>
																								<li><a href="${linkNode }">${level4link.item.linkName}</a></li>
																							</c:forEach>
																							<c:if test="${breakRow == numberOfRows or childStatus.isLast()}">
																									</ul>
																								</div>
																								<c:set var="breakRow" value="0" />
																							</c:if>
																					</c:if>
																				</c:forEach>
																			</c:if>
																		</c:forEach>
																	</c:when>
																	<c:otherwise>
																			</ul>
																		</div>
																		<c:set var="breakRow" value="0" />
																	</c:otherwise>
																</c:choose>
															</c:if>
														</c:if>
										</c:forEach>
									</div>
								</c:if>
								</div>
								<div class="col-md-4 offset-md-1 my-auto">
					                <cms:pageSlot position="NavigationPromoSlot" var="component" class="">
										<cms:component component="${component}" />
								   </cms:pageSlot>
					            </div>
							</div>
						</div>

          </c:if>

					</c:when>
					<c:otherwise>
		                <a class="nav-link dropdown-toggle" href="#" id="${fn:toLowerCase(childlink1.item.linkName)}dropdown" data-bs-toggle="dropdown" aria-expanded="false">${childlink1.item.linkName}</a>
		              	<div class="dropdown-menu megamenu" aria-labelledby="${fn:toLowerCase(childlink1.item.linkName)}dropdown">
							<div class="container">
								<div class="row">
									<div class="col-md-6 offset-md-1 submenu">
										<h5><i class="icon-${fn:toLowerCase(childlink1.item.category.code)}"></i> ${childlink1.item.linkName}</h5>
										<c:if test="${not empty childLevel1.children}">
										<div class="row">
										<c:set var="breakRow" value="0" />
										<c:set var="hasChildren" value="false"/>
										<c:forEach items="${childLevel1.children}" var="childLevel2" varStatus="loopStatus">
											<c:if test="${not empty childLevel2.children }">
												<c:set var="hasChildren" value="true"/>
											</c:if>
											<c:if test="${not empty childLevel2.entries}">
												<c:set var="breakRow" value="${breakRow + 1 }" />
												<c:if test="${breakRow == 1 }">
													<div class="col-md-4">
														<ul>
												</c:if>
															<c:forEach items="${childLevel2.entries}" var="childlink2">
																<c:url value="${childlink2.item.url}" var="linkNode"/>
																<li><a href="${linkNode }">${childlink2.item.linkName}</a></li>
															</c:forEach>
															<c:if test="${breakRow == numberOfRows or loopStatus.isLast()}">
																<c:choose>
																	<c:when test="${loopStatus.isLast() and hasChildren}">
																		<c:set var="breakRow" value="${breakRow}" />
																		<c:forEach items="${childLevel1.children}" var="level2">
																			<c:if test="${not empty level2.children}">
																				<c:forEach items="${level2.children}" var="level3" varStatus="childStatus">
																					<c:if test="${not empty level3.entries}">
																						<c:set var="breakRow" value="${breakRow + 1 }" />
																						<c:if test="${breakRow == 1 }">
																							<div class="col-md-4">
																								<ul>
																						</c:if>
																							<c:forEach items="${level3.entries}" var="level4link">
																								<c:url value="${level4link.item.url}" var="linkNode"/>
																								<li><a href="${linkNode }">${level4link.item.linkName}</a></li>
																							</c:forEach>
																							<c:if test="${breakRow == numberOfRows or childStatus.isLast()}">
																									</ul>
																								</div>
																								<c:set var="breakRow" value="0" />
																							</c:if>
																					</c:if>
																				</c:forEach>
																			</c:if>
																		</c:forEach>
																	</c:when>
																	<c:otherwise>
																			</ul>
																		</div>
																		<c:set var="breakRow" value="0" />
																	</c:otherwise>
																</c:choose>
															</c:if>
														</c:if>
										</c:forEach>
									</div>
								</c:if>
								</div>
								<div class="col-md-4 offset-md-1 my-auto">
					                <cms:pageSlot position="NavigationPromoSlot" var="component" class="">
										<cms:component component="${component}" />
								   </cms:pageSlot>
					            </div>
							</div>
						</div>
						</c:otherwise>
						</c:choose>
		            </li>
				</c:if>
			</c:if>
		</c:forEach>
	</c:forEach>	
</c:if>
</c:if>