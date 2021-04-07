<%@ tag body-content="empty" trimDirectiveWhitespaces="true" %>
<%@ attribute name="searchUrl" required="true" %>
<%@ attribute name="searchPageData" required="true"
              type="de.hybris.platform.commerceservices.search.pagedata.SearchPageData" %>
<%@ attribute name="top" required="true" type="java.lang.Boolean" %>
<%@ attribute name="showTopTotals" required="false" type="java.lang.Boolean" %>
<%@ attribute name="supportShowAll" required="true" type="java.lang.Boolean" %>
<%@ attribute name="supportShowPaged" required="true" type="java.lang.Boolean" %>
<%@ attribute name="additionalParams" required="false" type="java.util.HashMap" %>
<%@ attribute name="msgKey" required="false" %>
<%@ attribute name="showCurrentPageInfo" required="false" type="java.lang.Boolean" %>
<%@ attribute name="hideRefineButton" required="false" type="java.lang.Boolean" %>
<%@ attribute name="numberPagesShown" required="false" type="java.lang.Integer" %>
<%@ taglib prefix="pagination" tagdir="/WEB-INF/tags/responsive/nav/pagination" %>
<%@ taglib prefix="product" tagdir="/WEB-INF/tags/responsive/product" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ taglib prefix="ycommerce" uri="http://hybris.com/tld/ycommercetags" %>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<spring:htmlEscape defaultHtmlEscape="true" />

<c:set var="themeMsgKey" value="${not empty msgKey ? msgKey : 'search.page'}"/>


<c:if test="${searchPageData.pagination.totalNumberOfResults > 0}">
   <div class="btn-group">
      <c:if test="${not empty searchPageData.sorts}">
         <form id="sortForm${top ? '1' : '2'}" name="sortForm${top ? '1' : '2'}" method="get" action="#">
            <select id="sortOptions${top ? '1' : '2'}" name="sort" class="form-control">
               <c:forEach items="${searchPageData.sorts}" var="sort">
               <%-- Condition to hide the Relevance sorting option for PLP --%>
                 <c:if test="${pageType == 'CATEGORY' && sort.name ne 'Relevance'}">
                  <option value="${fn:escapeXml(sort.code)}" ${sort.selected? 'selected="selected"' : ''}>
                  <c:choose>
                     <c:when test="${not empty sort.name}">
                        ${fn:escapeXml(sort.name)}
                     </c:when>
                     <c:otherwise>
                        <spring:theme code="${themeMsgKey}.sort.${sort.code}"/>
                     </c:otherwise>
                  </c:choose>
                  </option>
                 </c:if>
                 <%-- Condition added for SLP sorting --%>
                 <c:if test="${pageType == 'PRODUCTSEARCH'}">
                         <option value="${fn:escapeXml(sort.code)}" ${sort.selected? 'selected="selected"' : ''}>
                             <c:choose>
                                 <c:when test="${not empty sort.name}">
                                  ${fn:escapeXml(sort.name)}
                                  </c:when>
                                <c:otherwise>
                                   <spring:theme code="${themeMsgKey}.sort.${sort.code}"/>
                               </c:otherwise>
                             </c:choose>
                         </option>
                 </c:if>
               </c:forEach>
            </select>
            <c:catch var="errorException">
               <spring:eval expression="searchPageData.currentQuery.query"
                  var="dummyVar"/>
               <%-- This will throw an exception is it is not supported --%>
               <!-- searchPageData.currentQuery.query.value is html output encoded in the backend -->
               <input type="hidden" name="q" value="${searchPageData.currentQuery.query.value}"/>
               <c:if test="${pageType == 'PRODUCTSEARCH'}">
               <input type="hidden" name="blPageType" value="${blPageType}"/>
               </c:if>
            </c:catch>
         </form>
      </c:if>
   </div>
</c:if>

