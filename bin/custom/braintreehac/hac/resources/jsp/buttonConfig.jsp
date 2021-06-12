<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<% pageContext.setAttribute("newLineChar", "\n"); %>

<html>
<head>
    <title>PayPal Button Config</title>

</head>
<body>
<div class="marginLeft prepend-top span-17">

    <h2>${pageLabel}</h2>

    <div class="dataTable no-footer">
        <form action="/hac/braintreehac/button/config/saveAll" method="GET">
            <div id="colorContainer">
                <label for="color"> Choose a color:</label>
                <select name="color" id="color">
                    <c:forEach var="item" items="${color}">
                        <option value="${item}" ${item == payPalComponent.color ? 'selected="selected"' : ''}>${item}</option>
                    </c:forEach>
                </select>
            </div>
            <div id="shapeContainer">
                <label for="shape"> Choose a shape:</label>
                <select name="shape" id="shape">
                    <c:forEach var="item" items="${shape}">
                        <option value="${item}" ${item == payPalComponent.shape ? 'selected="selected"' : ''}>${item}</option>
                    </c:forEach>
                </select>
            </div>
            <div id="heightContainer">
                <label for="height"> Choose a height:</label>
                <select name="height" id="height">
                    <c:forEach var="item" items="${height}">
                        <option value="${item}" ${item == payPalComponent.height ? 'selected="selected"' : ''}>${item}</option>
                    </c:forEach>
                </select>
            </div>
            <div id="labelContainer">
                <label for="label"> Choose a label:</label>
                <select name="label" id="label">
                    <c:forEach var="item" items="${label}">
                        <option value="${item}" ${item == payPalComponent.label ? 'selected="selected"' : ''}>${item}</option>
                    </c:forEach>
                </select>
            </div>
            <div id="layoutContainer">
                <label for="layout"> Choose a layout:</label>
                <select name="layout" id="layout">
                    <c:forEach var="item" items="${layout}">
                        <option value="${item}" ${item == payPalComponent.layout ? 'selected="selected"' : ''}>${item}</option>
                    </c:forEach>
                </select>
            </div>
            <input name="buttonConfig" value="${buttonConfig}" type="hidden"/>

            <div id="checkbox">
                <label>
                    <input id="payPalButtonTagline" name="tagline" type="checkbox" value="true" ${payPalComponent.tagline == true ? 'checked' : ''} />
                    Enable Tagline
                </label>
            </div>

            <br></br>
            <input type="submit" value="Save">
            <br></br>

        </form>
    </div>
</div>

</body>
</html>
