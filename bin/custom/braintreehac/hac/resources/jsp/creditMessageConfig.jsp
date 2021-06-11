<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>
<% pageContext.setAttribute("newLineChar", "\n"); %>

<html>
<head>
    <title>PayPal Credit Message Config</title>

</head>
<body>
<div class="marginLeft prepend-top span-17">

     <h2>${label}</h2>

<div class="dataTable no-footer">
    <form action="/hac/braintreehac/saveConfigs/" method="GET">
    <div id="layoutContainer">
    <label for="layout"> Choose a layout:</label>
        <select name="layout" id="layout" onchange="layoutChanged()">
            <c:forEach var="item" items="${layout}">
                <option value="${item}" ${item == creditMessage.layout ? 'selected="selected"' : ''}>${item.toLowerCase()}</option>
            </c:forEach>
        </select>
    </div>
    <div id="logoTypeContainer">
    <label for="logoType"> Choose a logo type:</label>
        <select name="logoType" id="logoType">
            <c:forEach var="item" items="${logoType}">
                <option value="${item}" ${item == creditMessage.logoType ? 'selected="selected"' : ''}>${item.toLowerCase()}</option>
            </c:forEach>
        </select>
    <br/>
    </div>
    <div id="logoPositionContainer">
    <label for="logoPosition"> Choose a logo position:</label>
        <select name="logoPosition" id="logoPosition">
            <c:forEach var="item" items="${logoPosition}">
                <option value="${item}" ${item == creditMessage.logoPosition ? 'selected="selected"' : ''}>${item.toLowerCase()}</option>
            </c:forEach>
        </select>
    </div>
    <div id="textColorContainer">
    <label for="textColor"> Choose a text color:</label>
        <select name="textColor" id="textColor">
            <c:forEach var="item" items="${textColor}">
                <option value="${item}" ${item == creditMessage.textColor ? 'selected="selected"' : ''}>${item.toLowerCase()}</option>
            </c:forEach>
        </select>
    <br/>
    </div>
    <div id="colorContainer">
    <label for="color"> Choose a color:</label>
        <select name="color" id="color">
            <c:forEach var="item" items="${color}">
                <option value="${item}" ${item == creditMessage.color ? 'selected="selected"' : ''}>${item}</option>
            </c:forEach>
        </select>
    </div>
    <div id="ratioContainer">
    <label for="ratio"> Choose a ratio:</label>
        <select name="ratio" id="ratio">
            <c:forEach var="item" items="${ratio}">
                <option value="${item}" ${item == creditMessage.ratio ? 'selected="selected"' : ''}>${item}</option>
            </c:forEach>
        </select>
    </div>
        <input name="uid" value="${creditMessage.uid}" type="hidden"/>
    <div id="checkbox">
        <label>
            <input id="hiddenEnabledCreditMessage" name="enabled" type="hidden"/>
            <input id="enabledCreditMessage" name="enabled" type="checkbox" value="true" ${creditMessage.enabled == true ? 'checked' : ''} />
            Enable Credit Message
        </label>
    </div>
    <br></br>
    <input type="submit" value="Save">
    <br></br>
    </form>
    </div>
    </div>

    <script>
        jQuery(document).ready(function ($) {
            layoutChanged();
        });

        $(document).on('submit', 'form', function() {
            if($("#enabledCreditMessage").is(':checked')){
                $("#hiddenEnabledCreditMessage").prop('disabled', true);
            } else {
                $("#hiddenEnabledCreditMessage").attr('value', 'false');
            }
        });

    function layoutChanged(){
    var layout = document.getElementById("layout").value;
    console.log(layout);
        if(layout === 'TEXT'){
            $("#logoTypeContainer").show();
            $("#logoPositionContainer").show();
            $("#textColorContainer").show();
            $("#colorContainer").hide();
            $("#ratioContainer").hide();
        } else if(layout === 'FLEX'){
            $("#logoTypeContainer").hide();
            $("#logoPositionContainer").hide();
            $("#textColorContainer").hide();
            $("#colorContainer").show();
            $("#ratioContainer").show();
        }
    }

    </script>
</body>
</html>
