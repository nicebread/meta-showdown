<%
String props = request.getParameter("p");
%>
<html>
<head>
<script language="JavaScript" type="text/javascript" src="/implicit/common/en-us/js/task.js"></script>
<script type="text/javascript" src="/implicit/common/en-us/js/swfobject.js"></script>
<style type="text/css">
	table { border: 5px solid #FFFFFF;}
	.text { font-family: verdana, arial, helvetica, sans-serif; font-size: 70%; color: #FFFFFF; }
</style>
<META http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Implicit Association Test</title>
<META HTTP-EQUIV="Expires" CONTENT="0">
<META HTTP-EQUIV="Pragma" CONTENT="no-cache">
<META HTTP-EQUIV="Cache-Control" CONTENT="no-cache">
</head>
<body bgcolor="0f0f0f">
<center>
<table>
<tr><td>
<div id="flashcontent">
Please install the <a href="http://www.macromedia.com/go/getflash">Flash Player Plugin</a>, then return to the site.
</div>
</tr></td>
</table>
<script type="text/javascript">
	var fo = new SWFObject("/implicit/common/en-us/flash/iat5.swf", "flashIAT", "500", "400", "6", "#000000");
	fo.addParam("quality", "high");
	fo.addVariable("i", '<%= request.getParameter("i") %>');
	fo.addVariable("p", '<%= props %>');
	fo.addVariable("tid", xGetCookie("tid"));
	fo.addVariable("jid", '<%= session.getId() %>');
	fo.write("flashcontent");
</script>
<p>&nbsp;</p>
<p class="text"><b>If the E and I keys do not work, click the mouse inside the white box and try again.<br /><br />
If the red <font color=red>X</font> appears, press the other key to make the red <font color=red>X</font> go away.</b></p>
</center>
</body>
</html>
