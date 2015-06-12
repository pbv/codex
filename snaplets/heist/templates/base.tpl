<html>
  <head>
    <title>Pythondo server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
    <script type="text/javascript"
	    src="/mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
    <script type="text/javascript">
function countdown(secs) {
    var timeout = new Date();
    timeout.setTime(timeout.getTime() + secs*1000);

    setInterval(function () {
        var now = new Date();
        var t = Math.floor((timeout - now)/1000);
        if (t>0) {
        s = t % 60;
        t = Math.floor(t/60);  
        m = t % 60;
        t = Math.floor(t/60);
        h = t % 24;
        d = Math.floor(t/24)
        time = (d>0 ? d + "d " : "") + 
               (h>0 ? h + "h " : "") +
               (m>0 ? m + "m " : "") +
               (s + "s");
       document.getElementById("js-timer").innerHTML = time;
       } else {
       document.getElementById("js-timer").innerHTML = "N/A";
       }
 }, 1000);
}    
    </script>
  </head>
  <body>
     <div id="topbar">
       <p><em><timeNow/></em><br/>
	 <ifLoggedIn> 
	   <strong>Login:</strong> <code><loggedInUser/></code> (<em><loggedInName/></em>);
	   carregue aqui para <a href="/asklogout">terminar a sessão</a>.
	 </ifLoggedIn> 
	 </p>
     </div>
    <div id="content">
      <apply-content/>
    </div>
    <div id="footer">
      <p class="info">Pythondo <version/> &copy; 2014 Pedro Vasconcelos.<br>
	Departamento de Ciência de Computadores, Faculdade de Ciências,
	Universidade do Porto.<br>
    Construido usando
    <a href="http://snapframework.com/">Snap web framework</a>,
    <a href="http://johnmacfarlane.net/pandoc/">Pandoc</a>,
    <a href="http://ace.ajax.org">ACE code editor</a>  e
    <a href="http://www.mathjax.org">MathJax</a>.</p>
    </div>
    <script src="/ace-builds/src-min-noconflict/ace.js" type="text/javascript" charset="utf-8"></script>
    <script type="text/javascript">
      var editor = ace.edit("editor");
      editor.setFontSize(16);
      editor.getSession().setMode("ace/mode/python");
    </script>
  </body>
</html>
